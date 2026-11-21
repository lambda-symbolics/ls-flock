(in-package #:ls-flock)

;;;; -- Conditions --

(define-condition file-lock-error (error)
  ((message
    :initarg       :message
    :reader        file-lock-error-message
    :type          string
    :documentation "The human-readable description of the failure.")
   (pathname
    :initarg       :pathname
    :reader        file-lock-error-pathname
    :type          pathname
    :documentation "The lock file involved in the failure."))
  (:report
   (lambda (condition stream)
     (format stream "~A (~A)"
             (file-lock-error-message condition)
             (file-lock-error-pathname condition))))
  (:documentation "An advisory file lock could not be opened or acquired."))

(define-condition file-lock-busy (file-lock-error)
  ((owner-pid :initarg :owner-pid :initform nil :reader file-lock-busy-owner-pid
              :documentation "Positive PID of an observed holder, or NIL if unknown.")
   (local-lease :initarg :local-lease :initform nil :reader flock--busy-local-lease
                :documentation "The observed local lease, or NIL for an external holder."))
  (:documentation "Another holder owns the advisory lock right now."))

(defun flock--busy (pathname &optional owner-pid local-lease)
  "Signal FILE-LOCK-BUSY with the available owner observation."
  (error 'file-lock-busy :message "The advisory lock is held elsewhere."
         :pathname pathname :owner-pid owner-pid :local-lease local-lease))

(defun flock--fail (pathname message &rest arguments)
  "Signal FILE-LOCK-ERROR for PATHNAME described by MESSAGE."
  (error 'file-lock-error
         :message  (apply #'format nil message arguments)
         :pathname pathname))


;;;; -- Process-Qualified Registry --

(defstruct (flock-state (:constructor flock--make-state (pid)))
  "Registry and descriptor bookkeeping owned by one process."
  pid
  (mutex (bordeaux-threads:make-lock "ls-flock registry"))
  (held (make-hash-table :test #'equal))
  uncertain)

(defvar *flock-state* nil
  "The process-qualified registry of all scoped and long-lived acquisitions.")

(defclass lease ()
  ((pathname :initarg :pathname :reader lease-pathname
             :documentation "The pathname supplied at acquisition.")
   (descriptor :initarg :descriptor :accessor lease--descriptor
               :documentation "The open native descriptor, or NIL after release.")
   (identity :initarg :identity :reader lease--identity
             :documentation "The native identity of the locked file.")
   (state :initarg :state :reader lease--state
          :documentation "The process registry that owns this lease.")
   (guard :initarg :guard :reader lease--guard
          :documentation "The pathname of the optional cleanup guard.")
   (thread :initarg :thread :reader lease--thread
           :documentation "The owner thread of a scoped lease, or NIL for an explicit lease.")
   (deferred :initform nil :accessor lease--deferred
             :documentation "Additional descriptors kept open until the native lock is released."))
  (:documentation "An exclusive native file lock."))

(defun reset-after-restart ()
  "Forget saved-core lock state without touching any descriptor numbers."
  (setf *flock-state* nil)
  nil)

#+sbcl
(pushnew 'reset-after-restart sb-ext:*init-hooks*)

(defun reset-after-fork ()
  "Close a fork child's inherited descriptors without touching inherited mutexes."
  (let ((old *flock-state*))
    (when (and old (/= (flock-state-pid old) (flock--process-id)))
      (maphash (lambda (key lease)
                 (declare (ignore key))
                 (flock--close (lease--descriptor lease))
                 (mapc #'flock--close (lease--deferred lease)))
               (flock-state-held old))
      (mapc #'flock--close (flock-state-uncertain old))
      (setf *flock-state* nil)))
  nil)

(defun flock--current-state ()
  "Return the current registry, never locking a mutex inherited from another PID."
  (let ((pid (flock--process-id)))
    (when (and *flock-state* (/= pid (flock-state-pid *flock-state*)))
      (reset-after-fork))
    (or *flock-state*
        (let ((new (flock--make-state pid)))
          #+sbcl (or (sb-ext:compare-and-swap (symbol-value '*flock-state*) nil new) new)
          #-sbcl (setf *flock-state* new)))))

(defun lease-held-p (lease)
  "Return true only for an active lease in the current process and image."
  (let ((state (lease--state lease)))
    (and (eq state *flock-state*) (= (flock-state-pid state) (flock--process-id))
         (not (null (lease--descriptor lease))))))

(defun flock--try-acquire (pathname mode &key existing-only-p guard scoped-p)
  "Try a native acquisition while serializing identity and descriptor operations."
  (let ((state (flock--current-state)))
    (bordeaux-threads:with-lock-held ((flock-state-mutex state))
      (let* ((held  (flock-state-held state))
             (local (gethash (flock--path-key pathname) held)))
        (when local (flock--busy pathname (flock-state-pid state) local))
        (let ((descriptor (flock--open pathname mode existing-only-p)))
          (when descriptor
            (unwind-protect
                 (let ((identity
                         (handler-case (flock--descriptor-key descriptor)
                           (error (cause)
                             ;; An unidentified descriptor might alias any live local lock.
                             (when (plusp (hash-table-count held))
                               (push descriptor (flock-state-uncertain state))
                               (setf descriptor nil))
                             (flock--fail pathname "Could not identify the open lock file: ~A" cause)))))
                   (let ((local (gethash identity held)))
                     (when local
                       ;; PATHNAME may have changed between stat and open. Closing this
                       ;; descriptor now would drop the POSIX record lock on the inode.
                       (push descriptor (lease--deferred local))
                       (setf descriptor nil)
                       (flock--busy pathname (flock-state-pid state) local)))
                   (let ((lease (make-instance 'lease :pathname pathname :descriptor descriptor
                                                :identity identity :state state :guard guard
                                                :thread (and scoped-p (bordeaux-threads:current-thread)))))
                     (flock--try-lock descriptor pathname)
                     (setf (gethash identity held) lease descriptor nil)
                     lease))
              (when descriptor (flock--close descriptor)))))))))

(defun flock--release (lease &key unlink-p)
  "Release LEASE and optionally remove its still-identical file under the guard."
  (when (lease-held-p lease)
    (let ((state (lease--state lease)))
      (bordeaux-threads:with-lock-held ((flock-state-mutex state))
        (let ((descriptor (lease--descriptor lease))
              (pathname   (lease-pathname lease))
              (identity   (lease--identity lease))
              (held       (flock-state-held state)))
          (when (and descriptor (eq lease (gethash identity held)))
            (let ((remove-p (and unlink-p
                                 (handler-case (equal identity (flock--path-key pathname))
                                   (file-lock-error () nil)))))
              (setf (lease--descriptor lease) nil)
              (remhash identity held)
              (flock--unlock descriptor)
              (flock--close descriptor)
              (mapc #'flock--close (lease--deferred lease))
              (setf (lease--deferred lease) nil)
              (when (zerop (hash-table-count held))
                (mapc #'flock--close (flock-state-uncertain state))
                (setf (flock-state-uncertain state) nil))
              (when remove-p
                (handler-case (delete-file pathname)
                  (file-error () nil)))))))))
  nil)


;;;; -- Scoped Locks --

(defun flock--wait-acquire (pathname mode already-held)
  "Wait for a native lock, or recognize an alias already in this ordered group."
  (loop
    (handler-case
        (return (values (flock--try-acquire pathname mode :scoped-p t) t))
      (file-lock-busy (condition)
        (let ((local (flock--busy-local-lease condition)))
          (cond
            ((and local (member local already-held))
             (return (values local nil)))
            ((and local (eq (lease--thread local) (bordeaux-threads:current-thread)))
             (error condition))))))
    (sleep 0.02)))

(defun call-with-file-lock (pathname function &key (mode #o600))
  "Call FUNCTION under PATHNAME's exclusive lock. Keep the file on release."
  (let ((lease (flock--wait-acquire (pathname pathname) mode nil)))
    (unwind-protect (funcall function)
      (flock--release lease))))

(defmacro with-file-lock ((pathname &key (mode #o600)) &body body)
  "Evaluate BODY while exclusively holding PATHNAME's advisory lock."
  `(call-with-file-lock ,pathname (lambda () ,@body) :mode ,mode))

(defun call-with-file-locks (pathnames function &key (mode #o600))
  "Acquire PATHNAMES in order, taking duplicate file identities once, then call FUNCTION."
  (let ((held nil))
    (unwind-protect
         (progn
           (dolist (pathname pathnames)
             (multiple-value-bind (lease new-p) (flock--wait-acquire (pathname pathname) mode held)
               (when new-p (push lease held))))
           (funcall function))
      (dolist (lease held) (flock--release lease)))))


;;;; -- Exclusive Leases --

(defun flock--check-guard (pathname guard)
  "Reject a guard which is also the file it protects."
  (when (and guard (equal (merge-pathnames pathname) (merge-pathnames guard)))
    (flock--fail pathname "A lock file cannot be its own lifecycle guard.")))

(defun lease-acquire (pathname &key (mode #o600) guard-pathname)
  "Try PATHNAME without waiting for its holder. With GUARD-PATHNAME, serialize
lifecycle changes under that persistent guard and remove the file on release.
All participants of a removable lease must use the same lifecycle guard."
  (let ((pathname (pathname pathname)))
    (flock--check-guard pathname guard-pathname)
    (flet ((acquire () (flock--try-acquire pathname mode :guard guard-pathname)))
      (if guard-pathname (call-with-file-lock guard-pathname #'acquire) (acquire)))))

(defun lease-release (lease)
  "Release idempotently. On failure to obtain the guard, release without unlinking."
  (when (lease-held-p lease)
    (let ((guard (lease--guard lease)))
      (if guard
          (unwind-protect
               (handler-case
                   (call-with-file-lock guard (lambda () (flock--release lease :unlink-p t)))
                 (file-lock-error () nil))
            (flock--release lease))
          (flock--release lease))))
  nil)

(defun file-lock-busy-p (pathname &key (mode #o600) guard-pathname (existing-only-p t))
  "Probe PATHNAME: return busy and optional owner PID, or NIL when free or absent.
With GUARD-PATHNAME, hold the guard through the whole probe and any file removal."
  (let ((pathname (pathname pathname)))
    (flock--check-guard pathname guard-pathname)
    (flet ((probe ()
             (handler-case
                 (let ((lease (flock--try-acquire pathname mode :existing-only-p existing-only-p)))
                   (when lease (flock--release lease :unlink-p (not (null guard-pathname))))
                   (values nil nil))
               (file-lock-busy (condition)
                 (values t (file-lock-busy-owner-pid condition))))))
      (if guard-pathname (call-with-file-lock guard-pathname #'probe) (probe)))))
