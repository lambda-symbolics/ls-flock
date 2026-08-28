(in-package #:ls-flock)

;;;; -- Conditions --

(define-condition file-lock-error (error)
  ((message
    :initarg :message
    :reader file-lock-error-message
    :type string
    :documentation "The human-readable description of the failure.")
   (pathname
    :initarg :pathname
    :reader file-lock-error-pathname
    :type pathname
    :documentation "The lock file involved in the failure."))
  (:report
   (lambda (condition stream)
     (format stream "~A (~A)"
             (file-lock-error-message condition)
             (file-lock-error-pathname condition))))
  (:documentation "An advisory file lock could not be opened or acquired."))

(define-condition file-lock-busy (file-lock-error)
  ()
  (:documentation "Another holder owns the advisory lock right now."))

(defun flock--busy (pathname)
  "Signal FILE-LOCK-BUSY for PATHNAME."
  (error 'file-lock-busy
         :message "The advisory lock is held elsewhere."
         :pathname pathname))

(defun flock--fail (pathname message &rest arguments)
  "Signal FILE-LOCK-ERROR for PATHNAME described by MESSAGE."
  (error 'file-lock-error
         :message (apply #'format nil message arguments)
         :pathname pathname))


;;;; -- In-Process Serialization --

;;; POSIX record locks never conflict within the owning process, so every
;;; lock file is paired with one process-wide mutex. Without it, two threads
;;; of one image would both pass the operating-system lock.

(defvar *flock-table-lock*
  (bordeaux-threads:make-lock "ls-flock tables")
  "Serialize the in-process mutex and held-lease registries.")

(defvar *flock-in-process-locks* (make-hash-table :test #'equal)
  "Map lock file namestrings to their process-wide mutexes.")

(defun flock--in-process-lock (pathname)
  "Return the process-wide mutex serializing PATHNAME's file lock."
  (bordeaux-threads:with-lock-held (*flock-table-lock*)
    (let ((key (namestring pathname)))
      (or (gethash key *flock-in-process-locks*)
          (setf (gethash key *flock-in-process-locks*)
                (bordeaux-threads:make-lock key))))))

(defun flock--open (pathname mode)
  "Return an open descriptor for lock file PATHNAME, creating it with MODE."
  (handler-case
      (progn
        (ensure-directories-exist pathname)
        (osicat-posix:open (namestring pathname)
                           (logior osicat-posix:o-creat
                                   osicat-posix:o-rdwr)
                           mode))
    (error (cause)
      (flock--fail pathname "Could not open the lock file: ~A" cause))))


;;;; -- Scoped Locks --

(defun call-with-file-lock (pathname function &key (mode #o600))
  "Call FUNCTION while exclusively holding PATHNAME's advisory lock.

Waits for other processes and serializes this process's own threads
through a per-pathname mutex. The empty lock file is created with MODE
when missing and intentionally left behind on release, so later holders
can reuse it. Acquisition failures signal FILE-LOCK-ERROR; conditions
from FUNCTION propagate unchanged."
  (bordeaux-threads:with-lock-held ((flock--in-process-lock pathname))
    (let ((descriptor (flock--open pathname mode)))
      (unwind-protect
           (progn
             (handler-case
                 (osicat-posix:lockf descriptor osicat-posix:f-lock 0)
               (error (cause)
                 (flock--fail pathname "Could not acquire the lock: ~A"
                              cause)))
             (funcall function))
        (ignore-errors
          (osicat-posix:lockf descriptor osicat-posix:f-ulock 0))
        (ignore-errors
          (osicat-posix:close descriptor))))))

(defmacro with-file-lock ((pathname &key (mode #o600)) &body body)
  "Evaluate BODY while exclusively holding PATHNAME's advisory lock."
  `(call-with-file-lock ,pathname (lambda () ,@body) :mode ,mode))


;;;; -- Exclusive Leases --

(defclass lease ()
  ((pathname
    :initarg :pathname
    :reader lease-pathname
    :type pathname
    :documentation "The persistent file carrying the process-shared lock.")
   (descriptor
    :initarg :descriptor
    :accessor lease--descriptor
    :type (or null integer)
    :documentation "The open descriptor holding the lock, or NIL after release."))
  (:documentation "A process-lifetime exclusive lease on one lock file."))

(defvar *flock-held-leases* (make-hash-table :test #'equal)
  "Map lock file namestrings to leases held by this process.")

(defun lease-held-p (lease)
  "Return true when LEASE still owns an open lock descriptor."
  (not (null (lease--descriptor lease))))

(defun lease-acquire (pathname &key (mode #o600))
  "Acquire a process-lifetime exclusive lease on PATHNAME without waiting.

Signals FILE-LOCK-BUSY when this process already holds a live lease on
PATHNAME or another process owns the lock. The empty lock file may
remain after release or a crash; a later acquisition reuses it as soon
as the former owner has released it or exited."
  (bordeaux-threads:with-lock-held (*flock-table-lock*)
    (let* ((key (namestring pathname))
           (existing (gethash key *flock-held-leases*)))
      ;; The registry check and the operating-system lock must fall under
      ;; one mutex: POSIX record locks never conflict within the owning
      ;; process, so two racing threads would otherwise both acquire.
      (when (and existing (lease-held-p existing))
        (flock--busy pathname))
      (when existing
        (remhash key *flock-held-leases*))
      (let ((descriptor (flock--open pathname mode))
            (acquired-p nil))
        (unwind-protect
             (progn
               (handler-case
                   (osicat-posix:lockf descriptor osicat-posix:f-tlock 0)
                 ;; POSIX allows either EACCES or EAGAIN for a held lock,
                 ;; and Linux reports errno 11 under its EWOULDBLOCK name.
                 ((or osicat-posix:eacces
                      osicat-posix:eagain
                      osicat-posix:ewouldblock) ()
                   (flock--busy pathname))
                 (file-lock-error (condition)
                   (error condition))
                 (error (cause)
                   (flock--fail pathname "Could not acquire the lock: ~A"
                                cause)))
               (let ((lease (make-instance 'lease
                                           :pathname pathname
                                           :descriptor descriptor)))
                 (setf (gethash key *flock-held-leases*) lease
                       acquired-p t)
                 lease))
          (unless acquired-p
            (ignore-errors (osicat-posix:close descriptor))))))))

(defun reset-after-fork ()
  "Drop lock state a fork child inherited from its parent.

POSIX record locks are not inherited across fork: the child owns none
of its parent's locks, while the copied tables claim otherwise. This
closes the child's copies of held lease descriptors, marks those
leases released, and clears both in-process tables. Call it in a fork
child before using ls-flock, never in a process whose own leases are
still live."
  (bordeaux-threads:with-lock-held (*flock-table-lock*)
    (maphash (lambda (key lease)
               (declare (ignore key))
               (let ((descriptor (lease--descriptor lease)))
                 (when descriptor
                   (setf (lease--descriptor lease) nil)
                   (ignore-errors (osicat-posix:close descriptor)))))
             *flock-held-leases*)
    (clrhash *flock-held-leases*)
    (clrhash *flock-in-process-locks*))
  nil)

(defun lease-release (lease)
  "Release LEASE idempotently.

Closing the descriptor is the final authority even when an explicit
unlock reports an operating-system failure."
  (bordeaux-threads:with-lock-held (*flock-table-lock*)
    (let ((descriptor (lease--descriptor lease))
          (key (namestring (lease-pathname lease))))
      (when descriptor
        (when (eq (gethash key *flock-held-leases*) lease)
          (remhash key *flock-held-leases*))
        (setf (lease--descriptor lease) nil)
        (ignore-errors
          (osicat-posix:lockf descriptor osicat-posix:f-ulock 0))
        (ignore-errors
          (osicat-posix:close descriptor)))))
  nil)
