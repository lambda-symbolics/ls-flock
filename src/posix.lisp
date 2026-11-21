(in-package #:ls-flock)

;;;; -- POSIX Record Locks --

;;; The operating-system half of ls-flock on POSIX hosts: descriptors from
;;; open(2) and whole-file record locks from lockf(3). Windows hosts load
;;; win32.lisp, which provides the corresponding operations over handles.

(defun flock--process-id ()
  "Return the PID owning this process's lock registry."
  (sb-posix:getpid))

(defun flock--stat-key (stat)
  "Return the device/inode identity of STAT."
  (cons (sb-posix:stat-dev stat) (sb-posix:stat-ino stat)))

(defun flock--path-key (pathname)
  "Observe PATHNAME without opening or closing a descriptor."
  (handler-case
      (flock--stat-key (sb-posix:stat (uiop:native-namestring pathname)))
    (sb-posix:syscall-error (cause)
      (unless (= sb-posix:enoent (sb-posix:syscall-errno cause))
        (flock--fail pathname "Could not identify the lock file: ~A" cause)))))

(defun flock--descriptor-key (descriptor)
  "Identify the object opened as DESCRIPTOR."
  (flock--stat-key (sb-posix:fstat descriptor)))

(defun flock--open (pathname mode existing-only-p)
  "Open PATHNAME, returning NIL when an existing-only lookup finds no file."
  (handler-case
      (progn
        (unless existing-only-p (ensure-directories-exist pathname))
        (sb-posix:open (uiop:native-namestring pathname)
                       (if existing-only-p sb-posix:o-rdwr
                           (logior sb-posix:o-creat sb-posix:o-rdwr))
                       mode))
    (sb-posix:syscall-error (cause)
      (unless (and existing-only-p (= sb-posix:enoent (sb-posix:syscall-errno cause)))
        (flock--fail pathname "Could not open the lock file: ~A" cause)))
    (error (cause)
      (flock--fail pathname "Could not open the lock file: ~A" cause))))

(defun flock--foreign-owner (descriptor)
  "Return a positive foreign PID observed by F_GETLK on DESCRIPTOR, if supported."
  (handler-case
      (destructuring-bind (class fcntl type-reader pid-reader get-lock write-lock read-lock seek-set)
          (mapcar (lambda (name) (find-symbol name :sb-posix))
                  '("FLOCK" "FCNTL" "FLOCK-TYPE" "FLOCK-PID" "F-GETLK"
                    "F-WRLCK" "F-RDLCK" "SEEK-SET"))
        (let ((lock (make-instance class :type (symbol-value write-lock)
                                   :whence (symbol-value seek-set) :start 0 :len 0)))
          (funcall fcntl descriptor (symbol-value get-lock) lock)
          (let ((pid (funcall pid-reader lock)))
            (when (and (member (funcall type-reader lock)
                               (list (symbol-value write-lock) (symbol-value read-lock)))
                       (typep pid '(integer 1)) (/= pid (flock--process-id)))
              pid))))
    (sb-sys:deadline-timeout () nil)
    (error () nil)))

(defun flock--try-lock (descriptor pathname)
  "Take PATHNAME's exclusive lock on DESCRIPTOR or signal FILE-LOCK-BUSY."
  (handler-case
      (sb-posix:lockf descriptor sb-posix:f-tlock 0)
    ;; POSIX allows either EACCES or EAGAIN for a held lock,
    ;; and Linux reports errno 11 under its EWOULDBLOCK name.
    (sb-posix:syscall-error (condition)
      (if (member (sb-posix:syscall-errno condition)
                  (list sb-posix:eacces sb-posix:eagain)
                  :test #'=)
          (flock--busy pathname (flock--foreign-owner descriptor))
          (flock--fail pathname "Could not acquire the lock: ~A" condition)))
    (file-lock-error (condition)
      (error condition))
    (error (cause)
      (flock--fail pathname "Could not acquire the lock: ~A" cause)))
  nil)

(defun flock--unlock (descriptor)
  "Release the lock held through DESCRIPTOR, ignoring failures."
  (handler-case
      (sb-posix:lockf descriptor sb-posix:f-ulock 0)
    (sb-posix:syscall-error ()
      nil))
  nil)

(defun flock--close (descriptor)
  "Close DESCRIPTOR, ignoring failures."
  (handler-case
      (sb-posix:close descriptor)
    (sb-posix:syscall-error ()
      nil))
  nil)
