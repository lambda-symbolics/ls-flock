(in-package #:ls-flock)

;;;; -- POSIX Record Locks --

;;; The operating-system half of ls-flock on POSIX hosts: descriptors from
;;; open(2) and whole-file record locks from lockf(3). Windows hosts load
;;; win32.lisp, which provides the same five operations over handles.

(defun flock--open (pathname mode)
  "Return an open descriptor for lock file PATHNAME, creating it with MODE."
  (handler-case
      (progn
        (ensure-directories-exist pathname)
        (sb-posix:open (namestring pathname)
                       (logior sb-posix:o-creat
                               sb-posix:o-rdwr)
                           mode))
    (error (cause)
      (flock--fail pathname "Could not open the lock file: ~A" cause))))

(defun flock--lock (descriptor pathname)
  "Wait until DESCRIPTOR holds PATHNAME's exclusive lock."
  (handler-case
      (sb-posix:lockf descriptor sb-posix:f-lock 0)
    (error (cause)
      (flock--fail pathname "Could not acquire the lock: ~A" cause)))
  nil)

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
          (flock--busy pathname)
          (flock--fail pathname "Could not acquire the lock: ~A" condition)))
    (file-lock-error (condition)
      (error condition))
    (error (cause)
      (flock--fail pathname "Could not acquire the lock: ~A" cause)))
  nil)

(defun flock--unlock (descriptor)
  "Release the lock held through DESCRIPTOR, ignoring failures."
  (ignore-errors
    (sb-posix:lockf descriptor sb-posix:f-ulock 0))
  nil)

(defun flock--close (descriptor)
  "Close DESCRIPTOR, ignoring failures."
  (ignore-errors
    (sb-posix:close descriptor))
  nil)
