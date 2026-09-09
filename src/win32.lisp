(in-package #:ls-flock)

;;;; -- Win32 File Locks --

;;; The operating-system half of ls-flock on Windows: file handles from
;;; CreateFileW and whole-file byte-range locks from LockFileEx. Locks are
;;; released when the handle closes, including when the process dies, which
;;; matches the lockf(3) lifetime the POSIX half relies on.

(sb-alien:define-alien-type flock--wide-string
    (sb-alien:c-string :external-format :ucs-2le))

(sb-alien:define-alien-routine ("GetLastError" flock--get-last-error)
    (sb-alien:unsigned 32))

(sb-alien:define-alien-routine ("CloseHandle" flock--close-handle)
    sb-alien:int
  (handle (sb-alien:signed 64)))

(sb-alien:define-alien-routine ("CreateFileW" flock--create-file)
    (sb-alien:signed 64)
  (name flock--wide-string)
  (access (sb-alien:unsigned 32))
  (share (sb-alien:unsigned 32))
  (security (* t))
  (disposition (sb-alien:unsigned 32))
  (flags (sb-alien:unsigned 32))
  (template (sb-alien:signed 64)))

(sb-alien:define-alien-routine ("LockFileEx" flock--lock-file-ex)
    sb-alien:int
  (handle (sb-alien:signed 64))
  (flags (sb-alien:unsigned 32))
  (reserved (sb-alien:unsigned 32))
  (low (sb-alien:unsigned 32))
  (high (sb-alien:unsigned 32))
  (overlapped (* t)))

(sb-alien:define-alien-routine ("UnlockFileEx" flock--unlock-file-ex)
    sb-alien:int
  (handle (sb-alien:signed 64))
  (reserved (sb-alien:unsigned 32))
  (low (sb-alien:unsigned 32))
  (high (sb-alien:unsigned 32))
  (overlapped (* t)))

(defparameter *flock-invalid-handle* -1
  "The handle value CreateFileW returns on failure.")

(defparameter *flock-generic-read-write* #xC0000000
  "GENERIC_READ and GENERIC_WRITE access to the lock file.")

(defparameter *flock-share-read-write* 7
  "FILE_SHARE_READ, FILE_SHARE_WRITE, and FILE_SHARE_DELETE, so other holders
can open the lock file and its directory can be deleted while it is held.")

(defparameter *flock-open-always* 4
  "The CreateFileW disposition creating the lock file when it is missing.")

(defparameter *flock-attribute-normal* #x80
  "FILE_ATTRIBUTE_NORMAL for a newly created lock file.")

(defparameter *flock-exclusive-lock* 2
  "LOCKFILE_EXCLUSIVE_LOCK.")

(defparameter *flock-fail-immediately* 1
  "LOCKFILE_FAIL_IMMEDIATELY, turning a held lock into an error.")

(defparameter *flock-error-lock-violation* 33
  "ERROR_LOCK_VIOLATION, reported when another holder owns the byte range.")

(defun flock--call-with-overlapped (function)
  "Call FUNCTION with a zeroed OVERLAPPED structure locking byte zero."
  (sb-alien:with-alien ((overlapped (sb-alien:array (sb-alien:unsigned 8) 32)))
    (dotimes (index 32)
      (setf (sb-alien:deref overlapped index) 0))
    (funcall function (sb-alien:alien-sap overlapped))))

(defun flock--open (pathname mode)
  "Return an open handle for lock file PATHNAME, creating it when missing.

MODE is accepted for symmetry with the POSIX half; Windows has no
permission bits to apply on creation."
  (declare (ignore mode))
  (handler-case
      (progn
        (ensure-directories-exist pathname)
        (let ((handle (flock--create-file (uiop:native-namestring pathname)
                                          *flock-generic-read-write*
                                          *flock-share-read-write*
                                          nil
                                          *flock-open-always*
                                          *flock-attribute-normal*
                                          0)))
          (when (= handle *flock-invalid-handle*)
            (flock--fail pathname "Could not open the lock file: Windows error ~D"
                         (flock--get-last-error)))
          handle))
    (file-lock-error (condition)
      (error condition))
    (error (cause)
      (flock--fail pathname "Could not open the lock file: ~A" cause))))

(defun flock--lock (descriptor pathname)
  "Wait until DESCRIPTOR holds PATHNAME's exclusive lock."
  (flock--call-with-overlapped
   (lambda (overlapped)
     (when (zerop (flock--lock-file-ex descriptor *flock-exclusive-lock* 0 1 0
                                       overlapped))
       (flock--fail pathname "Could not acquire the lock: Windows error ~D"
                    (flock--get-last-error)))))
  nil)

(defun flock--try-lock (descriptor pathname)
  "Take PATHNAME's exclusive lock on DESCRIPTOR or signal FILE-LOCK-BUSY."
  (flock--call-with-overlapped
   (lambda (overlapped)
     (when (zerop (flock--lock-file-ex descriptor
                                       (logior *flock-exclusive-lock*
                                               *flock-fail-immediately*)
                                       0 1 0 overlapped))
       (let ((code (flock--get-last-error)))
         (if (= code *flock-error-lock-violation*)
             (flock--busy pathname)
             (flock--fail pathname "Could not acquire the lock: Windows error ~D"
                          code))))))
  nil)

(defun flock--unlock (descriptor)
  "Release the lock held through DESCRIPTOR, ignoring failures."
  (ignore-errors
    (flock--call-with-overlapped
     (lambda (overlapped)
       (flock--unlock-file-ex descriptor 0 1 0 overlapped))))
  nil)

(defun flock--close (descriptor)
  "Close DESCRIPTOR, ignoring failures."
  (ignore-errors
    (flock--close-handle descriptor))
  nil)
