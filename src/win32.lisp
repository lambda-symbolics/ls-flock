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

(sb-alien:define-alien-routine ("GetCurrentProcessId" flock--process-id)
    (sb-alien:unsigned 32))

(sb-alien:define-alien-routine ("GetFileInformationByHandle" flock--file-information)
    sb-alien:int
  (handle (sb-alien:signed 64))
  (information (* t)))

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

(defun flock--descriptor-key (descriptor)
  "Return the volume serial and file index from the locking handle."
  (sb-alien:with-alien ((information (sb-alien:array (sb-alien:unsigned 32) 13)))
    (when (zerop (flock--file-information descriptor (sb-alien:alien-sap information)))
      (error "GetFileInformationByHandle failed: Windows error ~D" (flock--get-last-error)))
    (cons (sb-alien:deref information 7)
          (logior (ash (sb-alien:deref information 11) 32)
                  (sb-alien:deref information 12)))))

(defun flock--open-handle (pathname access existing-only-p)
  "Open a file handle, distinguishing an absent existing-only file from failure."
  (let ((handle (flock--create-file (uiop:native-namestring pathname)
                                    access *flock-share-read-write* nil
                                    (if existing-only-p 3 *flock-open-always*)
                                    *flock-attribute-normal* 0)))
    (if (/= handle *flock-invalid-handle*)
        handle
        (let ((code (flock--get-last-error)))
          (unless (and existing-only-p (member code '(2 3)))
            (flock--fail pathname "Could not open the lock file: Windows error ~D" code))))))

(defun flock--path-key (pathname)
  "Observe PATHNAME through a metadata-only handle, if it exists."
  (let ((handle (flock--open-handle pathname 0 t)))
    (when handle
      (unwind-protect
           (handler-case (flock--descriptor-key handle)
             (error (cause)
               (flock--fail pathname "Could not identify the lock file: ~A" cause)))
        (flock--close handle)))))

(defun flock--open (pathname mode existing-only-p)
  "Open PATHNAME for locking, returning NIL for an absent existing-only file."
  (declare (ignore mode))
  (handler-case
      (progn
        (unless existing-only-p (ensure-directories-exist pathname))
        (flock--open-handle pathname *flock-generic-read-write* existing-only-p))
    (file-lock-error (condition) (error condition))
    (error (cause)
      (flock--fail pathname "Could not open the lock file: ~A" cause))))

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
  "Release the lock held through DESCRIPTOR, ignoring failures.

UnlockFileEx reports failure through its return value, which this discards."
  (flock--call-with-overlapped
   (lambda (overlapped)
     (flock--unlock-file-ex descriptor 0 1 0 overlapped)))
  nil)

(defun flock--close (descriptor)
  "Close DESCRIPTOR, ignoring failures.

CloseHandle reports failure through its return value, which this discards."
  (flock--close-handle descriptor)
  nil)
