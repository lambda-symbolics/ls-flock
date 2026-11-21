(defpackage #:ls-flock
  (:documentation "Process-shared advisory file locks and exclusive leases.")
  (:use #:cl)
  (:export #:call-with-file-lock
           #:call-with-file-locks
           #:file-lock-busy
           #:file-lock-busy-owner-pid
           #:file-lock-busy-p
           #:file-lock-error
           #:file-lock-error-message
           #:file-lock-error-pathname
           #:lease
           #:lease-acquire
           #:lease-held-p
           #:lease-pathname
           #:lease-release
           #:reset-after-fork
           #:reset-after-restart
           #:with-file-lock))
