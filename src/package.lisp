(defpackage #:ls-flock
  (:use #:cl)
  (:export #:call-with-file-lock
           #:file-lock-busy
           #:file-lock-error
           #:file-lock-error-message
           #:file-lock-error-pathname
           #:lease
           #:lease-acquire
           #:lease-held-p
           #:lease-pathname
           #:lease-release
           #:with-file-lock))
