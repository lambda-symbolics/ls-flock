(defpackage #:ls-flock/tests
  (:use #:cl #:ls-flock)
  (:export #:run-tests))

(in-package #:ls-flock/tests)

(defvar *assertions* 0)

(defun check (value control &rest arguments)
  "Count one assertion and fail with CONTROL unless VALUE is true."
  (incf *assertions*)
  (unless value
    (error (apply #'format nil control arguments))))

(defmacro signals (condition-type &body body)
  "Return true when BODY signals CONDITION-TYPE."
  `(handler-case
       (progn ,@body nil)
     (,condition-type () t)))

(defun tests--scoped-locks (root)
  "Exercise scoped advisory locks beneath ROOT."
  (let ((pathname (merge-pathnames "scoped.lock" root)))
    (check (eql (with-file-lock (pathname) 42) 42)
           "a scoped lock did not return its body value")
    (check (probe-file pathname)
           "the lock file was not left behind for later holders")
    (check (= (logand (osicat-posix:stat-mode
                       (osicat-posix:stat (namestring pathname)))
                      #o777)
              #o600)
           "the lock file was not created private")
    (check (eql (call-with-file-lock pathname (lambda () 7)) 7)
           "a reused lock file did not serve a second holder")
    (check (signals simple-error
             (with-file-lock (pathname)
               (error "deliberate body failure")))
           "a body condition did not propagate unchanged")
    (check (eql (with-file-lock (pathname) 3) 3)
           "a signaling body did not release the lock"))
  (let ((pathname (merge-pathnames "missing/deeply/scoped.lock" root)))
    (check (eql (with-file-lock (pathname) 1) 1)
           "missing lock directories were not created"))
  nil)

(defun tests--thread-exclusion (root)
  "Prove two threads of one process exclude each other."
  (let ((pathname (merge-pathnames "threads.lock" root))
        (inside-p nil)
        (overlapped-p nil)
        (visits 0))
    (flet ((visit ()
             (with-file-lock (pathname)
               (when inside-p
                 (setf overlapped-p t))
               (setf inside-p t)
               (sleep 0.05)
               (incf visits)
               (setf inside-p nil))))
      (let ((threads (loop repeat 4
                           collect (bordeaux-threads:make-thread #'visit))))
        (mapc #'bordeaux-threads:join-thread threads)))
    (check (and (= visits 4) (not overlapped-p))
           "scoped locks did not serialize threads of one process"))
  nil)

(defun tests--leases (root)
  "Exercise process-lifetime exclusive leases beneath ROOT."
  (let* ((pathname (merge-pathnames "lease.lock" root))
         (lease (lease-acquire pathname)))
    (check (lease-held-p lease)
           "a fresh lease does not report itself held")
    (check (equal (namestring (lease-pathname lease)) (namestring pathname))
           "a lease does not name its lock file")
    (check (signals file-lock-busy (lease-acquire pathname))
           "a second in-process acquisition was not refused")
    (lease-release lease)
    (check (not (lease-held-p lease))
           "a released lease still reports itself held")
    (lease-release lease)
    (let ((replacement (lease-acquire pathname)))
      (check (lease-held-p replacement)
             "a released lock file could not be reacquired")
      (lease-release replacement)))
  nil)

(defun tests--cross-process (root)
  "Prove leases exclude other processes, not only other threads."
  (let* ((pathname (merge-pathnames "processes.lock" root))
         (program
           (format nil
                   "(let ((descriptor (sb-posix:open ~S ~
                                       (logior sb-posix:o-creat ~
                                               sb-posix:o-rdwr) ~
                                       #o600))) ~
                      (handler-case ~
                          (progn ~
                            (sb-posix:lockf descriptor sb-posix:f-tlock 0) ~
                            (sb-posix:exit 0)) ~
                        (error () (sb-posix:exit 7))))"
                   (namestring pathname)))
         (lease (lease-acquire pathname)))
    (flet ((probe-exit-code ()
             (nth-value 2
                        (uiop:run-program
                         (list "sbcl" "--noinform" "--non-interactive"
                               "--no-sysinit" "--no-userinit"
                               "--eval" "(require :sb-posix)"
                               "--eval" program)
                         :ignore-error-status t))))
      (unwind-protect
           (check (= (probe-exit-code) 7)
                  "another process acquired a held lease's lock")
        (lease-release lease))
      (check (= (probe-exit-code) 0)
             "another process could not lock a released lease file")))
  (let* ((pathname (merge-pathnames "foreign-holder.lock" root))
         (ready (merge-pathnames "foreign-holder.ready" root))
         (holder
           (format nil
                   "(let ((descriptor (sb-posix:open ~S ~
                                       (logior sb-posix:o-creat ~
                                               sb-posix:o-rdwr) ~
                                       #o600))) ~
                      (sb-posix:lockf descriptor sb-posix:f-lock 0) ~
                      (with-open-file (stream ~S :direction :output ~
                                              :if-does-not-exist :create) ~
                        (princ :ready stream)) ~
                      (sleep 5) ~
                      (sb-posix:exit 0))"
                   (namestring pathname)
                   (namestring ready)))
         (process
           (uiop:launch-program
            (list "sbcl" "--noinform" "--non-interactive"
                  "--no-sysinit" "--no-userinit"
                  "--eval" "(require :sb-posix)"
                  "--eval" holder))))
    (unwind-protect
         (progn
           (loop until (probe-file ready)
                 do (sleep 0.05))
           (check (signals file-lock-busy (lease-acquire pathname))
                  "a lock held by another process was not reported busy"))
      (uiop:terminate-process process :urgent t)
      (ignore-errors (uiop:wait-process process))))
  nil)

(defun run-tests ()
  "Run every ls-flock regression test."
  (setf *assertions* 0)
  (let ((root
          (uiop:ensure-directory-pathname
           (merge-pathnames
            (format nil "ls-flock-tests-~D-~D/"
                    (get-universal-time)
                    (random most-positive-fixnum))
            (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (tests--scoped-locks root)
           (tests--thread-exclusion root)
           (tests--leases root)
           #+sbcl (tests--cross-process root))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  (format t "~&~:D ls-flock tests passed.~%" *assertions*)
  nil)
