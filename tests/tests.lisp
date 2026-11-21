(defpackage #:ls-flock/tests
  (:documentation "Regression tests for ls-flock.")
  (:use #:cl)
  (:import-from #:ls-flock
                #:call-with-file-lock
                #:file-lock-busy
                #:lease-acquire
                #:lease-held-p
                #:lease-pathname
                #:lease-release
                #:reset-after-fork
                #:with-file-lock)
  (:export #:run-tests))

(in-package #:ls-flock/tests)

(defvar *assertions* 0
  "The number of assertions the current test run has checked.")

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
    #-win32
    (check (= (logand (sb-posix:stat-mode
                       (sb-posix:stat (namestring pathname)))
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
  (let ((pathname     (merge-pathnames "threads.lock" root))
        (inside-p     nil)
        (overlapped-p nil)
        (visits       0))
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
         (lease    (lease-acquire pathname)))
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

#+(and sbcl (not win32))
(defun tests--fork-reset (root)
  "An actual fork discards child state without dropping the parent's kernel lock."
  (let* ((pathname (merge-pathnames "fork-reset.lock" root))
         (lease    (lease-acquire pathname))
         (pid      (bordeaux-threads:with-lock-held
                       ((ls-flock::flock-state-mutex ls-flock::*flock-state*))
                     (let ((child (sb-posix:fork)))
                       (when (zerop child)
                         ;; Reset before even unwinding the inherited mutex scope.
                         (sb-posix:exit
                          (handler-case
                              (progn
                                (reset-after-fork)
                                (if (and (not (lease-held-p lease))
                                         (ls-flock:file-lock-busy-p pathname))
                                    0 1))
                            (error () 2))))
                       child))))
    (unwind-protect
         (multiple-value-bind (reaped status)
             (sb-ext:with-timeout 5 (sb-posix:waitpid pid 0))
           (check (and (= reaped pid) (zerop status)) "the fork child did not observe its parent lock")
           (setf pid nil)
           (check (lease-held-p lease) "fork reset abandoned the parent's lease"))
      (when pid
        (ignore-errors (sb-posix:kill pid sb-posix:sigkill))
        (ignore-errors (sb-posix:waitpid pid 0)))
      (lease-release lease))))

#+(and sbcl (not win32))
(defun tests--restart-reset (root)
  "An image init hook must forget a stale descriptor number without closing it."
  (let ((ls-flock::*flock-state* nil))
    (let* ((pathname (merge-pathnames "restart-reset.lock" root))
           (lease (lease-acquire pathname))
           (descriptor (ls-flock::lease--descriptor lease)))
      (unwind-protect
           (progn
             (check (member 'ls-flock:reset-after-restart sb-ext:*init-hooks*)
                    "saved-image init hook was not installed")
             (ls-flock:reset-after-restart)
             (check (not (lease-held-p lease)) "restart left a saved lease live")
             (lease-release lease)
             (check (sb-posix:fstat descriptor) "restart closed a potentially reused descriptor"))
        (ls-flock::flock--close descriptor)))))

(defun tests--guarded-leases (root)
  "Check persistent probes, guarded unlink, stale releases, and fallback."
  (let ((pathname (merge-pathnames "guarded.lock" root))
        (guard    (merge-pathnames "guard.lock" root)))
    (check (not (ls-flock:file-lock-busy-p pathname)) "missing probe reported busy")
    (check (not (probe-file pathname)) "missing probe created a file")
    (let ((lease (lease-acquire pathname :guard-pathname guard)))
      (check (ls-flock:file-lock-busy-p pathname :guard-pathname guard) "guarded lease not busy")
      (lease-release lease)
      (check (not (probe-file pathname)) "guarded release did not remove the file")
      (let ((successor (lease-acquire pathname :guard-pathname guard)))
        (lease-release lease)
        (check (and (probe-file pathname) (lease-held-p successor)) "stale release touched successor")
        (call-with-file-lock guard (lambda () (lease-release successor)))
        (check (and (not (lease-held-p successor)) (probe-file pathname))
               "failed guard acquisition did not release without unlinking")))
    (check (not (ls-flock:file-lock-busy-p pathname :guard-pathname guard)) "stale file reported busy")
    (check (and (not (probe-file pathname)) (probe-file guard)) "guarded free probe did not clean up")))

#+(and sbcl (not win32))
(defun tests--aliases (root)
  "Resolve hard and symbolic aliases without closing a descriptor on the held inode."
  (let ((path (merge-pathnames "held.lock" root))
        (hard (merge-pathnames "hard.lock" root))
        (soft (merge-pathnames "soft.lock" root)))
    (call-with-file-lock
     path
     (lambda ()
       (sb-posix:link (namestring path) (namestring hard))
       (sb-posix:symlink (namestring path) (namestring soft))
       (dolist (alias (list hard soft))
         (multiple-value-bind (busy-p pid) (ls-flock:file-lock-busy-p alias)
           (check (and busy-p (= pid (sb-posix:getpid))) "alias did not report its local owner")))
       (let ((pid (sb-posix:fork)))
         (when (zerop pid)
           (sb-posix:exit (if (ls-flock:file-lock-busy-p hard) 0 3)))
         (multiple-value-bind (reaped status)
             (sb-ext:with-timeout 5 (sb-posix:waitpid pid 0))
           (check (and (= reaped pid) (zerop status)) "alias probe dropped the kernel lock")))))
    (check (equal '(one two)
                  (multiple-value-list
                   (ls-flock:call-with-file-locks (list path hard soft path)
                                                  (lambda () (values 'one 'two)))))
           "ordered aliases did not preserve body values")))

#+sbcl
(defun tests--cross-process (root)
  "Exercise a separate SBCL using ls-flock itself on POSIX and Windows."
  (let* ((pathname (merge-pathnames "foreign-holder.lock" root))
         (guard    (merge-pathnames "foreign-guard.lock" root))
         (ready    (merge-pathnames "foreign-holder.ready" root))
         (registry (mapcar #'asdf:system-source-directory '("ls-flock" "bordeaux-threads")))
         (process  (uiop:launch-program
                    (list "sbcl" "--noinform" "--non-interactive" "--eval" "(require :asdf)"
                          "--eval" (format nil "(setf asdf:*central-registry* '~S)" registry)
                          "--eval" (format nil "(asdf:initialize-output-translations '~S)"
                                           `(:output-translations (t ,root) :ignore-inherited-configuration))
                          "--eval" "(asdf:load-system :ls-flock)"
                          "--eval" (format nil "(progn (ls-flock:lease-acquire ~S :guard-pathname ~S) ~
                                                         (with-open-file (stream ~S :direction :output) ~
                                                           (princ :ready stream)) (sleep 30))"
                                           pathname guard ready))
                    :output nil :error-output :output)))
    (unwind-protect
         (progn
           (sb-ext:with-timeout 20
             (loop until (probe-file ready) do
               (unless (uiop:process-alive-p process) (error "Lock holder exited before readiness"))
               (sleep 0.05)))
           (multiple-value-bind (busy-p pid) (ls-flock:file-lock-busy-p pathname :guard-pathname guard)
             (check busy-p "foreign lock was not busy")
             #+win32 (check (null pid) "Windows reported an unobserved foreign PID")
             #-win32 (when pid
                       (check (= pid (uiop:process-info-pid process)) "wrong foreign owner PID"))))
      (when (uiop:process-alive-p process)
        (uiop:terminate-process process :urgent t))
      (sb-ext:with-timeout 5 (uiop:wait-process process)))
    (check (probe-file pathname) "crash fixture did not leave its lock file")
    (lease-release (lease-acquire pathname :guard-pathname guard))
    (check (not (probe-file pathname)) "could not reuse and clean up crashed holder's file")))

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
           (tests--guarded-leases root)
           #+(and sbcl (not win32)) (tests--fork-reset root)
            #+(and sbcl (not win32)) (tests--restart-reset root)
           #+(and sbcl (not win32)) (tests--aliases root)
           #+sbcl (tests--cross-process root))
      (uiop:delete-directory-tree root
                                  :validate          t
                                  :if-does-not-exist :ignore)))
  (format t "~&~:D ls-flock tests passed.~%" *assertions*)
  nil)
