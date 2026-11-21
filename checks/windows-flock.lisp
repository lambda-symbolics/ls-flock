;;; Native Windows gate using the distribution declared by the owning qlfile.

(require :asdf)
(assert (and (member :win32 *features*) (member :x86-64 *features*)))
(assert (string= "2.6.6" (lisp-implementation-version)))
(format t "Native gate: ~A ~A, ~A ~A~%"
        (lisp-implementation-type) (lisp-implementation-version)
        (software-type) (software-version))
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

(let ((releases
        (with-open-file (stream "qlfile")
          (loop for line = (read-line stream nil)
                while line
                for fields = (remove "" (uiop:split-string line
                                                         :separator '(#\Space #\Tab #\Return))
                                     :test #'string=)
                when (equal (first fields) "ql") collect (third fields)))))
  (assert releases)
  (assert (every (lambda (release) (equal release (first releases))) releases))
  (ql-dist:install-dist
   (format nil "http://beta.quicklisp.org/dist/quicklisp/~A/distinfo.txt" (first releases))
   :replace t :prompt nil))

(let ((root (uiop:getcwd)))
  (asdf:load-asd (merge-pathnames "ls-flock.asd" root))
  (assert (equal root (asdf:system-source-directory "ls-flock")))
  (ql:quickload "ls-flock/tests")
  (assert (equal root (asdf:system-source-directory "ls-flock")))
  (assert (equal root (asdf:system-source-directory "ls-flock/tests")))
  (assert (plusp (uiop:symbol-call :ls-flock :flock--process-id)))
  (asdf:test-system "ls-flock"))
