(in-package :cl-user)

#+sbcl (require :sb-posix)

(defun heroku-getenv (target)
  #+sbcl (sb-posix:getenv target))

(defun heroku-setenv ()
  #+sbcl (sb-posix:putenv (format nil "XDG_CACHE_HOME=~A" (concatenate 'string (heroku-getenv "CACHE_DIR") "/.asdf/"))))

(defun env-var-to-path (var)
  (make-pathname :defaults (format nil "~a/" (heroku-getenv var))))

(defvar *build-dir* (env-var-to-path "BUILD_DIR"))
(defvar *cache-dir* (pathname-directory (pathname (concatenate 'string (heroku-getenv "CACHE_DIR") "/"))))
(defvar *buildpack-dir* (pathname-directory (pathname (concatenate 'string (heroku-getenv "BUILDPACK_DIR") "/"))))
(defvar *cl-webserver* (read-from-string (heroku-getenv "CL_WEBSERVER")))

;;; Tell ASDF to store binaries in the cache dir
(heroku-setenv)

(require :asdf)

(let ((ql-setup (make-pathname :directory (append *cache-dir* '("quicklisp")) :defaults "setup.lisp")))
  (if (probe-file ql-setup)
      (load ql-setup)))

(ecase *cl-webserver*
  (hunchentoot (ql:quickload "hunchentoot")))

;;; App can redefine this to do runtime initializations
(defun initialize-application ())

;;; Default toplevel, app can redefine.
(defun heroku-toplevel ()
  (let ((port (parse-integer (heroku-getenv "PORT"))))
    (format t "Listening on port ~A~%" port)
    (ecase *cl-webserver*
      (hunchentoot (funcall (symbol-function (find-symbol "START" (find-package "HUNCHENTOOT")))
			    (funcall 'make-instance (find-symbol "EASY-ACCEPTOR" (find-package "HUNCHENTOOT")) :port port))))
    (loop (sleep 60))))

;;; This loads the application
(load (merge-pathnames "heroku-setup.lisp" *build-dir*))

(defun h-save-app (app-file)
  #+sbcl (sb-ext:save-lisp-and-die app-file
				   :toplevel #'heroku-toplevel
				   :executable t))

(let ((app-file (format nil "~A/lispapp" (heroku-getenv "BUILD_DIR")))) ;must match path specified in bin/release
  (format t "Saving to ~A~%" app-file)
  (h-save-app app-file))
