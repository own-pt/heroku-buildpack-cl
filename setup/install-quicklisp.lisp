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
      (load ql-setup)
      (progn
	(load (make-pathname :directory (append *buildpack-dir* '("lib")) :defaults "quicklisp.lisp"))
	(funcall (symbol-function (find-symbol "INSTALL" (find-package "QUICKLISP-QUICKSTART")))
		 :path (make-pathname :directory (pathname-directory ql-setup))))))

