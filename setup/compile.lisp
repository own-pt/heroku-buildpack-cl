(in-package :cl-user)

(defun hgetenv (target)
  #+ccl (getenv target)
  #+sbcl (sb-posix:getenv target))

(defun hsetenv ()
  #+ccl (ccl:setenv "XDG_CACHE_HOME" (concatenate 'string (hgetenv "CACHE_DIR") "/.asdf/")))

(defvar *build-dir* (pathname-directory (pathname (concatenate 'string (hgetenv "BUILD_DIR") "/"))))
(defvar *cache-dir* (pathname-directory (pathname (concatenate 'string (hgetenv "CACHE_DIR") "/"))))
(defvar *buildpack-dir* (pathname-directory (pathname (concatenate 'string (hgetenv "BUILDPACK_DIR") "/"))))

;;; Tell ASDF to store binaries in the cache dir
(hsetenv)
;;(ccl:setenv "XDG_CACHE_HOME" (concatenate 'string (hgetenv "CACHE_DIR") "/.asdf/"))

(require :asdf)

(let ((ql-setup (make-pathname :directory (append *cache-dir* '("quicklisp")) :defaults "setup.lisp")))
  (if (probe-file ql-setup)
      (load ql-setup)
      (progn
	(load (make-pathname :directory (append *buildpack-dir* '("lib")) :defaults "quicklisp.lisp"))
	(funcall (symbol-function (find-symbol "INSTALL" (find-package "QUICKLISP-QUICKSTART")))
		 :path (make-pathname :directory (pathname-directory ql-setup))))))

;(asdf:clear-system "acl-compat")

;(load (make-pathname :directory (append *cache-dir* '("repos" "portableaserve" "acl-compat"))
;		     :defaults "acl-compat.asd"))
;(load (make-pathname :directory (append *cache-dir* '("repos" "portableaserve" "aserve"))
;		     :defaults "aserve.asd"))

(ql:quickload "hunchentoot")

(defun heroku-toplevel ()
  (let ((port (parse-integer (hgetenv "PORT"))))
    (format t "Listening on port ~A~%" port)
    (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port))))
;;; Default toplevel, app can redefine if necessary
;(defun heroku-toplevel ()
;  (let ((port (parse-integer (hgetenv "PORT"))))
;    (format t "Listening on port ~A~%" port)
;    (funcall (symbol-function (find-symbol "START" (find-package "NET.ASERVE")))
;	     :port port)
;    (loop (sleep 60))			;sleep forever
;    ))

;;; This loads the application
(load (make-pathname :directory *build-dir* :defaults "heroku-setup.lisp"))

(defun h-save-app (app-file)
  #+ccl (save-application app-file
        :prepend-kernel t
        :toplevel-function #'heroku-toplevel)
  #+sbcl (sb-ext:save-lisp-and-die app-file
        :toplevel #'heroku-toplevel
        :executable t))

(let ((app-file (format nil "~A/lispapp" (hgetenv "BUILD_DIR")))) ;must match path specified in bin/release
  (format t "Saving to ~A~%" app-file)
  (h-save-app app-file))
