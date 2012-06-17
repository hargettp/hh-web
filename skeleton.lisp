;; Copyright (c) 2012 Phil Hargett

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(in-package :hh-web)

(defclass skeleton ()
  ((location :type (satisfies directory-pathname-p) 
	     :initarg :location 
	     :accessor in-location)
   (package :type (or symbol string) 
	    :initarg :package 
	    :accessor for-package))
  (:documentation "A skeleton represents the starting point for a project based on hh-web"))

(defgeneric skeleton-components (skeleton)
  (:documentation "Return the list of components of the skeleton, as a list of keywords")
  (:method ((skeleton skeleton))
    `(:directory
      :system 
     :package
     :taglibraries
     :templates
     :logs
     :urls
     :locales
     :server
     :quickrun)))

(defgeneric location-pathname (location)
  (:documentation "Convert the argument to a pathname")
  (:method ((location pathname))
    location)
  (:method ((location symbol))
    (location-pathname (string location)))
  (:method ((location string))
    (make-pathname :directory location)))

(defgeneric skeleton-location (skeleton)
  (:method ((skeleton skeleton))
    (merge-pathnames (make-pathname :directory `(:relative ,(string-downcase (string (for-package skeleton))))) 
		     (location-pathname (in-location skeleton)))))

(defmacro with-skeleton-file ((stream skeleton path) &rest body)
  "Write a string representation of content to the 
location specified path relative to the skeleton's base director"
  `(with-open-file (,stream (merge-pathnames ,path (skeleton-location ,skeleton)) 
			    :direction :output
			    :if-does-not-exist :create)
     ,@body))

(defun generate-skeleton-file (skeleton path content)
  (declare (type skeleton skeleton)
	   (type pathname path)
	   (type content list))
  (with-open-file (os (merge-pathnames path (skeleton-location skeleton)) 
		      :direction :output
		      :if-does-not-exist :create)
    (let ((*print-case* :downcase)
	  (*package* (find-package :asdf)))
      (loop for item in content
	 do (print item os)
	 do (pprint-newline :mandatory)
	 do (pprint-newline :mandatory)))))

(defgeneric generate-skeleton (skeleton)
  (:documentation "Generate all the files necessary for a project using hh-web")
  (:method ((skeleton skeleton))
    ;; Generate each component--creating directory should be first one
    (loop for component in (skeleton-components skeleton)
       do (generate-skeleton-component skeleton component))
    (skeleton-location skeleton)))

(defgeneric generate-skeleton-component (skeleton component)
  (:documentation "Generate the files necessary for the indicated component and skeleton")
  (:method ((skeleton skeleton) (component symbol)) 
    "If no such component exists for the skeleton, do nothing"
    component)

  (:method ((skeleton skeleton) (component (eql :directory)))
    (ensure-directories-exist (skeleton-location skeleton)))

  (:method ((skeleton skeleton) (component (eql :system)))
    (let* ((package-name (string-downcase (symbol-name (for-package skeleton))))
	   (system-file-pathname (pathname (format nil "~a.asd" package-name))))
      (with-skeleton-file (os skeleton system-file-pathname)
	(format os "(defpackage #:~a-asd~%" package-name)
	(format os "  (:use :cl :asdf))~%~%")
	(format os "(in-package :~a-asd)~%~%" package-name)
	(format os "(defsystem ~a~%" package-name)
	(format os "  :name \"~a\"~%" package-name)
	(format os "  :version \"0.01\"~%")
	(format os "  :serial t~%")
	(format os "  :components ((:file \"package\")~%")
	(format os "               (:file \"logs\")~%")
	(format os "               (:file \"templates\")~%")
	(format os "               (:file \"urls\")~%")
	(format os "               (:file \"server\"))~%")
	(format os "  :depends-on (#:log5~%")
	(format os "               #:hh-web-tags~%")
	(format os "               #:hh-web))~%"))))

  (:method ((skeleton skeleton) (component (eql :package)))
    (let* ((package-name (string-downcase (symbol-name (for-package skeleton)))))
      (with-skeleton-file (os skeleton #p"package.lisp")
	(format os "(defpackage #:~a-asd~%" package-name)
	(format os "  (:use :cl :asdf))~%~%")
	(format os "(defpackage #:~a~%" package-name)
	(format os "  (:nicknames :~a)~%" package-name)
	(format os "  (:use :cl~%")
	(format os "        :hh-web-tags~%")
	(format os "        :hh-web)~%")
	(format os "  (:export~%")
	(format os "    ;; Exports go here~%")
	(format os "    #:start-httpd-server")
	(format os "    #:stop-httpd-server")
	(format os "    ))~%")
	)))

  (:method ((skeleton skeleton) (component (eql :taglibraries)))
    )

  (:method ((skeleton skeleton) (component (eql :templates)))
    (let* ((package-name (string-downcase (symbol-name (for-package skeleton)))))
      (with-skeleton-file (os skeleton #p"templates.lisp")
	(format os "(in-package :~a)~%~%" package-name)
	(format os "(deftemplates :tag-library-packages (\"~a\")~%" package-name)
	(format os "  :template-packages (\"~a\")~%" package-name)
	(format os "  :templates (~%")
	(format os "              ;; General site templates~%")
	(format os "              (home-page \"home_page.lisp\")))~%"))
      (ensure-directories-exist (merge-pathnames #p"templates/" (skeleton-location skeleton)))
      (with-skeleton-file (os skeleton #p"templates/home_page.lisp")
	(format os "(+tag-library :html)~%~%")
	(format os "(page~%")
	(format os "  (+title \"~a home page\")~%" (string-capitalize package-name))
	(format os "  (div \"hello world!\"))~%"))))

  (:method ((skeleton skeleton) (component (eql :logs)))
    (let* ((package-name (string-downcase (symbol-name (for-package skeleton)))))
      (with-skeleton-file (os skeleton #p"logs.lisp")
	(format os "(in-package :~a)~%~%" package-name)
	(format os "(defvar *log-root* #p\"logs\")~%~%")
	(format os "(defun init-logging()~%")
	(format os "  (log5:start-sender 'http-access-log (rotating-log-sender ~%")
	(format os "  				       :interval (* 30 60) ;; new log every 30 minutes~%")
	(format os " 				       :max 5~%")
	(format os " 				       :location (merge-pathnames \"~a-access.log\" *log-root*))~%" package-name)
	(format os " 		     :category-spec `(http-access)~%")
	(format os " 		     :output-spec `(log5:message))~%")
	(format os "  (log5:start-sender 'http-messages-log (rotating-log-sender ~%")
	(format os "  				       :interval (* 30 60) ;; new log every 30 minutes~%")
	(format os " 				       :max 5~%")
	(format os " 				       :location (merge-pathnames \"~a-messages.log\" *log-root*))~%" package-name)
	(format os " 		     :category-spec `(http-message)~%")
	(format os " 		     :output-spec `(log5:message)))~%")
	)))

  (:method ((skeleton skeleton) (component (eql :urls)))
    (let* ((package-name (string-downcase (symbol-name (for-package skeleton)))))
      (with-skeleton-file (os skeleton #p"urls.lisp")
	(format os "(in-package :~a)~%~%" package-name)
	(format os "(reset-urls)~%")
	(format os "(defurl \"^/$\" :handler (home-page))~%"))))

  (:method ((skeleton skeleton) (component (eql :locales)))
    )

  (:method ((skeleton skeleton) (component (eql :server)))
    (let* ((package-name (string-downcase (symbol-name (for-package skeleton)))))
      (with-skeleton-file (os skeleton #p"server.lisp")
	(format os "(in-package :~a)~%~%" package-name)
	(format os "(defvar *httpd-port* 8000)~%~%")
	(format os "(let ((httpd (create-web-server (:port *httpd-port*~%")
	(format os "                                 :read-timeout 0.5~%")
	(format os "                                 :write-timeout 0.5~%")
	(format os "                                 :access-log-destination *error-output*~%")
	(format os "                                 :message-log-destination *error-output*)~%")
	(format os "                                 :packages (( \"/\" \"~a\"))))~%" package-name)
	(format os "    (*locale-package :~a))~%" package-name)
	(format os "    (init-localization :~a \"en_US\")~%" package-name)
	(format os "    (defun start-httpd-server (&key (wait nil))~%")
	(format os "      (run-web-server httpd :wait wait))~%~%")
	(format os "    (defun stop-httpd-server ()~%")
	(format os "      (hunchentoot:stop httpd)))~%~%")
	)))

  (:method ((skeleton skeleton) (component (eql :quickrun)))
    (let* ((package-name (string-downcase (symbol-name (for-package skeleton)))))
      (with-skeleton-file (os skeleton #p"quickrun.lisp")
	(format os "(ql:quickload :~a)~%~%" package-name)
	(format os "(~a:start-httpd-server)~%" package-name)))))

(defun make-skeleton (&key for in)
  "Generate a skeleton for a package in the indicated location;  "
  (declare (type (or symbol string) for) 
	   (type (satisfies cl-fad:directory-pathname-p) in))
  (let ((skeleton (make-instance 'skeleton :package for :location in)))
    (generate-skeleton skeleton)
    skeleton))
