;; Copyright (c) 2010 Phil Hargett

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

(export
 (list

  'create-web-server
  'run-web-server

  )
 )

;;;------------------------------------------------------------------------------------
;;; Classes + types
;;;------------------------------------------------------------------------------------

(defclass web-server-acceptor (hunchentoot:easy-acceptor)
  ((template-provider-registry
    :initform nil
    :initarg :template-providers
    :accessor template-provider-registry-of)
   (tag-library-provider-registry
    :initform nil
    :initarg :tag-library-providers
    :accessor tag-library-provider-registry-of)
   (dispatch-table
    :initform nil
    :initarg :dispatchers
    :accessor dispatch-table-of)))

;;;------------------------------------------------------------------------------------
;;; Generics
;;;------------------------------------------------------------------------------------

(defgeneric run-web-server ( server-acceptor &key wait))

;;;------------------------------------------------------------------------------------
;;; Implementation
;;;------------------------------------------------------------------------------------

(defmethod hunchentoot:acceptor-dispatch-request ((hunchentoot:*acceptor* web-server-acceptor) (hunchentoot:*request* hunchentoot:request))
  (let* ((*template-provider-registry* (template-provider-registry-of hunchentoot:*acceptor*) )
	 (*tag-library-provider-registry* (tag-library-provider-registry-of hunchentoot:*acceptor*) )
	 (hunchentoot:*log-lisp-errors-p* t)
	 (hunchentoot:*show-lisp-errors-p* t)
	 (hunchentoot:*dispatch-table* (dispatch-table-of hunchentoot:*acceptor*) ))
    (call-next-method)))

(defmethod run-web-server ( (server-acceptor web-server-acceptor) &key (wait nil) )
  (let* ((acceptor (hunchentoot:start server-acceptor))
	 (taskmaster (hunchentoot::acceptor-taskmaster acceptor)))
    (when wait
      (bt:join-thread (hunchentoot::acceptor-process taskmaster) ))
    acceptor))

(defmacro create-web-server (( &rest initialization-args)
			     &key 
			     (folders nil) 
			     (packages nil)
			     (dispatchers nil))
  "Create a web server, using the provided initialization arguments for constructing the acceptor.
The acceptor class used internally inherits from hunchentoot's easy-acceptor, so initialization arguments
valid for that class are also valid here as well.

In addition to initialization arguments, the following keywords are understood:

 * folders : a list of pairs URL -> path mappings, where each points to a filesystem folder for static files
 * packages : a list of of pairs of URL -> package mappings, where packages should have a www/ subfolder containing static files
 * dispatchers: list of additional hunchentoot dispatchers to use, if the server cannot otherwise find a handler for a request
after attempting to locate a handler based on provided URL routes"
  (let ((folder-dispatchers (mapcar (lambda (folder)
				      (destructuring-bind (url-prefix path) folder
					`(hunchentoot:create-folder-dispatcher-and-handler ,url-prefix ,path))) 
				    folders))
	(package-file-cache-dispatchers (mapcar (lambda (package)
						  (destructuring-bind (url-prefix package &key (populate t) (static t) ) package
						    `(create-file-cache-dispatcher-and-handler 
						      (make-package-fs-cache ,package :populate ,populate :static ,static)
						      ,url-prefix)))
						packages))
	(template-provider-registry (local-template-provider-registry-symbol) )
	(tag-library-provider-registry (local-tag-library-provider-registry-symbol) ))
    `(make-instance 'web-server-acceptor
		    ,@initialization-args
		    :template-providers ,template-provider-registry
		    :tag-library-providers ,tag-library-provider-registry
		    :dispatchers (list
				  ,@folder-dispatchers
				  ,@dispatchers
				  ,@package-file-cache-dispatchers
				  #'dispatch-url-cache))))
