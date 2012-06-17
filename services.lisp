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

;;;------------------------------------------------------------------------------------
;;; Constants
;;;------------------------------------------------------------------------------------

(defvar +method-parameter+ "_method"
  "Special parameter used in POST requests to indicate the actual desired
  method (should be either 'DELETE' or 'PUT').  Used by some browser-side
  code as a substitute for situations where the browser will not allow Javascript
  to send a native DELETE or PUT request.")

(defvar +meta-service-request-parameter+ "_meta"
  "When this parameter appears for a service, the service will perform other 'meta'
   activities for the service.  See handle-service-meta-request.")

;; common values for +meta-serivce-request-parameter+
(defvar +client-meta-service-request+ "client")


;;;------------------------------------------------------------------------------------
;;; Template for form generation
;;;------------------------------------------------------------------------------------

;; TODO fix service form user interface
;; (deftemplate service-form "service-form.html"
;;   :args (service-name)
;;   )


;;;------------------------------------------------------------------------------------
;;; Query parameters
;;;------------------------------------------------------------------------------------

(defmacro with-http-parameters ( (&rest parameter-list) &rest body)
  "Execute body with the values of provided parameters in indicated variables.  The parameter-list
   should be a list of variable/parameter combinations, where each combination in the list
   is of the form (variable parameter).  The parameter name should be a string, the variable
   should be a symbol referenced in the body."
  `(let (,@(mapcar (lambda (variable-parameter) 
		     (destructuring-bind (variable parameter)
			 variable-parameter
		     `(,variable (hunchentoot:parameter ,parameter))))
		  parameter-list))
     (let ((result (progn ,@body)))
       (log5:log-for (log5:trace) "With-http-parameters result is ~s~%" result)
       result)))

(defun meta-service-request-p (&optional (hunchentoot:*request* hunchentoot:*request*) )
  "If there is a _meta parameter present, then regard the request as a 'meta' request
   by returning t; otherwise return nil"
  (when (hunchentoot:parameter +meta-service-request-parameter+ hunchentoot:*request*) t))

;;;------------------------------------------------------------------------------------
;;; Request methods
;;;
;;;   Following the conventions of some script libraries (working around lack
;;;   support for all HTTP methods from script), both put & delete check for a
;;;   "_method" query parameter on a :post request to determine whether put or delete
;;;   were intended
;;;------------------------------------------------------------------------------------

(defun http-get-p (&optional (hunchentoot:*request* hunchentoot:*request*) )
  (eql :get (hunchentoot:request-method hunchentoot:*request*) ))

(defun http-post-p (&optional (hunchentoot:*request* hunchentoot:*request*) )
  (and (eql :post (hunchentoot:request-method hunchentoot:*request*) )
       (not (hunchentoot:parameter +method-parameter+ hunchentoot:*request*) )))

(defun http-put-p (&optional (hunchentoot:*request* hunchentoot:*request*) )
  (or (eql :put (hunchentoot:request-method hunchentoot:*request*) )
      (and (eql :post (hunchentoot:request-method hunchentoot:*request*) )
	   (equal (string-downcase (hunchentoot:parameter +method-parameter+ hunchentoot:*request*) )
		  "put"))))

(defun http-delete-p (&optional (hunchentoot:*request* hunchentoot:*request*) )
  (or (eql :delete (hunchentoot:request-method hunchentoot:*request*) )
      (and (eql :post (hunchentoot:request-method hunchentoot:*request*) )
	   (equal (string-downcase (hunchentoot:parameter +method-parameter+ hunchentoot:*request*) )
		  "delete"))))

;;;------------------------------------------------------------------------------------
;;; Generic functions
;;;------------------------------------------------------------------------------------

(defgeneric handle-service-meta-request (service-name meta &optional hunchentoot:*request*)
  (:documentation "Handle the service meta request with the named service, using the
    meta argument (if non-nil) or the +meta-service-request-parameter+ to determine what to do.  
    Common meta service requests include (but are not limited to) the following values of 
    +meta-service-request-parameter+:

     client - returns sufficient Javascript for invoking the service as an ordinary
       function form javascript
     help - returns an HTML page with a documentation string for the service
     documentation - same as help
     doc - same as help
     docs - same as help
     form - returns an HTML page with both the documentation string for the
       service, but also a form suitable for entering parameters and then
       invoking the service, as well as displaying (and changing?) values
       of involved cookies before and after the invocation."))

(defgeneric generate-service-client-script (service-name)
  (:documentation "Return a string of client-side Javascript sufficient for calling all
   methods of the service"))

(defgeneric service-documentation-string (service-name)
  (:documentation "Return the documentation string provided with the service definition"))

(defgeneric service-args (service-name)
  (:documentation "Return a list of symbols identifying the arguments a service expects.  All
    methods of a service share the same arguments."))

(defgeneric service-methods (service-name)
  (:documentation "Return as a list the symbolic names of methods supported by the service"))

(defgeneric service-method-documentation-string (service-name method-name)
  (:documentation "Return the documentation string provided with the method definition"))

(defgeneric service-method-http-method (service-name method-name)
  (:documentation "Return the HTTP method expected for this service method"))

(defgeneric service-method-test (service-name method-name)
  (:documentation "Return the test expected for this service method"))

(defgeneric service-method-cookies (service-name method-name)
  (:documentation "Return the cookies touched by a service method"))

(defgeneric service-method-args (service-name method-name)
  (:documentation "Return the argument list (derived from URL path component,typically) for a service"))

(defgeneric service-method-parameters (service-name method-name)
  (:documentation "Return the parameter list for a service method"))

;;;------------------------------------------------------------------------------------
;;; Defining services
;;;------------------------------------------------------------------------------------

;; ------------------------- Default methods ------------------------------------------ 

(defmethod handle-service-meta-request ( (service-name symbol)  (meta (eql nil) ) &optional (hunchentoot:*request* nil) )
  "Default handling of service meta requests--"
  (let ((meta-request (string-downcase (hunchentoot:parameter +meta-service-request-parameter+ hunchentoot:*request*)) ))
    (log5:log-for (log5:info) "Handling meta request ~s for service ~s~%" meta-request service-name)
    (cond
      ((string-equal meta-request "client") ;; return client script
       (handle-service-meta-request service-name :client hunchentoot:*request*))
      ((string-equal meta-request "form") ;; return form for entering parameters
       (handle-service-meta-request service-name :form hunchentoot:*request*))
      (t
       (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
       ""))))

(defmethod handle-service-meta-request ( (service-name symbol) (meta (eql :client) ) &optional hunchentoot:*request*)
  "Return the client script for the service"
  (setf (hunchentoot:content-type*) "text/javascript")
  (generate-service-client-script service-name (hunchentoot:script-name hunchentoot:*request*)))

;; TODO fix service form user interface
;; (service-form service-name)
(defmethod handle-service-meta-request ( (service-name symbol) (meta (eql :form) ) &optional hunchentoot:*request*)
  "Return the form for entering service parameters manually")

(defun ps-service-method-parameter-list (parameter-list)
  "Helper for processing service parameter lists during script generation"
  (when parameter-list
    (destructuring-bind (var parm) (car parameter-list) 
      (append (list parm var )
	      (ps-service-method-parameter-list (cdr parameter-list) )))))

(defun ps-service-method-client-script (method-name service-args parameters http-method)
  (let ((effective-http-method (if (or (eql http-method :put)
				       (eql http-method :delete))
				   :post
				   http-method))
	(method-parameter (if (or (eql http-method :put)
				  (eql http-method :delete))
			      `(,+method-parameter+ ,http-method)
			      nil)))
    `(defun ,method-name (service-url ,@(mapcar #'car parameters) success error)
       (ps:new (  (ps:@ j-query ajax)	;j-query.ajax
		  (ps:create
		   url service-url
		   type ,effective-http-method
		   ,@(when (or parameters method-parameter)
			   `(data (ps:create
				   ,@method-parameter
				   ,@(ps-service-method-parameter-list parameters))))
		   success success
		   error error))))))

;; ------------------------- Macros --------------------------------------------------- 

(defmacro map-service-method-defs ( (method-defs) &rest work )
  "Used to pull apart a method definition, which has the following structure.
   After the initial :method keyword, the structure below is a valid lambda list.

        (:method method-name
                       &key
		       ((:documentation documentation) nil)
		       ((:http-method http-method) :get)
		       ((:if test) nil)
		       ((:cookies cookies) nil)
		       ((:parameters parameters) nil)
	   )

  "
  (when method-defs
    `(mapcar (lambda (method-def)
	       (if (eql :method (car method-def) )
		   (destructuring-bind ( method-name &rest def ) (cdr method-def)
		     (declare (ignorable method-name) )
		     (destructuring-bind (
					  &key
					  ((:documentation documentation) nil)
					  ((:http-method http-method) :get)
					  ((:if test) t)
					  ((:cookies cookies) nil)
					  ((:parameters parameters) nil))
			 def
		       (declare (ignorable documentation http-method test cookies parameters) )
		       ,@work))
		   (error "Service method definition must start with :method")))
	     ,method-defs))) 

(defmacro defservice (name
		      (&key
		       ((:documentation documentation) nil)
		       ((:args service-args) nil))
		      &rest
		      methods)
  "Defines a service with the specified name, optional documentation, optional args and keyword args 
   (typically extracted from the URL associated with the service), and a set of methods.  

If present, args appear as the parameter lists of methods in client script
and are used to construct the path of the request.  The intent is
to enable passing of args from the client through the URL
and also to have named registers from the pattern
registered for the service available as well"
  `(progn

     (export 
      (list 
       (quote ,name)))

     (export
      (quote ,service-args))

     (defmethod generate-service-client-script ((service-name (eql (quote ,name)) ))
       (with-output-to-string (os)
	 ,@(map-service-method-defs 
	    (methods)
	    `(format os "~a~%~%" 
		     (ps:ps* (ps-service-method-client-script (quote ,method-name)
							      (quote ,service-args )
							      (quote ,parameters )
							      ,http-method))))))
     
     (defmethod service-documentation-string ( (service-name (eql (quote ,name)) ) )
       (quote ,documentation))

     (defmethod service-methods ( (service-name (eql (quote ,name)) ) )
       (quote ,(map-service-method-defs (methods)
					method-name)))

     (defmethod service-args ( (service-name (eql (quote ,name)) ) )
       (quote ,service-args))

     ,@(map-service-method-defs (methods)
				`(defmethod service-method-documentation-string ( (service-name (eql (quote ,name)) ) (method-name (eql (quote ,method-name)) ) )
				   ,documentation))

     ,@(map-service-method-defs (methods)
				`(defmethod service-method-http-method ( (service-name (eql (quote ,name)) ) (method-name (eql (quote ,method-name)) ) )
				   (quote ,http-method)))

     ,@(map-service-method-defs (methods)
				`(defmethod service-method-test ( (service-name (eql (quote ,name)) ) (method-name (eql (quote ,method-name)) ) )
				   (quote ,test)))

     ,@(map-service-method-defs (methods)
				`(defmethod service-method-cookies ( (service-name (eql (quote ,name)) ) (method-name (eql (quote ,method-name)) ) )
				   (quote ,cookies)))

     ,@(map-service-method-defs (methods)
				`(defmethod service-method-parameters ( (service-name (eql (quote ,name)) ) (method-name (eql (quote ,method-name)) ) )
				   (quote ,parameters)))

     ,@(map-service-method-defs (methods)
				`(export
				  (list 
				   ,@(loop for parameter in (mapcar #'car parameters ) 
					collect `(quote ,parameter)))))))

(defmacro defservice-client (service-name)

  `(progn
     ,@(loop for method-name in (service-methods service-name )
	  collect (let ((parameters (service-method-parameters service-name method-name))
			(local-method-name (intern (symbol-name method-name) *package*) ))
		    `(defun ,local-method-name (service-url ,@(mapcar #'car parameters) )
			 (drakma:http-request service-url
					      :method ,(service-method-http-method service-name method-name)
					      ,@(when parameters
						      `(:parameters (list
								     ,@(loop for param in parameters
									  ;; query string parameter name first, in this case
									  collect `(cons ,(cadr param) ,(car param))))))))))))

(defmacro map-service-method-handlers ((service-name method-handlers)
				       &rest
				       work)
  "Each method handler should have the following form:
     (:method method-name &rest method-handler-body)"
  (when method-handlers
    `(mapcar (lambda (method-handler)
	       (if (eql :method (car method-handler) )
		   (destructuring-bind (local-method-name &rest method-handler-body) (cdr method-handler)
		     (declare (ignorable method-name method-handler-body))
		     (let* (
			    (method-name (intern (symbol-name local-method-name) (symbol-package service-name) ) )
			    (http-method (service-method-http-method ,service-name method-name) )
			    (cookies (service-method-cookies ,service-name method-name) )
			    (parameters (service-method-parameters ,service-name method-name) )
			    (test (service-method-test ,service-name method-name) ))
		       (declare (ignorable http-method cookies parameters test) )
		       ,@work))
		   (error "Method handler must start with :method")))
	     ,method-handlers)))
  

(defmacro defservice-handler (service-name
			      &rest
			      method-handlers)
  (let ((local-service-name (intern (symbol-name service-name)) )
	(service-args (service-args service-name)))
    `(progn

       (defun ,local-service-name 
	   (,@service-args 
	    &optional 
	      (hunchentoot:*request* hunchentoot:*request*))
	 ,(service-documentation-string service-name)
	 (let ((service-name (quote ,service-name) )
	       ) (cond 
	     ( (meta-service-request-p hunchentoot:*request*) (handle-service-meta-request service-name nil hunchentoot:*request*) )
	     ,@(map-service-method-handlers (service-name method-handlers)
					 `( (and ,(cond
						   ( (eql http-method :get) '(http-get-p))
						   ( (eql http-method :put) '(http-put-p))
						   ( (eql http-method :delete) '(http-delete-p))
						   ( (eql http-method :post) '(http-post-p))
						   ( t nil)) 
						 ,test)
					    (log5:log-for (log5:info) "Executing method ~a for ~a" (quote ,method-name) service-name )
					    (with-cookies (,@cookies)
					      (with-http-parameters (,@parameters)
						,@method-handler-body))))
	     ( t 
	      (log5:log-for (log5:warn log5:info) "Could not dispatch ~a for service ~a" (hunchentoot:script-name hunchentoot:*request*) service-name )
	      (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+) 
	      "")))))))
  
(defmacro service-client (service-name)
  `(progn
     (setf (hunchentoot:content-type*) "text/javascript")
     (generate-service-client-script (quote ,service-name))))
