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
;;; Classes + types
;;;------------------------------------------------------------------------------------

(defclass template-provider-registry ()
  ((template-providers 
    :initform () 
    :initarg :providers
    :accessor template-providers-of)
   (modified-time 
    :initform (universal-to-timestamp 0) 
    :initarg :modified 
    :accessor modified-time-of)))

;;;------------------------------------------------------------------------------------
;;; Dynamic variables
;;;------------------------------------------------------------------------------------

(defvar *template-provider-registry* ()
  "List of template providers, each of which will be called to resolve a template name
  to locate an appropriate template")

(defvar *template-cache* (make-hash-table :test 'equal)
  "Cache of template names to template objects")

(defvar *template* ()
  "While reading a template, this is the current template object for whom
   a definition is being read")

(defvar *minimum-template-stale-time* 1
  "Minimum time in seconds for which a template must remain stale (e.g. not expire,
   before reloading from its source")

(defvar *default-template-folder* "templates"
  "The default folder for templates")

(defvar *package-template-folder* *default-template-folder*
  "The name of the folder within a package's folder structure where template 
   files are located")


;;;------------------------------------------------------------------------------------
;;; Conditions
;;;------------------------------------------------------------------------------------

(define-condition template-not-found-error (error) 
  ((template-path :initarg :path :reader template-path))
  (:report (lambda (condition stream)
             (format stream "Could not find template ~a~%."
                     (template-path condition)))))

;;;------------------------------------------------------------------------------------
;;; Templates
;;;------------------------------------------------------------------------------------

(defclass template ()
  ((path :initarg :path :reader template-path)
   (provider :initarg :provider :accessor template-provider)
   (libraries :initform () :accessor tag-libraries-used)
   (modified-time :initform (now) :accessor modified-time-of)
   (package :initform (make-package (gensym)) :initarg :package :accessor template-package)
   (args :initform () :initarg :args :accessor template-args)
   (kwargs :initform () :initarg :kwargs :accessor template-keyword-args)
   (definition :initform () :initarg :definition :accessor definition-of)))

(defgeneric template-stalep (tmpl)
  (:method ( (tmpl template) )
    (if (definition-of tmpl)
	(timestamp> (now) 
		    (timestamp+ (modified-time-of tmpl) *minimum-template-stale-time* :sec))
	t)))

(defgeneric template-tags-expiredp (tmpl)
  (:documentation "Return t if any of the known required tag libraries
   used in the template have expired; nil otherwise")
  (:method ( (tmpl template) )
    (find-if #'(lambda (library-name) 
		 (let ((library (find-cached-tag-library library-name)))
		   (if library
		       (tag-library-expiredp (find-cached-tag-library library-name) )
		       t)))
	     (tag-libraries-used tmpl ))))

(defgeneric template-expiredp (tmpl)
  (:method ( (tmpl template) )
    (when (template-stalep tmpl)
      (or (template-tags-expiredp tmpl)
	  (not (slot-boundp tmpl 'provider) )
	  (provider-template-expiredp tmpl (template-provider tmpl) )))))

;;;------------------------------------------------------------------------------------
;;; Template providers
;;;------------------------------------------------------------------------------------

(defclass template-provider ()
  ())

; -- Base functions suggested all providers implement

(defgeneric provider-template-expiredp (*template* provider)
  (:documentation "Return t if the provider considers
   the template expired, otherwise nil")
  (:method ( tmpl (provider template-provider)  )
    t))

(defgeneric load-template-from-provider (*template* template-path provider)
  (:documentation "If the provider can provide a template with the indicated path,
   return the template; otherwise, return nil"))

; -- Helper and framework functions

(defun process-directive (expr)
  "Useful for pre-processing specific expressions as directives inside a template;
  returns nil if the expression is *not* a directive--returns t if the expr
  should be regarded as a directive and discarded"
  (if (listp expr)
      (cond ;; will likely need to add more cases over time
	( (eql '+tag-library (car expr) ) (eval expr) t )
	( (eql 'use-package (car expr) ) (eval expr) t ))	   
      ;; defaults to nil if no match, meaning no directive
      nil))

(defgeneric read-template-definition (input-stream template-package template-args template-keyword-args)
  (:documentation "Read a template definition from a stream")

  (:method ( (input-stream stream) (template-package package) (template-args list) (template-keyword-args list) )
    (let ((*package* template-package)
	  (*read-eval* nil) )
      ;; import arguments into the template package--otherwise reader will create different symbols
      (dolist (arg template-args)
	(import arg template-package))
      (dolist (kwarg template-keyword-args)
	(import kwarg template-package))
      ;; setup template package for use
      (use-package 'cl template-package)
      (use-package 'cl-user template-package)
      (use-package 'hh-web template-package)
      ;; read template
      (loop 
	 for expr = (with-tag-reader () (read input-stream nil :eof)) then (with-tag-reader () (read input-stream nil :eof))
	 until (eq expr :eof)
	 unless (process-directive expr) collect expr)))
  (:method ( (input-string string) (template-package package) (template-args list) (template-keyword-args list) )
    (with-input-from-string (input-stream input-string)
      (read-template-definition input-stream template-package template-args template-keyword-args))))

(defun load-template-definition (*template* template-path &optional (*template-provider-registry* *template-provider-registry*) )
  "Load the indicated template definition from the first provider that can"
  (when *template-provider-registry*
    (let* ((template-providers (template-providers-of *template-provider-registry*)) 
	   (template-provider (car template-providers) )
	   (found-template-definition (when template-provider
					(load-template-from-provider *template* template-path template-provider ))))
      (if found-template-definition
	  found-template-definition
	  (load-template-definition *template* template-path (cdr template-providers) )))))

(defgeneric load-template (template-path &optional template-args template-keyword-args)
  (:method (template-path &optional (template-args nil) (template-keyword-args nil) )
    (let* ((*template* (make-instance 'template 
				      :path template-path
				      :args template-args 
				      :kwargs template-keyword-args)) 
	   (*package* (template-package *template*))
	   (definition (eval `(lambda (,@template-args ,@(if template-keyword-args `(&key ,@template-keyword-args)) )
				,@(or (load-template-definition *template* template-path)
				      `( (error 'template-not-found-error :path ,template-path) ))))) )
      (setf (definition-of *template*) definition)
      (setf (gethash template-path *template-cache*) *template*)
      *template*)))

(defun flush-template (template-path cached-template)
  "Safely a cached template, typically for reloading"
  ;; remove the template from the cache
  (remhash template-path *template-cache*)
  ;; also, delete the template's package...could clutter up the runtime
  (when (template-package cached-template)
    (delete-package (template-package cached-template))))

(defun find-cached-template (template-path)
  (let ((cached-template (gethash template-path *template-cache* nil) ))
    (if cached-template
	(if (template-expiredp cached-template) 
	    (progn
	      (flush-template template-path cached-template)
	      nil)
	    cached-template))))

;; -------- File-based template provider ---------------------
;; 
;;  Generalized definitions designed to aid any template provider
;;  that serves templates from a filesystem

(defclass file-based-template-provider (template-provider)
  ()
  (:documentation "Generalized type for providers serving templates from the filesystem"))

(defgeneric template-provider-base-directory (provider)
  (:documentation "Returns the base directory on a fileystem for templates 
    served by the provider.  Directory name should end in /."))

(defgeneric template-full-path (template-path provider)
  (:method (template-path (provider file-based-template-provider) )
    (let ((provider-path (template-provider-base-directory provider) ))
	  ;; folder-name should be an absolute path, ending / to indicate directory
      (merge-pathnames (make-pathname :directory (pathname-directory template-path)
				      :name (pathname-name template-path)
				      :type (pathname-type template-path)) 
		       (make-pathname :host (pathname-host provider-path)
				      :directory (pathname-directory provider-path))))))

(defgeneric template-file-modified-time (template-path provider)
  (:method (template-path (provider file-based-template-provider) )
    (let ((full-path (template-full-path template-path provider) ))
      (universal-to-timestamp (file-write-date full-path)))))

; -- Implementation of base template-provider functions

(defmethod provider-template-expiredp (*template* (provider file-based-template-provider) )
  (let ((full-path (template-full-path (template-path *template*) provider) ))
    (timestamp> (template-file-modified-time full-path provider)
		(modified-time-of *template*))))

(defmethod load-template-from-provider ( (*template* template) template-path (provider file-based-template-provider) )
  (let ((full-path (template-full-path template-path provider) ))
    (when (probe-file full-path)
      (setf (template-provider *template*) provider)
      (setf (modified-time-of *template*) 
	    (template-file-modified-time template-path provider))
      (read-template-definition (open full-path :direction :input) (template-package *template*)
				(template-args *template*)
				(template-keyword-args *template*)))))

;; -------- Folder provider ---------------------
;; 
;;  provides templates in individual files of a specified 
;;  folder on the filesystem

(defclass folder-template-provider (file-based-template-provider)
  ((folder :initarg :folder :accessor folder-of)
   (modified-time :initform (now) :initarg :modified :accessor modified-time-of)))

(defmethod template-provider-base-directory ( (provider folder-template-provider) )
  (folder-of provider))

(defun create-folder-template-provider (folder)
  (make-instance 'folder-template-provider :folder folder))

;; -------- ASDF system provider ---------------------
;; 
;;  Services for templates a "templates" folder using 
;;  asdf:system-relative-pathname and the indicated system, and provides
;;  templates in individual files from any of those locations
;;

(defclass asdf-system-provider (folder-template-provider)
  ((folder :initform *package-template-folder*) 
   (system :initform nil :initarg :system :accessor system-of)))

(defmethod template-provider-base-directory ( (provider asdf-system-provider) )
  (asdf:system-relative-pathname (system-of provider) (make-pathname :directory `(:relative ,(folder-of provider))) ))

(defun create-asdf-system-template-provider (system &key ((:folder folder) *package-template-folder*))
  (make-instance 'asdf-system-provider :system system :folder folder))

; -- Implementation of base template-provider functions

(defmethod provider-template-expiredp (*template* (provider asdf-system-provider) )
  (let ((full-path (template-full-path (template-path *template*) provider) ))
    (when (probe-file full-path) ;; if file does not exist, no point in declaring it expired--can't reload anyway
      (timestamp> (template-file-modified-time full-path provider)
		  (modified-time-of *template*)))))

(defmethod load-template-from-provider ( (*template* template) template-path (provider asdf-system-provider) )
  (let ((full-path (template-full-path template-path provider) ))
    (when (probe-file full-path)
      (setf (template-provider *template*) provider)
      (setf (modified-time-of *template*) 
	    (template-file-modified-time template-path provider))
      (read-template-definition (open full-path :direction :input) (template-package *template*)
				(template-args *template*)
				(template-keyword-args *template*)))))


;;;------------------------------------------------------------------------------------
;;;  Template declaration
;;;
;;;  Templates can have arguments, both positional and keyword.  It is important
;;;  that these argument lists have no specializers or other qualifiers (such as 
;;;  default values), as the same lists will be used both to declare arguments to 
;;;  the template invocation macro and to pass values to the template's definition 
;;;  function.
;;;
;;;------------------------------------------------------------------------------------

(defmacro deftemplate (name
		       path
		       &key
		       ((:args template-args) nil))
  `(progn

     ;; pre-load -- note that this will likely create definitions in error, because a template cannot be found
     ;; doing this so that a "reload" later can find all the templates to reload
     (handler-case 
	 (load-template ,path (quote ,template-args))
       ;; we want to catch this error and continue, in case the template will be
       ;; available later
       (template-not-found-error (c) (format *standard-output* "Delaying load of ~s: ~s~%" ',name c))) 

     ;; create a macro to invoke the template
     (defun ,name (,@template-args)
       (let* ((*tag-library-provider-registry* (or *tag-library-provider-registry* ,(local-tag-library-provider-registry-symbol)))
	      (*template-provider-registry* (or *template-provider-registry* ,(local-template-provider-registry-symbol)))
	      (tmpl (or (find-cached-template ,path) 
			(load-template ,path (quote ,template-args)))))
	    (when tmpl
	      (let ((*template* tmpl)
		    (*package* (template-package tmpl)))
		(funcall (definition-of tmpl) ,@template-args)))))))

(defun local-template-provider-registry-symbol () 
  "Returns a symbol in the current package for storing the template provider registry expected by the package"
  (intern "*PACKAGE-TEMPLATE-PROVIDER-REGISTRY*" *package*))

(defun local-tag-library-provider-registry-symbol () 
  "Returns a symbol in the current package for storing the tag library provider registry expected by the package"
  (intern "*PACKAGE-TAG-LIBRARY-PROVIDER-REGISTRY*" *package*) )

(defmacro deftemplates (&key
			((:tag-library-packages tag-library-packages) nil)
			((:template-packages template-packages) nil)
			((:templates templates) nil)
			((:template-folder default-template-folder) '*package-template-folder*)
			((:tag-library-folder default-tag-library-folder) '*package-tag-library-folder*))
  (let ((template-provider-registry (local-template-provider-registry-symbol) )
	(tag-library-provider-registry (local-tag-library-provider-registry-symbol) ))
    `(progn
       (defparameter ,template-provider-registry
	 (make-instance 'template-provider-registry 
			:providers
			(list ,@(append (mapcar (lambda (template-package-spec)
						  (destructuring-bind (template-package template-folder)
						      (if (listp template-package-spec)
							  template-package-spec
							  (list template-package-spec default-template-folder))
						    `(create-asdf-system-template-provider (quote ,template-package) :folder ,template-folder)))
						template-packages)
					;; always here by default
					(list `(create-asdf-system-template-provider 'hh-web :folder *default-template-folder*))))))

       (defparameter ,tag-library-provider-registry
	 (list ,@(append (mapcar (lambda (tag-library-package-spec)
				   (destructuring-bind (tag-library-package tag-library-folder)
				       (if (listp tag-library-package-spec)
					   tag-library-package-spec
					   (list tag-library-package-spec default-tag-library-folder))
				     `(create-asdf-system-tag-library-provider (quote ,tag-library-package) :folder ,tag-library-folder)))
				 tag-library-packages)
			 ;; always here by default
			 (list `(create-asdf-system-tag-library-provider 'hh-web :folder *default-tag-library-folder*)))))

       (let ((*tag-library-provider-registry* ,tag-library-provider-registry)
	     (*template-provider-registry* ,template-provider-registry))
	 ,@(mapcar (lambda (template)
		     `(deftemplate ,@template))
		   templates)))))

(defun load-templates (&optional (*template-provider-registry* *template-provider-registry*))
  "Refresh the list of templates, if necessary"
  (loop for provider in (template-providers-of *template-provider-registry*)
     do (let* ((*package* (find-package (system-of provider)))
	      (package-name (intern (package-name *package*) :keyword))
	      (templates-path (asdf:system-relative-pathname package-name "templates" :type "lisp")))
	  (when (and  (timestamp> (now) 
				  (timestamp+ (modified-time-of provider) 
					      *minimum-template-stale-time* :sec))
		      (timestamp> (universal-to-timestamp (file-write-date templates-path))
				  (modified-time-of provider)))
	    ;; time to check for changes on disk
	    ;; read templates
	    (load templates-path)
	    (setf (modified-time-of provider) (now))))))

;;;------------------------------------------------------------------------------------
;;;  Tag library use in templates
;;;
;;;  Used at top of template files; modifies the template's package (actually, *package*)
;;;  to use the tag library's package.
;;;

(defun +tag-library (library-name)
  "Find the tag-library (if possible), and import it into the
   current package (e.g., found by looking at *package* in
   dynamic environment
   "
  (let ((library (import-tag-library library-name)))
    (when library
      (when *template*
	(putendnew library-name (tag-libraries-used *template*) :test 'equal))
      library))
  nil)
