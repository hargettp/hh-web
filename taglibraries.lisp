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
;;; Dynamic variables
;;;------------------------------------------------------------------------------------

(defvar *tag-library-provider-registry* ()
  "A list of functions such that when each is called with 1 argument (a tag name), they
  return the tag source as a stream or string")

(defvar *minimum-tag-library-stale-time* 1
  "Minimum time in seconds for which a tag library must remain stale (e.g. not expire)
   before reloading from its source")

(defvar *default-tag-library-folder* "taglibraries"
  "The default folder for tag libraries")

(defvar *package-tag-library-folder* *default-tag-library-folder*
  "Default folder within a package that contains the package's tag libraries")

;;;------------------------------------------------------------------------------------
;;; Conditions
;;;------------------------------------------------------------------------------------

(define-condition tag-library-not-found-error (error) 
  ((library-name :initarg :name :reader library-name))
  (:report (lambda (condition stream)
             (format stream "Could not find tag library ~a~%."
                     (library-name condition)))))

;;;------------------------------------------------------------------------------------
;;; Utilities
;;;------------------------------------------------------------------------------------

(defmacro putendnew (some-object some-place &key ((:test test) 'equal))
  `(unless (member ,some-object ,some-place :test ,test)
     (setf ,some-place (nconc ,some-place (list ,some-object)))))

;;;------------------------------------------------------------------------------------
;;; Tag libraries
;;;------------------------------------------------------------------------------------

(defclass tag-library ()
  ((name :initarg :name :reader tag-library-name)
   (provider :initarg :provider :accessor tag-library-provider)
   (modified-time :initform (now) :accessor modified-time-of)
   (package :initform nil :initarg :package :accessor tag-library-package)
   (libraries :initform () :accessor tag-libraries-used
    :documentation "A list of symbols identifying other libraries upon 
    which this one depends")
   (tags :initform (make-hash-table) :accessor tag-library-tags
    :documentation "A list of symbols identifying tags defined by this library")))

(defmethod initialize-instance :after ( (library tag-library) &key )
  (let* ((pkg-name (intern (symbol-name (tag-library-name library) )
			   'cl-user))
	 (pkg (or (find-package pkg-name)
		  (make-package pkg-name))))
    (setf (tag-library-package library) pkg)
    library))

(defgeneric tag-library-stalep (library)
  (:method ( (library tag-library) )
    (timestamp> (now) 
       (timestamp+ (modified-time-of library) *minimum-tag-library-stale-time* :sec))))

(defgeneric tag-library-expiredp (library)
  (:documentation "Return if the library (or any of its dependent libraries) has epxired"))

;;;------------------------------------------------------------------------------------
;;; Tag library providers
;;;------------------------------------------------------------------------------------

(defclass tag-library-provider ()
  ())

; -- Base functions suggested all providers implement

(defgeneric provider-tag-library-expiredp (library provider)
  (:documentation "Return t if the provider considers
   the tag library expired, otherwise nil")
  (:method ( library (provider tag-library-provider)  )
    t))

(defgeneric load-tag-library-from-provider (library-name provider)
  (:documentation "If the provider can provide a tag library with the indicated name,
   return the tag library; otherwise, return nil"))

; -- Helper and framework functions

(defgeneric read-tag-library (input-stream)
  (:documentation "Read in a tag library from a stream")

  (:method ( (input-stream stream) )
    (let ((*tag-library* nil)
	  (*read-eval* nil) 
	  (*package* (find-package 'cl-user)))
      ;; read tag library
      (use-package :hh-web *package*)
      (handler-case (loop 
		       ;; TODO as a consequence of how this is implemented, any whitespace after last tag 
		       ;; causes an irrelevant EOF error
		       while (listen input-stream)
		       do (eval (with-tag-reader () (read input-stream))))
	(end-of-file () t))
      ;; expecting an in-tag-library call inside the file
      ;; to set the value of this variable
      *tag-library*))
  (:method ( (input-string string) )
    (with-input-from-string (input-stream input-string)
      (read-tag-library input-stream))))

(defgeneric load-tag-library (library-name &optional *tag-library-provider-registry*)
  (:documentation "Load the tag library from the first provider that can provide it")
  (:method (library-name &optional (*tag-library-provider-registry* *tag-library-provider-registry*) )
    (when *tag-library-provider-registry*
      (let* ((provider (car *tag-library-provider-registry*) )
	     (found-tag-library (when provider
				  (load-tag-library-from-provider library-name provider))))
	(if found-tag-library
	    (progn
	      (setf (gethash library-name *available-tag-libraries*) found-tag-library)
	      found-tag-library)
	    (load-tag-library library-name (cdr *tag-library-provider-registry*)))))))

(defun flush-tag-library (library-name cached-library)
  "Safely a cached library, typically for reloading"
  ;; remove the library from the cache
  (remhash library-name *available-tag-libraries*)
  ;; also, delete the library's package...should make it easier to redefine it
  (when (template-package cached-library)
    (delete-package (tag-library-package cached-library))))

(defun find-cached-tag-library (library-name)
  (let ((cached-library (gethash library-name *available-tag-libraries* nil)))
    (if cached-library
	(if (tag-library-expiredp cached-library) 
	    (progn
	      (remhash library-name *available-tag-libraries*)
	      nil)
	    cached-library))))

(defmethod tag-library-expiredp ( (library tag-library) )
  (when (tag-library-stalep library)
    (or (not (slot-boundp library 'provider)) ;; in case a library fails during creation with no provider found
	(provider-tag-library-expiredp library (tag-library-provider library) )
	(find-if #'(lambda (library-name) 
		     (let ((library (find-cached-tag-library library-name)))
		       (if library
			   (tag-library-expiredp (find-cached-tag-library library-name) )
			   t)))
		 (tag-libraries-used library)))))

;; -------- Folder provider ---------------------
;; 
;;  provides tag libraries in individual files of a specified 
;;  folder on the filesystem
;;

(defclass folder-tag-library-provider (tag-library-provider)
  ((folder :initarg :folder :accessor folder-of)))

(defgeneric tag-library-full-path (library-name provider)
  (:method (library-name (provider folder-tag-library-provider) )
    ;; folder-name should be an absolute path, ending / to indicate directory
    (let ((provider-path (folder-of provider)))
      (merge-pathnames (make-pathname :name (pathname-name (string-downcase
							    (symbol-name library-name)))
				      :type "lisp")
		       (make-pathname :host (pathname-host provider-path)
				      :directory (pathname-directory provider-path))))))

(defgeneric tag-library-file-modified-time (library-name provider)
  (:method (library-name (provider folder-tag-library-provider) )
    (let ((full-path (tag-library-full-path library-name provider) ))
      (universal-to-timestamp (file-write-date full-path)))))

(defun create-folder-tag-library-provider (folder)
  (make-instance 'folder-tag-library-provider :folder folder))

(defun create-asdf-system-tag-library-provider (system &key ((:folder *package-tag-library-folder*) *package-tag-library-folder*))
  (make-instance 'folder-tag-library-provider 
		 :folder (asdf:system-relative-pathname 
					(asdf:find-system system) 
					(make-pathname :directory `(:relative ,*package-tag-library-folder*)))))


; -- Implementation of base tag-library-provider functions

(defmethod provider-tag-library-expiredp (library (provider folder-tag-library-provider) )
  (timestamp>  (tag-library-file-modified-time (tag-library-name library) provider)
	(modified-time-of library)))

(defmethod load-tag-library-from-provider (library-name  (provider folder-tag-library-provider))
  (let ((full-path (tag-library-full-path library-name provider)))
    (when (probe-file full-path)
      (let ((library (read-tag-library (open full-path :direction :input))))
	;; TODO note: if there is a failure during read, then neither of the following 2
	;; statements will execute
	(setf (tag-library-provider library) provider)
	(setf (modified-time-of library) 
	      (tag-library-file-modified-time library-name provider))
	library))))


; -- Tag library support for rendering to html

;; generic function declared in tags.lisp, because tags need to invoke the function 
;; at point of tag declaration

(defmethod add-tag-to-library ( (tag-definition tag-definition) &optional (*tag-library* *tag-library*))
  (if *tag-library*
      (setf (gethash (tag-symbol tag-definition) (tag-library-tags *tag-library*))
	    tag-definition)))

(defmethod expand-expression-with-libraries (expr &optional (*available-tag-libraries* *available-tag-libraries*))
  `(macrolet (,@(tag-library-tag-expansions (loop for v being the hash-values of *available-tag-libraries* collect v)))
     ,expr))

(defgeneric tag-library (name)
  (:documentation "Return the library with the indicated name"))

(defgeneric tag-library-tag-expansions (some-library)
  (:documentation "For some library, return the list of macro expansions 
   for each tag in the library"))

(defmethod tag-library ( (name symbol) )
  (gethash name *available-tag-libraries*))

(defmethod tag-library-tag-expansions ( (some-library tag-library) )
  (loop for each-tag being the hash-keys of (tag-library-tags some-library)
     collect (tag-expansion each-tag)))

(defmethod tag-library-tag-expansions ( (some-library (eql 'nil) ) )
  nil)

(defmethod tag-library-tag-expansions ( (some-library symbol) )
  (tag-library-tag-expansions (tag-library some-library)))

(defmethod tag-library-tag-expansions ( (some-libraries list) )
  "Gather all tag expansions for all libraries in the list"
  
  (apply 'concatenate (cons 'list 
			    (mapcar (lambda (each-library) 
				      (tag-library-tag-expansions each-library))
				    some-libraries))))

;;;------------------------------------------------------------------------------------
;;;  Tag library language
;;;
;;;  Macros & functions used in a tag library file
;;;

(defmacro in-tag-library (library-name)
  "All following tag definitions will go into this named library"
  `(let* ((library (make-instance 'tag-library :name (quote ,library-name)) )
	  (library-package (tag-library-package library)))
     (setf (gethash (quote ,library-name) *available-tag-libraries*)
	   library)
     (setf *package* library-package )
     (setf *tag-library* library)

     ;; setup tag library package for use
     (use-package 'cl library-package)
     (use-package 'cl-user library-package)
     (use-package 'hh-web library-package)
     library))

(defun import-tag-library (library-name)
  (let ((library (or (find-cached-tag-library library-name) 
		     (load-tag-library library-name))))
    (if library
	(progn
	  (use-package (tag-library-package library) *package*)
	  library)
	(error 'tag-library-not-found-error :name library-name))))

(defun use-tag-library (library-name)
  "Add the named tag library to the list of libraries used by the
  active tag library"
  (let* ((library (import-tag-library library-name) ))
    (when library
      (when *tag-library*
	(putendnew library-name (tag-libraries-used *tag-library*) :test 'equal))
      library)))



	
