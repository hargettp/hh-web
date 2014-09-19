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

(defmacro hout (format-string &rest args )
  "Macro to simplify writing out html to correct stream"
  `(format *html-out* ,format-string ,@args))

;; -------------
;; tag reader

(defun set-tag-reader ()
  (set-macro-character #\{
		     (lambda (str char)
		       (declare (ignore char))
		       (let
			   ((*readtable* (copy-readtable *readtable* nil))
			    (keep-going t))
			 (set-macro-character #\} (lambda (stream char)
						    (declare (ignore char) (ignore stream))
						    (setf keep-going nil)))
			 (let
			     ((attributes (loop for key = (read str nil nil t)
					while keep-going
					for value = (read str nil nil t)
					collect key
					collect value)))
			   `(+@ ,@attributes))))))

(defmacro with-tag-reader ( () &rest body)
  `(let ((*readtable* (copy-readtable *readtable* nil)))
    (set-tag-reader)
    ,@body))

;;;------------------------------------------------------------------------------------
;;; Dynamic variables
;;;------------------------------------------------------------------------------------

(defvar *current-tag* nil
  "The current tag under construction (if any), within the scope of a tag's body")

(defvar *this-tag* nil
  "The active tag in the scope of a tag method (e.g., while rendering); while rendering a custom tag,
   *this-tag* will not change within the same method, but *current-tag* changes within the body of each
   tag in the content")

(defvar *default-tag-definition* nil
  "Provides default implementation for tag methods")

(defvar *tag-printing-level* 0
  "Used for pretty printing HTML, with proper indentation for nested tags")

(defvar *html-out* *standard-output* 
  "Output stream for rendering html")

(defvar *page-title* ""
  "The title of the page as it should be presented to the user in the browser")

(defvar *page-links* ()
  "A list of link tags that should appear in the head of the page")

(defvar *page-scripts* ()
  "A list of strings, each representing a script added to the page")

(defvar *page-ready-scripts* ()
  "A list of strings, each representing script to be run when the document is ready (after load)")

(defvar *page-styles* ()
  "A list of strings, each representing CSS added to the page")

(defvar *page-style-sheets* ()
  "A list of strings, each pointing to a styleseheet that the page references")

(defvar *page-script-libraries* ()
  "A list of strings, each pointing to a script library that the page referneces")

;;;------------------------------------------------------------------------------------
;;; Types
;;;------------------------------------------------------------------------------------

(defclass tag-definition ()
  ((symbol :initarg :symbol :accessor tag-symbol :documentation "The tag symbol is the unique identifier for a tag definition")
   (name :initarg :name :accessor tag-name :documentation "The tag name is the string used in the start tag when rendering to HTML")
   (bases :initform nil :initarg :bases :accessor tag-bases)
   (attributes :initform nil :initarg :attributes :accessor tag-attributes)
   (init :initform nil :initarg :init :accessor tag-init)
   (has-body :initform nil :initarg :has-body :accessor tag-has-body-p)
   (has-end-tag :initform nil :initarg :has-end-tag :accessor tag-has-end-tag-p)
   (scripts :initform nil :initarg :scripts :accessor tag-scripts)
   (ready-scripts :initform nil :initarg :ready-scripts :accessor tag-ready-scripts)
   (script-libraries :initform nil :initarg :script-libraries :accessor tag-script-libraries)
   (styles :initform nil :initarg :styles :accessor tag-styles)
   (style-sheets :initform nil :initarg :style-sheets :accessor tag-style-sheets)
   (content :initform nil :initarg :content :accessor tag-content))
  (:documentation "Holds the details of a particular type of tag. All slots are lambdas
   (or funcallables) except for symbol, name, bases, and attributes"))

(defclass htmltag ()
  ((_definition :initarg :definition :accessor tag-definition) 
   (_attribute-map :initform (make-hash-table :test #'equal) :initarg :attribute-map :accessor tag-attribute-map)
   (_body :initform () :initarg :body :accessor body-of)))

;;;------------------------------------------------------------------------------------
;;; Tag requirements of tag libraries
;;;------------------------------------------------------------------------------------

; -- Tag library functions that tags require for rendering

(defvar *available-tag-libraries* (make-hash-table :test 'eql)
  "All tag libraries")

(defvar *tag-library* nil
  "Current tag library to which new tag defintions will belong")

(defgeneric add-tag-to-library (tag-definition &optional tag-library)
  (:documentation "Called at the point of tag definition to add the tag to the library"))

(defgeneric expand-expression-with-libraries (expr &optional *available-tag-libraries*)
  (:documentation "Expand the expression using the provided libraries"))


;;;------------------------------------------------------------------------------------
;;; Generics
;;;------------------------------------------------------------------------------------

(defgeneric tag-name (*current-tag*)
  (:documentation "Return the name of the tag as a string 
used for rendering in html"))

(defgeneric tag-library-name-for-tag (*current-tag*)
  (:documentation "Return the name of the tag library in which the tag is defined"))

(defgeneric tag-expansion (some-tag-symbol)
  (:documentation "Return the macro for expanding a tag expression
into a tag object"))

(defgeneric tag-has-body-p (some-tag-symbol)
  (:documentation "Return t (the default) if a tag expects to 
   have a tag body; nil otherwise.  If no body, that implies
   the tag treats the raw body as an initializer for the tag,
   and that a custom html method will then use the tag's
   slots to render appropriate html."))

(defgeneric tag-has-end-tag-p (*current-tag*)
  (:documentation "Return t (the default) if a tag expects to 
   have an end tag rendered (e.g., </a>); nil otherwise.  If nil,
   then the tag's rendering only includes the start tag and content
   (if any).  Examples of use is the <img> and <input> tags."))


(defgeneric tag-separate-initializer-and-body (some-tag-symbol raw-body)
  (:documentation "Return 2 values (nil if the value is not applicable):
   the first is the tag initializer, the 2nd is the tag's computed body.
   Default behavior takes the first list starting ith +@ and uses the
   cdr of that list as the arguments to a make-instance call for that
   tag class.  All other items in the list will be returned in the 2nd
   value for the body of the tag"))

(defgeneric tag-attributes (*current-tag*)
  (:documentation "Return a list of attribute names for the tag"))

(defgeneric tag-attribute-value (*current-tag* attribute)
  (:documentation  "Return the value of attribute on tag"))

(defgeneric (setf tag-attribute-value) (value *current-tag* attribute)
    (:documentation "Set the value of attribute on the tag"))

(defgeneric render-tag-scripts (*current-tag*)
  (:documentation
   "Return the script that should be shared by all instances
     of some-tag's class within the same context (page, stylesheet, etc.)
    "))

(defgeneric render-tag-ready-scripts (*current-tag*)
  (:documentation
    "Return a string representing script to run when a page
     first loads.  During rendering, scripts are accumulated by visiting each tag
     in the page (depth, then breadth), and adding each unique script
     (based on string comparison) to the end of a list.  Consequently,
     outer tags have their ready script run before inner tags."))

(defgeneric render-tag-script-libraries (*current-tag*)
  (:documentation
    "Return a list of strings pointing to script libraries used
     by instances of this tag"))

(defgeneric render-tag-styles (*current-tag*)
  (:documentation
    "Return a list of styles expected by instances of this tag"))

(defgeneric render-tag-style-sheets (*current-tag*)
  (:documentation
    "Return a list of strings pointing to stylesheets used
     by instances of this tag"))

(defgeneric render-tag-start (*current-tag*)
  (:documentation "Render the beginning of a tag, including its attributes (by
    calling render-attributes)"))

(defgeneric render-tag-attributes (*current-tag*)
  (:documentation "Render a tag's attributes as string of name=value pairs"))

(defgeneric render-tag-body (*current-tag*)
  (:documentation "Render the body of a tag as html"))

(defgeneric render-tag-end (*current-tag*)
  (:documentation "Render the end of a tag"))

(defgeneric render-tag-content (*current-tag*)
  (:documentation "Render just the content portion of the tag (no styles or scripts):
   usually, the start tag, the rendered body, and the end tag.  In tags
   that have a custom html representation, this is usually the method overriden."))

(defgeneric render-as-html (some-expr)
  (:documentation "Return some expression as an HTML string"))


;;;------------------------------------------------------------------------------------
;;; Methods and their helpers
;;;------------------------------------------------------------------------------------

(defun find-tag-definition (tag-symbol)
  "Given the symbol for a tag definition, locate that definition in any
   available tag library"
  ;; we check *tag-library* first, in case it's in the current library--
  ;; which may also be the library under construction (and thus possibly
  ;; not yet in *available-tag-libraries*
  (or (when *tag-library* (gethash tag-symbol (tag-library-tags *tag-library*)))
      (loop for library being the hash-values of *available-tag-libraries*
	 ;; TODO we're using tag-library-tags, defined in taglibraries.lisp,
	 ;; so that's a forward reference
	 for definition = (gethash tag-symbol (tag-library-tags library))
	 when definition return definition
	 finally (return nil))))

(defun find-next-tag-method (definition method-name &optional (visited-definitions nil))
  (or
   (loop for base in (tag-bases definition)
      for base-definition = (find-tag-definition base)
      for base-method = (when base-definition 
			  (find-tag-method base-definition 
					   method-name
					   (cons definition visited-definitions)))
      when base-method return base-method
      finally (return nil))
   (slot-value *default-tag-definition* method-name)))

(defun find-tag-method (definition method-name &optional (visited-definitions nil))
  (or
   (slot-value definition method-name)
   (find-next-tag-method definition method-name visited-definitions)))

(defmethod tag-name ((*current-tag* htmltag))
  (tag-name (tag-definition *current-tag*)))

(defmethod tag-library-name-for-tag ( (*current-tag* htmltag) )
  (intern (package-name (symbol-package (tag-symbol (tag-definition *current-tag*)))) 'keyword))

(defun default-tag-attributes ()
  "Return a list of default attributes common to all tags"
  `(id role class style))

(defmethod tag-attributes ( (*current-tag* htmltag) )
  ;; TODO add attributes for bases
  (tag-attributes (tag-definition *current-tag*)))

(defun find-tag-attributes (tag-symbol)
  (let ((attributes (default-tag-attributes))
	(visited-symbols ()))
    (labels ((find-tag-symbol-attributes (a-symbol)
	       (unless (member a-symbol visited-symbols)
		 (let ((definition (find-tag-definition a-symbol)))
		   (unless definition (error "No tag definition found for ~s" a-symbol))
		   (loop for attribute in (tag-attributes definition)
		      ;; TODO note that using string= is slightly out of sync with
		      ;; with the actual hash test in tag-attribute-map--that also
		      ;; does a string-downcase; this should unless work unless a symbol
		      ;; is actually of mixed case
		      do (pushnew attribute attributes :key #'symbol-name :test #'string=))
		   (pushnew a-symbol visited-symbols)
		   (loop for base in (tag-bases definition)
		      do (find-tag-symbol-attributes base))))))
      (find-tag-symbol-attributes tag-symbol))
    attributes))

(defun find-all-tag-attributes (tag-attributes bases)
  (let ((attributes (default-tag-attributes))
	(visited-symbols ()))
    (labels ((push-attribute (attribute)
	       (pushnew attribute attributes :key #'symbol-name :test #'string=))
	     (find-tag-symbol-attributes (a-symbol)
	       (unless (member a-symbol visited-symbols)
		 (let ((definition (find-tag-definition a-symbol)))
		   (unless definition (error "No tag definition found for ~s" a-symbol))
		   (loop for attribute in (tag-attributes definition)
		      ;; TODO note that using string= is slightly out of sync with
		      ;; with the actual hash test in tag-attribute-map--that also
		      ;; does a string-downcase; this should unless work unless a symbol
		      ;; is actually of mixed case
		      ;; do (pushnew attribute attributes :key #'symbol-name :test #'string=)
		      do (push-attribute attribute))
		   (pushnew a-symbol visited-symbols)
		   (loop for base in (tag-bases definition)
		      do (find-tag-symbol-attributes base))))))
      (loop for base in bases
	 do (find-tag-symbol-attributes base))
      (loop for attribute in tag-attributes
	 do (push-attribute attribute)))
    attributes))

(defmethod tag-attribute-value ((*current-tag* htmltag) ( attribute symbol))
  (tag-attribute-value *current-tag* (symbol-name attribute)))

(defmethod tag-attribute-value ((*current-tag* htmltag) ( attribute string))
  (gethash (string-downcase attribute) (tag-attribute-map *current-tag*)))

(defmethod (setf tag-attribute-value) (value (*current-tag* htmltag) (attribute symbol))
    (setf (tag-attribute-value *current-tag* (symbol-name attribute)) value))

(defmethod (setf tag-attribute-value) (value (*current-tag* htmltag) (attribute string))
    (setf (gethash (string-downcase attribute) (tag-attribute-map *current-tag*)) value))

(defmethod render-tag-scripts ((some-tag-name symbol))
  nil)

(defmethod render-tag-scripts ((*current-tag* htmltag))
    (let ((tag-method (find-tag-method (tag-definition *current-tag*) 'scripts)))
      (when tag-method
	(funcall tag-method *current-tag*))))

(defmethod render-tag-ready-scripts((some-tag-name symbol))
    nil)

(defmethod render-tag-ready-scripts((*current-tag* htmltag))
  (let ((tag-method (find-tag-method (tag-definition *current-tag*) 'ready-scripts)))
    (when tag-method
      (funcall tag-method *current-tag*))))

(defmethod render-tag-script-libraries((some-tag-name symbol))
    nil)

(defmethod render-tag-script-libraries ((*current-tag* htmltag))
  (let ((tag-method (find-tag-method (tag-definition *current-tag*) 'script-libraries)))
    (when tag-method
      (funcall tag-method *current-tag*))))

(defmethod render-tag-styles ((some-tag-name symbol))
    nil)

(defmethod render-tag-styles ((*current-tag* htmltag))
  (let ((tag-method (find-tag-method (tag-definition *current-tag*) 'styles)))
    (when tag-method
      (funcall tag-method *current-tag*))))

(defmethod render-tag-style-sheets ((some-tag-name symbol))
    nil)

(defmethod render-tag-style-sheets ((*current-tag* htmltag))
  (let ((tag-method (find-tag-method (tag-definition *current-tag*) 'style-sheets)))
    (when tag-method
      (funcall tag-method *current-tag*))))

(defmacro initialize-tag (tag-var &rest attribute-key-values)
  `(progn
     ,@(loop for attribute in attribute-key-values by #'cddr
	for value in (cdr attribute-key-values) by #'cddr
	collect `(setf (tag-attribute-value ,tag-var ',attribute) ,value))
     (let ((init-method (find-tag-method (tag-definition *current-tag*) 'init)))
       (when init-method
	 (funcall init-method *current-tag*)))))

(defmethod tag-expansion ( (name symbol) )
  `(,name (&rest content)
	  (let ((name (quote ,name)))
	    (multiple-value-bind (initializer body) (tag-separate-initializer-and-body (quote ,name) content)
	      `(let ((*current-tag* (make-instance 'htmltag :definition (find-tag-definition ',name))))
		 (initialize-tag *current-tag* ,@initializer)
		 (setf (body-of *current-tag*) (list ,@body)) *current-tag*)))))

(defmethod tag-has-body-p ( (some-tag-symbol symbol) )
  t)

(defmethod tag-has-end-tag-p ( (*current-tag* htmltag) )
  t)

(defmethod tag-separate-initializer-and-body ( (some-tag-symbol symbol) raw-body)
  (if (tag-has-body-p some-tag-symbol)
      (separate-initializer-and-body raw-body)
      (values raw-body nil)))

(defmethod render-as-html ( (*current-tag* htmltag) )
  (render-tag-script-libraries *current-tag* )
  (render-tag-scripts *current-tag* )
  (render-tag-styles *current-tag* )
  (render-tag-style-sheets *current-tag* )
  (render-tag-ready-scripts *current-tag* )
  (render-tag-content *current-tag* ))

(defmethod render-as-html :around (tag)
  (handler-case 
      (call-next-method)
    (error (e) 
      (let ((*print-escape* nil))
	(print (type-of e) *html-out*)
	(print-object e *html-out*)))))

(defmethod render-as-html ( (some-expr string))  
  (hout "~a" 
	(with-output-to-string (os)
	  (with-input-from-string (is some-expr)
	    (loop for c = (read-char is nil nil)
	       while c
	       do (cond ((and (equal c #\$)
			      (peek-char nil is nil nil))
			 (if (equal #\$ (peek-char nil is nil nil))
			     (progn 
			       (write-char c os)
			       ;; consume the next character
			       (read-char is nil nil))
			     (format os "~a " (eval `(html ,(with-tag-reader () (read is nil nil)))))))
			(t  
			 (write-char c os))))))))

(defmethod render-as-html ( (some-expr t) )
  (hout "~a" some-expr))

(defmethod render-as-html ( (some-list list) )
  (dolist (item some-list)
    (render-as-html item)))

(defun tag-indentation-string ()
  (make-array (* 2 *tag-printing-level*) :element-type 'character :initial-element #\Space))

(defmethod render-tag-start ( (*current-tag* htmltag) )
  (let ((name (tag-name *current-tag* ) ))
    ;; here's one place we manage indentation
    (hout "~%~a<~a" 
	  (tag-indentation-string)
	  (string-downcase name))
    (render-tag-attributes *current-tag*)
    (unless (tag-has-end-tag-p *current-tag*)
      (format *html-out* "/"))
    (format *html-out* ">")))

(defmethod render-tag-attributes ( (*current-tag* htmltag) )
  (hout "~:{ ~a='~a'~}" 
	(loop for attribute in (find-tag-attributes (tag-symbol (tag-definition *current-tag*)))
	     for value = (tag-attribute-value *current-tag* attribute)
	     when value 
	   collect (list (string-downcase (symbol-name attribute)) value))))

(defmethod render-tag-body ( (*current-tag* htmltag) )
  (dolist (some-expr (body-of *current-tag*) )
    (render-as-html some-expr)))


(defmethod render-tag-end ( (*current-tag* htmltag) )
  (let ((name (tag-name *current-tag* ) ))
    ;; and here's another place we manage indentation
    (hout "~%~a</~a>" 
	  (tag-indentation-string)
	  (string-downcase name) )))

(defmethod render-tag-content ( (*current-tag* htmltag) )
  (let ((tag-method (find-tag-method (tag-definition *current-tag*) 'content)))
    (when tag-method
      (funcall tag-method *current-tag*))))

;;;------------------------------------------------------------------------------------
;;; Functions
;;;------------------------------------------------------------------------------------

(defun separate-initializer-and-body (raw-body)
  (let ((body (loop for b in raw-body
		 unless (and (listp b) (equal '+@ (car b)))
		 collect b))
	(initializer (cdr (car (loop for b in raw-body
				  when (and (listp b) (equal '+@ (car b)))
				  collect b)))))
    (values initializer body)))
    
 (defun add-attribute (library-symbol tag-symbol attribute)
    "Pushes an additional attribute to the tag definition. Sample would be 
    (add-attribute :HTML 'HTML:DIV HTML::NG-APP"
    (push attribute (tag-attributes 
                       (gethash tag-symbol 
                                (tag-library-tags (tag-library library-symbol))))))   

;;;------------------------------------------------------------------------------------
;;; Macros
;;;------------------------------------------------------------------------------------

(defmacro expansion (expr)
  `(expand-expression-with-libraries (quote ,expr) *available-tag-libraries*))

(defmacro tags (expr)
  (expand-expression-with-libraries expr *available-tag-libraries*))

(defmacro +title (expr)
  `(progn
     (setf *page-title* ,expr)
     nil))

(defmacro +language (expr)
  `(progn
     (setf *page-language* ,expr)
     nil))

(defmacro +doctype (expr)
  `(progn
     (setf *page-doctype* ,expr)
     nil))

(defmacro +charset (expr)
  `(progn
     (setf *page-charset* ,expr)
     nil))

(defmacro +link (&key
		 ((:rel rel) nil)
		 ((:type type) nil)
		 ((:href href) nil))
  `(progn
     (pushnew (list ,rel ,type ,href)
	      *page-links*
	      :test 'equal)
     nil))

(defmacro putend (some-object some-place)
  "Append some-object to the end of the list in some-place.
   Modifies some-place.
   "
  `(setf ,some-place
         (nconc ,some-place
                 (list ,some-object))))

(defmacro putendnew (some-object some-place
		     &key 
		     ((:test test) 'equal))
  `(if (not (member ,some-object ,some-place :test ,test) )
       (putend ,some-object ,some-place)))

(defmacro union-ordered (some-list some-place)
  `(dolist (item ,some-list)
     (putendnew item
		,some-place
		:test 'equal)))

(setf *default-tag-definition* 
      (make-instance 'tag-definition 
		     :init (lambda (*current-tag*) )
		     :has-body (lambda (*current-tag*) t)
		     :has-end-tag (lambda (*current-tag*) t)
		     :scripts (lambda (*current-tag*) "")
		     :ready-scripts (lambda (*current-tag*) "")
		     :script-libraries (lambda (*current-tag*) ())
		     :styles (lambda (*current-tag*) ())
		     :style-sheets (lambda (*current-tag*) ())
		     :content (lambda (*current-tag*)	
				(render-tag-start *current-tag* )
				(let ((*tag-printing-level* (1+ *tag-printing-level*)))
				  (render-tag-body *current-tag* ))
				(if (tag-has-end-tag-p *current-tag*)
				    (render-tag-end *current-tag* )))))

(defmacro defhtmltag (name
		      &key
		      ((:tag tag) nil)
		      ((:bases bases) nil) ; other base classes here
		      ((:attributes attributes) nil)
		      ((:init init) nil)
		      ((:hasbody hasbody) t)
		      ((:noendtag noendtag) nil)
		      ((:content content) nil)
		      ((:script script) nil)
		      ((:ready ready) nil)
		      ((:style style) nil)
		      ((:style-sheets style-sheets) nil)
		      ((:script-libraries script-libraries) nil))
  "Define a new tag renderable as HTML."
  (declare 
   (type (or null list) style-sheets script-libraries))
  (let ((all-attributes (let ((unique-attributes ()))
			  (loop for attribute in (append (default-tag-attributes)
							 attributes
							 (loop for base in bases
							    append (tag-attributes (find-tag-definition base))))
			     do (pushnew attribute unique-attributes :key #'symbol-name :test #'string=))
			  unique-attributes)))
    `(let ((definition (make-instance 'tag-definition :name ',name)))
       	 (setf (tag-name definition) 
	       ,(if tag
		    tag	    
		    `(quote ,name)))

	 (setf (tag-symbol definition) ',name)

	 (add-tag-to-library definition *tag-library*)

	 (export (list
		  (quote ,name)))

	 (setf (tag-bases definition) ',bases)

	 ;; (setf (tag-attributes definition) ',attributes)
	 ;; TODO consider using the default attributes in the tag-attributes method,
	 ;; as this will create a copy of the defaults in every definition in an inheritance
	 ;; chain--not huge, as its just references to symbols, but nonetheless, does not adequately
	 ;; implement the intent
	 (setf (tag-attributes definition) ',attributes)

       (macrolet ((with-tag-attributes (() &rest body)
		    (let ((attributes (find-all-tag-attributes ',attributes ',bases)))
		      ;; Okay, challenge here is that this macro is expanded *before* the current tag library is assigned
		      ;; to *tag-library; so...we need to collect the attributes from the tag's bases, add in
		      ;; any defined here in the call to the macro, and then should be good.
		      `(symbol-macrolet (,@(loop for attribute in attributes 
					      collect `(,attribute (tag-attribute-value *this-tag* ',attribute))))
			 (with-slots (_body) *this-tag*
			   ,@body))))
		  (define-tag-method ((method-name) &rest body)
		    `(lambda (*current-tag*)
		       (flet ((call-next-tag-method (&optional (*current-tag* *current-tag*))
				(let ((next-tag-method (find-next-tag-method definition ',method-name)))
				  (when next-tag-method
				    ;; TODO consider whether it's an error if there is no
				    ;; tag method found...at the moment we just silently
				    ;; evaluate to nil
				    (funcall next-tag-method *current-tag*)))))
			 (let* ((*this-tag* *current-tag*) 
				(*tag-library* (gethash (tag-library-name-for-tag *this-tag*) 
							*available-tag-libraries*))
				;; TODO this part is a bit of a forward reference, as we're
				;; using an accessor on a tag library object
				(*package* (tag-library-package *tag-library*)))
			   ;; render the content expression instead of the usual content rendering
			   (with-tag-attributes () 
			     ,@body))))))

	 ,(when init
		`(setf (tag-init definition) 
		       (define-tag-method (init)
			   ,init)))

	 ,(when hasbody
		`(setf (tag-has-body-p definition)
		       (define-tag-method (has-body)
			   ,hasbody)))

	 ,(when noendtag 
		`(setf (tag-has-end-tag-p definition)
		       (define-tag-method (has-end-tag)
			   nil)))

	 ,(when script
		`(setf (tag-scripts definition)
		       (define-tag-method (scripts)
			   (union-ordered (list (format nil "/* --------- Script for tag ~a:~a ------------- */~%"
							(tag-library-name-for-tag *current-tag*)
							(tag-name *current-tag*))
						,script)
					  *page-scripts* ))))

	 ,(when ready
		`(setf (tag-ready-scripts definition)
		       (define-tag-method (ready-scripts)
			   (union-ordered (list (format nil "/* --------- Initialization for tag ~a:~a ------------- */~%"
							(tag-library-name-for-tag *current-tag*)
							(tag-name *current-tag*))
						,ready)
					  *page-ready-scripts*))))

	 ,(when script-libraries
		`(setf (tag-script-libraries definition)
		       (define-tag-method (script-libraries)
			   (union-ordered ,script-libraries *page-script-libraries*))))

	 ,(when style
		`(setf (tag-styles definition)
		       (define-tag-method (styles)
			   (union-ordered (list (format nil "/* --------- Styles for tag ~a:~a ------------- */~%"
							(tag-library-name-for-tag *current-tag*)
							(tag-name *current-tag*))
						,style)
					  *page-styles*))))

	 ,(when style-sheets
		`(setf (tag-style-sheets definition)
		       (define-tag-method (style-sheets)
			   (union-ordered ,style-sheets *page-style-sheets*))))

	 ,(when content
		`(setf (tag-content definition)
		       (define-tag-method (content)
			   (render-as-html (tags ,content) ))))))))


(defmacro defentity (name text &optional documentation)
  "Defines an entity usable in HTML"
  `(progn
     ,(if documentation
	  `(defvar ,name ,text ,documentation)
	  `(defvar ,name ,text))
     (export (list (quote ,name)))))

(defmacro html (&rest body)
  "Interpret the body as html markup, return multiple values, the first of which is an HTML string. The full list of return values is:

   $(ul
     (li \"The intended title of the page containing this HTML fragment\")
     (li \"Link elements that should appear in the head of the page containing this fragment\")
     (li \"Style sheets required by the HTML fragment\")
     (li \"Script libraries required by the HTML fragment\")
     (li \"Sections of Javascript code required by the HTML fragment\")
     (li \"Sections of Javascript code to be run on page initialization that are required by the HTML fragment\")
     )

  Any of these values may be null (including the fragment itself).  Care should be taken when nesting calls to this macro, as inline
  expansion of $$ characters will occur in each invocation of it, and unexpected results may occur.
   "
  `(let ((*page-title* ())
	 (*page-links* ())
	 (*page-doctype* ())
	 (*page-charset* ())
	 (*page-language* ())
	 (*page-style-sheets* ())
	 (*page-styles* ())
	 (*page-script-libraries* ())
	 (*page-scripts* ())
	 (*page-ready-scripts* ()))
     (let ((content (with-output-to-string (*html-out*)
		      (render-as-html (tags ,@body) ))))
       (values content *page-title* *page-links* *page-style-sheets* *page-styles* *page-script-libraries* *page-scripts* *page-ready-scripts*))))

(defmacro page (&rest raw-body)
  "Interpret the raw body as html markup, and return a complete HTML page.  In combination with $(a {:href \"_macro_html\"} \"html\"), this macro weaves
   the output of calls to $(em \"html\") into a complete page, with styles, scripts, references to script libraries, a page title, etc., all arranged
   in the appropriate order."
  `(multiple-value-bind (page-content *page-title* *page-links* *page-doctype* *page-charset* *page-language* *page-style-sheets* *page-styles* *page-script-libraries* *page-scripts* *page-ready-scripts*) 
       (html-for-user-agent ,@raw-body)
     ;; now render the page
     (if page-content ;; in case nothing suitable for the desired user agent
         (values (with-output-to-string (*html-out*)
                   (if *page-doctype*
                     (hout "<!DOCTYPE ~a>~%" *page-doctype*)
                     (hout "<!DOCTYPE html>~%"))
                   (if *page-language*
                     (hout "<html lang=\"~a\">~%" *page-language*)
                     (hout "<html lang=\"en\">~%"))
                   (hout "<head>~%")
                   (if *page-charset*
                     (hout "<meta charset=\"~a\">~%" *page-charset*)
                     (hout "<meta charset=\"utf-8\">~%"))
                   (hout "<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">")
                   (hout "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, maximum-scale=1\">")
                   (when *page-title*
                     (hout "<title>~a</title>~%" *page-title*))

                   (when *page-links*
                     (dolist (link *page-links*)
                       (destructuring-bind (rel type href) link
                         (hout "<link~@[ rel='~a'~]~@[ type='~a'~]~@[ href='~a'~]/>~%"
                               rel type href))))

                   (when *page-style-sheets*
                     (dolist (style-sheet *page-style-sheets*)
                       (hout "<link type='text/css' href='~a' rel='stylesheet' />~%"
                             style-sheet )))
                   
                   (when *page-styles*
                     (hout "<style type='text/css'>~%~%~{~a~%~}~%</style>~%"
                           *page-styles*))

                   (hout "</head>~%")

                   (hout "<body>~%")

                   (hout "~a~%" page-content)

                   (when *page-script-libraries*
                     (dolist (library *page-script-libraries*)
                       (hout "<script type='text/javascript' src='~a'></script>~%"
                             library)))
                   
                   (when (and *page-scripts*
                              (not (equal *page-scripts* "")))
                     (hout "<script type='text/javascript'>~%~%~{~a~%~}~%</script>~%"
                           *page-scripts*))

                   (when (and *page-ready-scripts*
                              (not (equal *page-ready-scripts* "")))
                     (progn
                       (hout "<script type='text/javascript'>~%~%")
                       (hout "$(function(){~%~{~a~%~}~%~%"
                             *page-ready-scripts*)
                       (hout "~%});")
                       (hout "~%</script>~%")))

                   (hout "</body>~%")

                   (hout "</html>~%"))
                 "text/html")
         (values nil nil))))


;;;------------------------------------------------------------------------------------
;;; Printing
;;;------------------------------------------------------------------------------------

(defmethod print-object ((object htmltag) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "Name=~s Body=~s" (tag-name object) (slot-value object '_body))))

(defmethod print-object ((object tag-definition) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "Symbol=~s" (tag-symbol object) )))
