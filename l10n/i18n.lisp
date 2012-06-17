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

;; -----------------------------------------------------------------------------
;;
;; Classes + types
;;
;; -----------------------------------------------------------------------------

(defclass locale ()
  ((name :initarg :name :accessor name)
   (strings :initform (make-hash-table :test #'equal) :accessor strings)))

(defclass localizable-string ()
  ((name :initarg :name :accessor name 
	 :documentation "The string as it appeared in the development locale")
   (contexts :initform () :accessor contexts
	     :documentation "The contexts where the string appears; usually URLs (actually paths) 
                             to pages containing the string")
   (description :initform "" :initarg :description :accessor description
		:documentation "A description of the string's purpose and expected usage.
                                Likely this slot is unused in localized strings, but is
                                useful for strings in the development locale")
   (notes :initform "" :initarg :notes :accessor notes
	  :documentation "Notes from / to localizers about a string")))

(defclass string-localization ()
  ((name :initarg :name :accessor name 
	 :documentation "The string as it appeared in the development locale")
   (locale :initarg :locale :accessor locale
	   :documentation "The locale in which this specific string has been localized")
   (localization :initform nil :initarg :translation :accessor localization
		 :documentation "The localized representation of a string")
   (notes :initform "" :initarg :notes :accessor notes
	  :documentation "Notes from / to localizers about a string")))

;; -----------------------------------------------------------------------------
;;
;; Globals + constants
;;
;; -----------------------------------------------------------------------------

(defvar *localizable-strings* (make-hash-table :test #'equal))
(defvar *localization-context* nil 
  "Typically set to the URL in which the string occurs, but not always (and not always set
   implying no relevant context")

(defvar *development-locale-name* "en_US"
  "Locale in which development occurs, and in which strings in code
  first appear before translation into other locales")

(defvar *locale* nil "Current locale")

(defvar *locales* (make-hash-table :test #'equal) "List of loaded or known locales")

(defvar *locale-package* nil "Default package containing localization information (locales)")

;; -----------------------------------------------------------------------------
;;
;; Implementation
;;
;; -----------------------------------------------------------------------------

(defun add-localization-context (localizable-string &optional (*localization-context* *localization-context*))
  "Captures the context in which the string appears, if a relevant context is available"
  (when *localization-context* (pushnew context (contexts localizable-string) :test #'equal)))

(defun add-localizable-string (literal-string &optional (*locale* *locale*))
  (unless (gethash literal-string *localizable-strings*)
    (let ((localizable-string (make-instance 'localizable-string :name literal-string)))
      (setf (gethash literal-string *localizable-strings*)
	    localizable-string)))
  literal-string)

(defun add-string-localization (name localization notes &optional (*locale* *locale*))  
  (let ((localized-string (make-instance 'string-localization 
					 :name name 
					 :notes notes
					 :translation localization
					 :locale (name *locale*))))
    (setf (gethash name (strings *locale*)) localized-string)
    localized-string))

(defun gettext (literal-string &optional (*locale* *locale*))
  (let* ((string-localization (gethash literal-string (strings *locale*) nil)))
    (if string-localization
	(progn
	  (let ((localizable-string (gethash literal-string *localizable-strings* nil))
		(localization (localization string-localization)))
	    (when localizable-string (add-localization-context localizable-string))	 
	    (if localization
		(values (localization string-localization) t)
		(values literal-string nil))))
	(values literal-string nil))))

(defun get-localizable-strings (&optional (*locale* *locale*))
  (sort (loop for k being the hash-keys of *localizable-strings* collect k)
	#'(lambda (left right) 
	    (string<= (string-downcase left)
		      (string-downcase right)))))

(defun get-localization-contexts (literal-string)
  (let ((localizable-string (gethash literal-string *localizable-strings* nil)))
    (when localizable-string
      (contexts localizable-string))))

(defun get-package-locales-directory (package)
  (asdf:system-relative-pathname 
   (asdf:find-system (if (symbolp package) package (find-symbol package :keyword))) 
   (make-pathname :directory `(:relative "locales") )))

(defun get-package-locale-directory (package locale)
  (merge-pathnames (make-pathname :directory `(:relative ,locale))
		   (get-package-locales-directory package)))

(defun get-package-string-localization-file (package locale)
  (merge-pathnames (make-pathname :name "strings" :type "lisp" 
				  :defaults (get-package-locale-directory package locale))))

(defun load-string-localizations (package locale)
  (when (probe-file (get-package-string-localization-file package locale))
    (with-open-file (is (get-package-string-localization-file package locale) :external-format :utf-8)
      (let* ((*package* (find-package package))
	    (new-locale (eval (read is))))
	(setf (gethash (name new-locale) *locales*) new-locale)
	new-locale))))

(defun save-string-localizations (package locale)
  (ensure-directories-exist (get-package-locale-directory package locale))
  (with-open-file (os (get-package-string-localization-file package locale) 
		      :direction :output :external-format :utf-8 :if-exists :overwrite :if-does-not-exist :create)
    (write `(deflocale (,locale) 
		,@(let ((strings (strings (gethash locale *locales*))))
		       (loop for string being the hash-key of strings
			  collect (with-slots (name localization notes) (gethash string strings)
				    `(:string :name ,name :localization ,localization :notes ,notes)))))
	   :stream os)))

(defun load-available-package-locales (package)
  (let ((locale-wild (merge-pathnames (make-pathname :directory `(:relative :wild)) 
				      (get-package-locales-directory package))))
    (loop for locale-directory in (directory locale-wild)
	 do (let ((locale-name (car (last (pathname-directory locale-directory)))))
	      (load-string-localizations package locale-name)))))

(defun get-available-locales (&optional (*locales* *locales*))
  (loop for k being the hash-keys of *locales* collect k))

(defun create-package-locale (locale-package locale-name)
  (let ((strings-file (get-package-string-localization-file locale-package locale-name)))
	(ensure-directories-exist strings-file)
	(unless (probe-file strings-file)
	  (let ((new-locale (make-instance 'locale :name locale-name)))
	    (setf (gethash locale-name *locales*) new-locale)
	    (save-string-localizations locale-package locale-name)))
	(load-string-localizations locale-package locale-name)))

(defun delete-package-locale (locale-package locale-name )
  (let ((locale-directory (get-package-locale-directory locale-package locale-name)))
    (cl-fad:delete-directory-and-files locale-directory
				       :if-does-not-exist :ignore)))

(defmacro _ (literal-string)
  "Should record literal string for reference within the i18n subsystem, and by
   default return the appropriate string for the current locale"
  (add-localizable-string literal-string (gethash *development-locale-name* *locales*)) ;; because it's not localized yet
  `(gettext ,literal-string))

(defmacro deflocale ((locale) &rest resources)
  `(let ((*locale* (make-instance 'locale :name ,locale)))
     ,@(loop for resource in resources
	  collect (destructuring-bind (type &key name localization notes) resource
		    (cond ( (eql type :string)
			   `(add-string-localization ,name ,localization ,notes *locale*)))))
     *locale*))

(defun init-localization (locale-package development-locale-name)
  (setf *locale-package* locale-package)
  (setf *development-locale-name* development-locale-name)
  (load-available-package-locales *locale-package*)
  (unless (gethash *development-locale-name* *locales* nil)
    (setf (gethash *development-locale-name* *locales*) 
	  (make-instance 'locale :name *development-locale-name*))
    (save-string-localizations locale-package *development-locale-name*))
  (setf *locale* (gethash *development-locale-name* *locales*)))

(defun get-string-localization (string locale-name)
  (let ((locale (gethash locale-name *locales*)))
    (let ((localized-string (when locale (gethash string (strings locale) nil))))
      (when localized-string (localization localized-string)))))

(defun get-string-localization-notes (string locale-name)
  (let ((locale (gethash locale-name *locales*)))
    (let ((localized-string (when locale (gethash string (strings locale) nil))))
      (when localized-string (notes localized-string)))))

(defun get-localizable-string-notes (string)
  (get-string-localization-notes string *development-locale-name*))

(defun set-locale (locale-name)
  (let ((locale (gethash locale-name *locales*)))
    (when locale
      (setf *locale* locale))))
