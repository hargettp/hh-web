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

  'defurl
  'defurls
  'create-package-folder-dispatcher-and-handler
  'reset-urls
  'flush-url-category
  'dispatch-url-cache

  )
 )

;;;------------------------------------------------------------------------------------
;;; Classes + types
;;;------------------------------------------------------------------------------------

(defclass url-cache-provider ()
  ((patterns :initform () :accessor patterns)))

(defclass url-cache-item ()
  ((path :initarg :path :accessor cache-key)
   (test :initarg :test :accessor test)
   (modified-time :initarg :modified :accessor modified-time-of)
   (content :initform (make-hash-table) :initarg :content :accessor content)
   (generator :initarg :generator :accessor generator)
   (categories :initform () :initarg :categories :accessor categories 
	       :documentation "List of symbols identifying categories to which the content belongs")
   ;; set handler once, so does not need to be created each time
   (handler :accessor handler) 
   ;; check for whether item has expired or not
   (expiry :initarg :expiry :accessor expiry)) 
  (:documentation "Represents an cache entry containing the generated content for a specific URL"))

;;;------------------------------------------------------------------------------------
;;; Dynamic variables
;;;------------------------------------------------------------------------------------

(defvar *path-pattern-dispatchers* ()
  "A list of path pattern dispatcher, created by defurl.  Each
  handler tests the path against a regex, and returns a handler if the match
  succeeds, nil otherwise.")

(defvar *url-cache* (make-instance 'cache :provider (make-instance 'url-cache-provider)) )

;;;------------------------------------------------------------------------------------
;;; Package-based folder dispatcher
;;;------------------------------------------------------------------------------------

(defun create-package-folder-dispatcher-and-handler (uri package-name)
  "Create a dispatcher for all files inside a www subfolder of a package's directory"
  (lambda (hunchentoot:*request*)
    (use-backtrace-logging 
     (let ((path (hunchentoot:script-name hunchentoot:*request*) )
	   (package (asdf:find-system package-name) ))
       (if (string-starts-with path uri)
	   ;; paths match, now check if a file exists
	   (let* ((relative-path (subseq path (length uri)) )
		  (file-path (asdf:system-relative-pathname package 
							    ;; TODO yes, we should be using merge-pathnames here
							    (format nil "www/~a" relative-path))))
	     (if (and (not (cl-fad:directory-pathname-p file-path)  )
			   (probe-file file-path))
		 ;; file exists and path is not for a directory, return handler
		 (lambda ()
		   (hunchentoot:handle-static-file file-path))
		 ;; no file at the expected location--move on
		 nil))
	   ;; no match for path; another dispatcher should handle it
	   nil)))))

;;;------------------------------------------------------------------------------------
;;; URL pattern handlers
;;;------------------------------------------------------------------------------------

(defun cacheable-request-p (hunchentoot:*request*)
  "If a request has any query parameters, it's not cachable"
  (not (or (hunchentoot:get-parameters* hunchentoot:*request*)
	   (hunchentoot:post-parameters* hunchentoot:*request*)
	   )
       )
  )

(defmacro parameter-name-p (register)
  `(and ,register 
	(not (string-starts-with ,register "&" ) )))

(defun url-pattern-parameter-names (named-registers)
  (when named-registers
    (let ((register (car named-registers) )
	  (remaining-registers (cdr named-registers) ))
      (if (parameter-name-p register)
	  (cons (intern (string-upcase register))
		(url-pattern-parameter-names remaining-registers))
	  (url-pattern-parameter-names remaining-registers )))))

(defun url-pattern-parameter-values (path named-registers starts ends &optional (parameter-index 0) )
  (when named-registers
    (let ((register (car named-registers) )
	  (start (elt starts parameter-index) )
	  (end (elt ends parameter-index) )
	  (remaining-registers (cdr named-registers) ))
      (if (and (parameter-name-p register)
	       (numberp start)
	       (numberp end))
	  (cons (when (> end start) 
		  (subseq path start end))
		(url-pattern-parameter-values path 
					      remaining-registers 
					      starts 
					      ends
					      (+ parameter-index 1)))
	  (url-pattern-parameter-values path 
					remaining-registers 
					starts 
					ends
					(+ parameter-index 1))))))

(defun url-pattern-parameters (hunchentoot:*request* pattern)
  "Returns 3 values: a list of parameter name, a list of parameter values, and either t or nil to indicate that a match was successful"
  (multiple-value-bind (scanner named-registers) 
      (let ( (*allow-named-registers* t) )
	(create-scanner pattern :case-insensitive-mode nil) )
    (let* ((path (hunchentoot:script-name hunchentoot:*request*) ))
      (multiple-value-bind (match-start match-end reg-starts reg-ends)
	  (handler-case  (scan scanner path)
	    (end-of-file ()  nil ))
	(declare (ignorable match-start match-end) )
	(if match-start ; proxy for checking that there was a match
	    (let ((parameter-values (url-pattern-parameter-values path
									   named-registers
									   reg-starts
									   reg-ends))
		  (parameter-names (url-pattern-parameter-names named-registers) ))
	      (log5:log-for (log5:info) "Extracted values ~a~%" parameter-values)
	      ;; we "optionalize" the last parameter, for simplicity
	      (values (when parameter-names (concatenate 'list 
							 (butlast parameter-names)
							 (list `&optional)
							 (last parameter-names)))
		      parameter-values 
		      t))
	    (progn
	      (log5:log-for (log5:warn) "No match found for ~s in ~s~%" pattern path)
	      (values nil nil nil)))))))

(defun make-url-pattern-function-form (parameter-names parameter-values extras body)
  `(lambda (hunchentoot:*request* ,@extras)
     (declare (ignorable hunchentoot:*request*))
     (use-backtrace-logging
      (funcall (lambda (,@parameter-names)
		 ,body)
	       ,@parameter-values))))

(defun make-url-pattern (pattern dispatch-test handler categories expiry)
  `(lambda (hunchentoot:*request*) ;newly added dispatcher
     (use-backtrace-logging 
      (multiple-value-bind (parameter-names parameter-values match)
	  (let ( (*package* ,*package*) ) ;; we're doing this odd move to make sure parameter names are in correct package
	    (url-pattern-parameters hunchentoot:*request* ,pattern) )
	(when match
	  (let* ((pattern ,pattern)
		 (dispatch-test (quote ,dispatch-test))
		 (handler (quote ,handler) )
		 (expiry (quote ,expiry) )
		 (path (hunchentoot:script-name hunchentoot:*request*))
		 (test-form (make-url-pattern-function-form parameter-names parameter-values nil dispatch-test) )
		 (test (eval test-form) )
		 (generator-form (make-url-pattern-function-form parameter-names parameter-values nil handler) )
		 ;; ignore any expiring information and expire right away if we have parameters
		 (expiry-form (if (cacheable-request-p hunchentoot:*request*)
				  `(lambda (cache-item)
				     ,expiry)
				  ;; not cachable--set up as immediately expired
				  `(lambda (cache-item)
				     (declare (ignorable cache-item))
				     t))))
	    (when (funcall test hunchentoot:*request*) ;; we check the dispatch test
	      (log5:log-for (html-caching) "Expiry form for ~a is ~a~%" (hunchentoot:script-name hunchentoot:*request*) expiry-form)
	      (let ((item (make-instance 'url-cache-item 
					 :path path
					 :modified (now)
					 :test test
					 :generator (eval generator-form)
					 :categories (quote ,categories)
					 :expiry (eval expiry-form))))
		(setf (handler item) (lambda () (handle-cached-item item)) )
		item))))))))

(defmacro defurl (pattern 
		  &key
		  ((:if dispatch-test) t)
		  ((:handler handler) nil)
		  ((:categories categories) nil)
		  ((:until cache-until) nil) ;; using both :until and :while would be unusual, but not allowed
		  ((:while cache-while) nil))
  (let ((expiry (cond ( (numberp cache-until)
		       `(timestamp> (now) (timestamp+ (modified-time-of cache-item) ,cache-until :sec)))
		      ( (numberp cache-while)
		       `(timestamp> (now) (timestamp+ (modified-time-of cache-item) ,cache-while :sec)))
		      ( cache-until
		       cache-until)
		      ( cache-while
		       `(not ,cache-while))
		      (t
		       ;; by default, cache items expire essentially right away
		       t))))
    `(setf (patterns (cache-provider *url-cache*))
	   (append (patterns (cache-provider *url-cache*))
		   (list
		    ,(make-url-pattern pattern 
				       dispatch-test 
				       handler 
				       categories 
				       expiry))))))

(defmethod load-cache-item-from-provider ( (provider url-cache-provider) hunchentoot:*request*)
  (when (patterns (cache-provider *url-cache*))
    (let ((item (loop for pattern in (patterns (cache-provider *url-cache*))
		      for handler = (funcall pattern hunchentoot:*request*)
		      if handler return handler)))
      (when item
	  (progn
	    (log5:log-for (log5:trace) "Dispatching path ~s~%" (hunchentoot:script-name hunchentoot:*request*) )
	    item)))))

(defmethod cached-item-expiredp ( cache (cache-item url-cache-item) )
  (funcall (expiry cache-item) cache-item))

(defmethod cached-item-invalidp ( cache (item url-cache-item) request)
  "It's invalid if the test fails, or it has parameters "
  (or (not (cacheable-request-p request) )
      (not (funcall (test item) request))))

(defun content-for-user-agent ( cached-item )
  (loop for *user-agent* in (detect-user-agents)
     do (progn
	  (log5:log-for (html-caching) "Looking up : content for user agent ~s at ~a" *user-agent* (cache-key hunchentoot:*request*))
	  (let ((cached-content (gethash *user-agent* (content cached-item))))
	    (if cached-content
		(progn
		  (log5:log-for (html-caching) "Found : content for user agent ~s at ~a" *user-agent* (cache-key hunchentoot:*request*))
		  (return (values-list cached-content)))
		(multiple-value-bind ( generated-content generated-content-type) (funcall (generator cached-item) hunchentoot:*request*)
		  (when generated-content
		    (progn
		      (log5:log-for (html-caching) "Generated : content for user agent ~s at ~a" *user-agent* (cache-key  hunchentoot:*request*))
		      (setf (gethash *user-agent* (content cached-item)) (list generated-content generated-content-type))
		      (return (values generated-content generated-content-type))))))))))

(defun handle-cached-item ( cached-item )
  (unless cached-item
    (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
    (hunchentoot:abort-request-handler))
  (let ((time (timestamp-to-universal (modified-time-of cached-item)) ))
    (multiple-value-bind (content content-type) (content-for-user-agent cached-item) 
      (when content-type
	(setf (hunchentoot:content-type*) content-type ))
      (hunchentoot:handle-if-modified-since time )
      content)))

(defun flush-url-category (category)
  (log5:log-for (html-caching) "Flushing category ~a~%" category)
  (let ((flushing-urls (loop for url being the hash-key in (entries *url-cache*) 
		      for cache-item being the hash-value in (entries *url-cache*)
		      if (member category (categories cache-item))
		      collect url)))
    (loop for url in flushing-urls
       do (progn
	    (log5:log-for (html-caching) "Flushing ~a for category ~a~%" url category)
	    (remhash url (entries *url-cache*) )))
    flushing-urls))

(defun dispatch-url-cache (hunchentoot:*request*)
  (use-backtrace-logging 
   (let ((cached-item (get-cached-item *url-cache* hunchentoot:*request*) ))
     (when cached-item
       ;; for any localized resources
       (let ((*localization-context* (hunchentoot:script-name hunchentoot:*request*)))
	 ;; item exists
	 ;; return the handler lambda--already created
	 (handler cached-item)) ))))

(defmacro reset-urls ()
  "Clear out list of dispatchers; useful to ensures that stale patterns do not survive reload"
  `(setf (patterns (cache-provider *url-cache*)) nil))

(defmacro defurls (&rest urldefs)
  `(progn
     (reset-urls)
     ,@(mapcar (lambda (def)
		 (cons 'defurl def))
	       urldefs)))
