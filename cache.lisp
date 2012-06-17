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

  'get-cached-item
  'make-fs-cache
  'make-package-fs-cache
  'create-file-cache-dispatcher-and-handler
  'create-file-cache-dispatcher-and-handler-for-root
  'create-package-file-cache-dispatcher-and-handler

  )
)

;;;------------------------------------------------------------------------------------
;;; Dynamic variables
;;;------------------------------------------------------------------------------------

(defvar *minimum-cached-item-stale-time* 1
  "Minimum time in seconds for which a cache entry is not stale (and thus can't expire), but
   after which the item is stale and possible expired"
  )

;;;------------------------------------------------------------------------------------
;;; Types
;;;------------------------------------------------------------------------------------

(defclass cache ()
  (
   (entries
    :initform (make-hash-table :test 'equal)
    :accessor entries
    )
   (provider
    :initarg :provider
    :accessor cache-provider
    )
   (loading-enabled
    :initform t
    :accessor cache-loading-enabled
    )
   )
  )

;; --- Cache providers ---

(defclass cache-provider ()
  (
   )
  )

;; --- Filesystem cache provider ---

(defclass fs-cache-provider ()
  (
   (root
    :initarg :root
    :accessor fs-root
    )
   )
  )

(defclass fs-cache-item ()
  (
   (path
    :initarg :path
    :accessor cache-key
    )
   (modified-time
    :initarg :modified
    :accessor modified-time-of
    )
   (content-type
    :initarg :type
    :accessor content-type
    )
   (content
    :initarg :content
    :accessor content
    )
   )
  ) 

;;;------------------------------------------------------------------------------------
;;; Generics
;;;------------------------------------------------------------------------------------

;; --- Cache ---

(defgeneric get-cached-item ( cache request )
  (:documentation "Obtain a cached item, regardless of whether the item is in the cache
   or must be loaded from the provider")
  )

(defgeneric cache-provider-key ( cache-provider request)
  (:documentation "Compute or determine the key to use for the particular cache and request"))

(defgeneric disable-cache-loading ( cache )
  (:method ( (cache cache) )
    (setf (cache-loading-enabled cache) nil)
    )
  )

(defgeneric enable-cache-loading ( cache )
  (:method ( (cache cache) )
    (setf (cache-loading-enabled cache) t)
    )
  )

(defgeneric find-cached-item ( cache request )
  (:documentation "Find an item in the cache with the indicated key, or null if there is
    no item cached for that key or if the cached item has expired")
  )

(defgeneric cache-item ( cache request item)
  (:documentation "Force an item into the cache for the indicated key, removing
    any item previously cached for that key")
  )

(defgeneric cached-item-stalep ( cache item )
  (:documentation "If an item is not yet stale, then the cache does not check with the
  provider to determine if the item is expired (intended as a means of throttling
  checks by the provider, which may be expensive")
  )

(defgeneric load-and-cache-item ( cache request )
  (:documentation "Load an item with the indicated key into the cache")
  )

(defgeneric cache-key ( item-or-request )
  (:documentation "Return the key for the item (or request)")
  )

;; --- Cache items ---

(defgeneric modified-time-of ( item )
  (:documentation "Required method for items in a cache, to determine whether it
    has become stale or expired")
  )

;; --- Cache provider ---

(defgeneric cached-item-expiredp ( provider-or-cache item )
  (:documentation "Return non-nil if item should be refreshed from provider, rather than 
    reused from the cache")
  )

(defgeneric cached-item-invalidp ( provider-or-cache item request)
  (:documentation "Return non-nil if item is not valid (for example, because of
    a failed authorization check")
  )

(defgeneric load-cache-item-from-provider ( provider request)
  (:documentation "Load an item from the provider, given the indicated request")
  )


;; --- Filesystem cache provider ---
(defgeneric fs-cache-file-full-path (path provider)
  (:documentation "Return the full path of the indicated file")
  )

(defgeneric fs-cache-file-modified-time ( path provider)
  (:documentation "Obtain the file's modified time")
  )

;;;------------------------------------------------------------------------------------
;;; Implementation
;;;------------------------------------------------------------------------------------

(log5:defcategory html-caching)

(defmethod cache-key ( (key t) )
  key
  )

(defmethod cache-key ( (request hunchentoot:request) )
  (hunchentoot:script-name request)
  )

(defmethod get-cached-item ( (cache cache) request )
  (log5:log-for (html-caching) "Looking up: ~a~%" (cache-provider-key cache request))
  (or (find-cached-item cache request)
      (load-and-cache-item cache request)
      )
  )

(defmethod cache-provider-key ( (cache t) request)
  (declare (ignorable cache))
  (cache-key request))

(defmethod cache-provider-key ( (cache cache) request)
  (cache-provider-key (cache-provider cache) request))

(defmethod find-cached-item ( (cache cache) request )
  (let* (
	 (key (cache-provider-key cache request))
	 (item (gethash key (entries cache) nil) )
	 )
    (if item
	(if (or (cached-item-expiredp cache item) 
		(cached-item-invalidp cache item request)
		)
	    (progn
	      (remhash key (entries cache))
	      nil
	      )
	    (progn
	      (log5:log-for (html-caching) "Found: ~a~%" key)
	      item
	      )
	    )
	(progn
	  (log5:log-for (html-caching) "Missed: ~a~%" key)
	  nil
	  )
	)
    )
  )

(defmethod cached-item-stalep ( (cache cache) item )
  (timestamp> (now) 
	      (timestamp+ (modified-time-of item) *minimum-cached-item-stale-time* :sec)
	      )  
  )

(defmethod cached-item-expiredp ( (cache cache) item )
  ;; if not yet stale, then don't take expense of checking further for expiration
  (when (cached-item-stalep cache item)
    (cached-item-expiredp (cache-provider cache) item)
    )
  )

(defmethod load-and-cache-item ( (cache cache) request )
  (when (cache-loading-enabled cache)
    (let (
	  (item (load-cache-item-from-provider (cache-provider cache) request) )
	  )
      (when item
	(cache-item cache request item)
	(log5:log-for (html-caching) "Loaded: ~a~%" (cache-provider-key cache request))
	)
      item
      )
    )
  )

(defmethod cache-item ( (cache cache) request item)
  (let ((cache-key (cache-provider-key cache request)))
    (log5:log-for (html-caching) "Cached: ~s~%" cache-key)
    (setf (gethash cache-key (entries cache) ) item)))

;; --- Filesystem cache provider ---

(defmethod fs-cache-file-full-path (path (provider fs-cache-provider) )
  (let (
	(fs-root (fs-root provider) )
	)
    ;; folder-name should be an absolute path, ending in / to indicate directory
    (merge-pathnames (make-pathname :name (pathname-name path)
				    :type (pathname-type path)
				    :directory (pathname-directory path)
				    )
		     (make-pathname :host (pathname-host fs-root)
				    :directory (pathname-directory fs-root)
				    )
		     )
    )
  )

(defmethod fs-cache-file-modified-time ( path (provider fs-cache-provider) )
  (let ( 
	(full-path (fs-cache-file-full-path path provider)) 
	)
    (unix-to-timestamp (sb-posix:stat-mtime (sb-posix:stat full-path)) )
    )
  )

(defmethod cached-item-expiredp ( (provider fs-cache-provider) item )
  (timestamp>  (fs-cache-file-modified-time (cache-key item) provider)
	       (modified-time-of item)
	       )
  )

(defmethod cached-item-invalidp ( provider-or-cache item request)
  (declare (ignorable provider item request))
  nil
  )

(defmethod load-cache-item-from-provider ( (provider fs-cache-provider) request)
  (let* (
	 (path (cache-key request))
	 (full-path (fs-cache-file-full-path path provider) )
	 )
    (when (and (probe-file full-path)
	       (not (cl-fad:directory-pathname-p full-path ) )
	       )
      (let* (
	     (file-size (sb-posix:stat-size (sb-posix:stat full-path)) )
	     (content-buffer (make-array file-size :initial-element 0 :element-type `(unsigned-byte 8)) )
	     (item (make-instance 'fs-cache-item 
				  :path full-path
				  :modified (fs-cache-file-modified-time path provider)
				  :type (or (hunchentoot:mime-type full-path)
					    "application/octet-stream"
					    )
				  :content content-buffer
				  )
	       )
	     )
	(with-open-file (is full-path :element-type '(unsigned-byte 8) ) 
	  (read-sequence content-buffer is)
	  )    
	item
	)
      )
    )
  )

(defun make-fs-cache ( root &key (populate nil) (static nil) )
  "Create a cache with the indicated root, and if populate is non-nil,
   preload the cache with all files underneath the root"
  (let* (
	(fs-cache-provider (make-instance 'fs-cache-provider :root root) )
	(cache (make-instance 'cache :provider fs-cache-provider) )
	(path-start (length (namestring root)) )
	)
    (when (and populate (probe-file root))
      (cl-fad:walk-directory root
			     (lambda (f)
			       (load-and-cache-item cache 
						    (subseq (namestring f)
							    path-start
							    )
						    )
			       )
			     :directories nil
			     )
      )
    (when (and populate static)
      (disable-cache-loading cache)
      )
    cache
    )
  )

(defun make-package-fs-cache ( package &key (populate nil) (static nil) )
  "Create a filesystem cache for the www subdirectory of a package"
  (make-fs-cache (asdf:system-relative-pathname package 
						(make-pathname :directory `(:relative "www") )
						)
		 :populate populate
		 :static static
		 )
  )

(defun handle-cached-file ( cached-file )
  "Given a cached file, return the contents of the file in a hunchentoot reply"
  (unless cached-file
    (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
    (hunchentoot:abort-request-handler)
    )
  (let (
	(time (timestamp-to-universal (modified-time-of cached-file)) )
	)
    (setf (hunchentoot:content-type*) (content-type cached-file) )
    (hunchentoot:handle-if-modified-since time )
    (setf (hunchentoot:header-out :last-modified) (hunchentoot:rfc-1123-date time)
	  (hunchentoot:content-length*) (length (content cached-file))
	  )
    (let (
	  (out (hunchentoot:send-headers) )
	  )
      (write-sequence (content cached-file) out)
      (finish-output out)
      )
    )
  )

(defun create-file-cache-dispatcher-and-handler ( cache &optional (uri "/") )
  (lambda (hunchentoot:*request*)
    (use-backtrace-logging 
     (let (
	   (request-path (cache-key hunchentoot:*request*) )
	   )
       (when (string-starts-with request-path uri)
	 ;; paths match, now check if an item exists
	 (let* (
		(relative-request-path (subseq request-path (length uri)) )
		(cached-item (get-cached-item cache relative-request-path) )
		)
	   (when cached-item
	     ;; item exists
	     (lambda ()
	       (handle-cached-file cached-item)
	       )
	     ;; else no item served by this cache -- move on
	     )
	   )
	 ;; else no match for uri; another dispatcher should handle it
	 )
       )
     )
    )
  )

(defun create-file-cache-dispatcher-and-handler-for-root (root &key (uri "/") (populate nil) (static nil) )
  "Return a dispatcher suitable for use with Hunchentoot, dispatching URLs "
  (let (
	(cache (make-fs-cache root :populate populate :static static) )
	)
     (create-file-cache-dispatcher-and-handler cache uri)
    )
  )

(defun create-package-file-cache-dispatcher-and-handler (package &key (uri "/") (populate nil) (static nil) )
  (create-file-cache-dispatcher-and-handler-for-root (asdf:system-relative-pathname package
									   (make-pathname :directory `(:relative "www") )
									   ) 
					    :uri uri 
					    :populate populate
					    :static static
					    )
  )
