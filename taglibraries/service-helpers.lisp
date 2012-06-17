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

(in-tag-library :service-helpers)

(use-tag-library :html)

(defhtmltag view-service-documentation
    :attributes ((service-name
		  :initarg :service-name
		  :accessor service-name))
    :content (let ((service-name (service-name *current-tag*) ))
	    (p
	     (service-documentation-string service-name ))))

(defhtmltag view-service-parameters
    :attributes ((service-name
		  :initarg :service-name
		  :accessor service-name))
    :content (let ((service-name (service-name *current-tag*) ))
	    (ul
	     (mapcar (lambda (v-p)
		       (destructuring-bind (v p) v-p
			 (declare (ignorable v) )
			 (li p)))
		     (service-parameters service-name)))))

(defhtmltag view-service-cookie
    :attributes ((cookie-name
		  :initarg :cookie-name
		  :accessor cookie-name))
    :script-libraries `("/scripts/jquery-1.4.1.js"
			"/scripts/jquery.cookie.js")
    :ready (let ((cookie-name (string-downcase (symbol-name (cookie-name *current-tag*) ) ) ))
	     (with-output-to-string (os)
	       (format os "$('#~a').text(decodeURI($.cookie(~s)))"
		       cookie-name
		       cookie-name)))

    :content (let ((cookie-name (string-downcase (symbol-name (cookie-name *current-tag*) ) ) ))
	    (span (+@ :id cookie-name)
		  +nbsp+)))

(defhtmltag view-service-cookies
    :attributes ((service-name
		  :initarg :service-name
		  :accessor service-name))
    :content (let ((service-name (service-name *current-tag*) ))
	    (table
	     (mapcar (lambda (c)
		       (tr (td c)
			   (td
			    (view-service-cookie (+@ :cookie-name c)))))
		     (service-cookies service-name)))))

(defhtmltag view-service-method-form
    :attributes ((service-name
		  :initarg :service-name
		  :accessor service-name)
		 (method-name
		  :initarg :method-name
		  :accessor method-name))
    :script-libraries (list
			"/scripts/jquery-1.4.1.js"
			(format nil "~a?_meta=client" (hunchentoot:script-name hunchentoot:*request*) ))
    :content (with-slots (service-name method-name) *current-tag*
	       (list
		(form (+@ :id "service" 
			  :action (ps:ps-inline* (method-name) )
			  :method nil)
		      (table (mapcar (lambda (var-parm)
				       (destructuring-bind (var parm) var-parm (declare (ignorable var) )
							   (tr (td parm) (td (input (+@ :id parm :name parm :type "text")) ) )))
				     (service-parameters service-name)))

		      (submit)))))

(defhtmltag view-service-method-description
    :attributes ((service-name
		  :initarg :service-name
		  :accessor service-name)
		 (method-name
		  :initarg :method-name
		  :accessor method-name))
    :content (with-slots (service-name method-name) *current-tag*
	       (div
		(a (+@ :name method-name)
		   (h3 method-name))
		(table (+@ :style "border: 1px solid gray")
		 (tr (td (em "Documentation") ) (td (service-method-documentation-string service-name method-name) ) )
		 (tr (td (em "Cookies") ) (td (service-method-cookies service-name method-name) ) )
		 (tr (td (em "Parameters") ) (td (ul
						  (mapcar (lambda (var-parm)
							    (destructuring-bind (var parm) var-parm
							      (li (format nil "~a : '~a'" var parm) )))
							  (service-method-parameters service-name method-name)))))))))

(defhtmltag view-service-description
    :attributes ((service-name
		  :initarg :service-name
		  :accessor service-name))
    :script-libraries (list
		       (with-output-to-string (os)
			 (format os "~a?_meta=client" (hunchentoot:script-name hunchentoot:*request*))))
    :content (let ((service-name (service-name *current-tag*) ))
	    (list 
	     (h1 "Service : "
		 (string-downcase (symbol-name service-name)))

	     (h2 "Documentation")
	     (view-service-documentation (+@ :service-name service-name))

	     (h2 "Methods")
	     (ul
	      (mapcar (lambda (m)
			(li (a (+@ :href (format nil "#~a" m)) m) ))
		      (service-methods service-name)))
	     (mapcar (lambda (m)
		       (view-service-method-description (+@ :service-name service-name
							    :method-name m)))
		     (service-methods service-name)))))
