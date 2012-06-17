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
  
  'defcookie
  'with-cookies
  
  )
 )

(defgeneric cookie-key (cookie)
  (:documentation
    "
    ")
  (:method ((cookie symbol))
    (symbol-name cookie)))

(defgeneric cookie-variable (cookie)
  (:documentation
    "
    ")
  (:method ((cookie symbol))
    (intern (format-string "*~a*" (symbol-name cookie))
            (symbol-package cookie))))

(defgeneric cookie-expiry-date (cookie)
  (:documentation
    "
    ")
  (:method ((cookie t))
    ;; defaults to lasting a day
    (+ (get-universal-time) (* 60 60 24)))) 

(defgeneric get-cookie-value (cookie)
  (:documentation
    "Obtain cookie value from current hunchentoot:*request*")
  (:method ((cookie t))
    (hunchentoot:url-decode
       (hunchentoot:url-decode 
        (hunchentoot:cookie-in (cookie-key cookie))))))

(defgeneric set-cookie-value (cookie value)
  (:documentation
   "Set cookie value in current hunchentoot:*reply*")
  (:method ((cookie t) value)
    (log5:log-for (log5:info) "Adding cookie ~s with value ~s"
			     (cookie-key cookie)
			     (if value 
				 ;(hunchentoot:url-encode value)
				 value
				 ""))
    (hunchentoot:set-cookie (cookie-key cookie)
			    :value (if value 
				       ;(hunchentoot:url-encode value)
				       value
				       "")
			    :path "/"
					; :domain realm-name
			    :expires (if value
					 (cookie-expiry-date cookie)
					 ; automatically expired
					 1))))

(defmacro defcookie (name
                     key
                     var
                     expiry-period ; expiry in seconds
		     &optional
		     (export nil))
  `(progn
     (defvar ,var)
     (defmethod cookie-key ((cookie (eql (quote ,name))))
       ,key)
     (defmethod cookie-variable ((cookie (eql (quote ,name))))
       (quote ,var))
     (defmethod cookie-expiry-date ((cookie (eql (quote ,name))))
       (+ (get-universal-time) ,expiry-period))

     ,(when export
	    `(export (list
		      (quote ,name)
		      (quote ,var))))))

(defmacro with-cookies (&optional (cookies nil) &rest body)
  (let ((cookie-in-list (if cookies 
			    (mapcar (lambda (c)
				      `(,(cookie-variable c) (get-cookie-value (quote ,c))))
				    cookies)))
        (cookie-out-list (if cookies 
			     (mapcar (lambda (c)
				       `(set-cookie-value (quote ,c) ,(cookie-variable c)))
				     cookies))))
    `(let (,@cookie-in-list)
       (let ((result (progn ,@body ) ))
	 ,@cookie-out-list
	 (log5:log-for (log5:trace) "With-cookies result is ~s~%" result)
	 result))))
