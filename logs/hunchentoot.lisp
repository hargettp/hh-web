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

  'http-access 
  'http-message

  'log-http-access
  'log-http-message

  ))

;;;------------------------------------------------------------------------------------
;;; Categories
;;;------------------------------------------------------------------------------------

(log5:defcategory http-access)

(log5:defcategory http-message)

;;;------------------------------------------------------------------------------------
;;; Hunchentoot loggers
;;;------------------------------------------------------------------------------------
(defun log-http-access (&key return-code content content-length)
  "Sends a standardized access log message to the access log with
  information about the current request and response."
  (log5:log-for (http-access) "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] [~A] \"~A ~A~@[?~A~] ~
                    ~A\" ~A ~:[~*-~;~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\"~%"
		(hunchentoot:remote-addr*)
		(hunchentoot:header-in* :x-forwarded-for)
		(hunchentoot:authorization)
		(hunchentoot::iso-time)
		(hunchentoot:request-method*)
		(hunchentoot:script-name*)
		(hunchentoot:query-string*)
		(hunchentoot:server-protocol*)
		return-code
		content
		content-length
		(hunchentoot:referer)
		(hunchentoot:user-agent)))

(defun log-http-message (log-level format-string &rest format-arguments)
  "Sends a formatted message to the file http message log.  FORMAT and ARGS
are as in FORMAT.  LOG-LEVEL is a keyword denoting the log level or
NIL in which case it is ignored."
  (log5:log-for (http-message) "[~A~@[ [~A]~]] ~?~%"
		(hunchentoot::iso-time) 
		log-level
		format-string 
		format-arguments))
