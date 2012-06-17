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

(export `( *user-agent-strings* *enabled-user-agents* *user-agent* enabled-user-agents ))

(defvar *user-agent-strings* 
  (let ((new-user-agent-strings (make-hash-table)))
    (macrolet ((user-agent-string (key string)
				   `(setf (gethash ,key new-user-agent-strings) ,string)))
      	       (user-agent-string :bing "Bingbot")
	       (user-agent-string :chrome "Chrome")
	       (user-agent-string :firefox "Firefox")
	       (user-agent-string :firefox3 "Firefox 3")
	       (user-agent-string :firefox3.5 "Firefox 3.5")
	       (user-agent-string :firefox3.6 "Firefox 3.6")
	       (user-agent-string :firefox3.7 "Firefox 3.7")
	       (user-agent-string :firefox3.8 "Firefox 3.8")
	       (user-agent-string :firefox4 "Firefox 4")
	       (user-agent-string :gecko "Gecko")
	       (user-agent-string :google "Googlebot")
	       (user-agent-string :ie "MSIE ")
	       (user-agent-string :ie6 "MSIE 6")
	       (user-agent-string :ie7 "MSIE 7")
	       (user-agent-string :ie8 "MSIE 8")
	       (user-agent-string :ie9 "MSIE 9")
	       (user-agent-string :iphone "iPhone")
	       (user-agent-string :ipad "iPad")
	       (user-agent-string :khtml "KHTML")
	       (user-agent-string :mozilla "Mozilla")
	       (user-agent-string :msie "MSIE ")
	       (user-agent-string :msie6 "MSIE 6")
	       (user-agent-string :msie7 "MSIE 7")
	       (user-agent-string :msie8 "MSIE 8")
	       (user-agent-string :msie9 "MSIE 9")
	       (user-agent-string :msn "msnbot") ;; bing
	       (user-agent-string :opera "Opera")
	       (user-agent-string :webkit "WebKit")
	       (user-agent-string :safari "Safari")
	       (user-agent-string :slurp "Yahoo! Slurp")
	       )
    new-user-agent-strings))

(defvar *enabled-user-agents* ())

(defvar *user-agent* :default)

(defun request-user-agent (&optional (hunchentoot:*request* hunchentoot:*request*))
  (when hunchentoot:*request*
    (hunchentoot:user-agent hunchentoot:*request*)))

(defun detect-user-agents (&optional (hunchentoot:*request* hunchentoot:*request*))
  (let ((user-agent-string (request-user-agent)))
    (append (loop for user-agent-key in *enabled-user-agents*
	       if (search (gethash user-agent-key *user-agent-strings*) user-agent-string)
	       collect user-agent-key)
	    `( :default ))))

(defmacro enabled-user-agents (&rest user-agents)
  `(setf *enabled-user-agents* ',user-agents))

;; TODO note that we are making a forward reference to the HTML macro defined later in tags.lisp
;; not ideal, but since we don't use these macros together until after all are defined, not a near-term problem
(defmacro html-for-user-agent (&rest body) 
  "Given the markup for a page, build a cond statement that will return the rendering appropriate to the current user agent"
  (multiple-value-bind (user-agent-bodies default-body) (loop for remainder on body
							   if (and (listp (car remainder)) (keywordp  (car (car remainder))))
							   collect (car remainder) into bodies else do ( return (values bodies remainder))
							   finally (return (values nil body)))
    (let ((user-agent-cases (loop for user-agent-body in user-agent-bodies
			       collect (destructuring-bind (user-agent-key &rest render-body) user-agent-body
					     `( (eql ,user-agent-key *user-agent*) (html (list ,@render-body)))))))
      `(cond ,@user-agent-cases
	     ( ( eql ':default *user-agent* ) (html (list ,@default-body)))))))