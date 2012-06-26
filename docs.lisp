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

(in-tag-library :docs)

(+tag-library :html)
(+tag-library :script)

(defun is-macro-p (symbol)
  (macro-function symbol))

(defun is-function-p (symbol)
  (and (handler-case (symbol-function symbol) 
	  (undefined-function () nil))
       (not (is-macro-p symbol))))

(defun is-variable-p (symbol)
  (and (not (is-macro-p symbol))
       (not (is-function-p symbol))
       (equal #\* (elt (symbol-name symbol) 0))))

(defun is-other-symbol-p (symbol)
  (and (not (is-macro-p symbol))
       (not (is-function-p symbol))
       (not (is-variable-p symbol))))

(defun sorted-package-symbols (package-designator predicate)
  (sort (loop for sym being the external-symbols of package-designator
	   when (funcall predicate sym) 
	   collect sym)
	#'(lambda (left right)
	  (string-lessp (symbol-name left) (symbol-name right)))))

(defhtmltag package-reference
    :content (em "hh-web-tags"))

(defhtmltag symbol-reference
    :attributes (sym type)
    :content (let* ((name (string-downcase (symbol-name sym)))
		    (anchor (format nil "_~a_~a" type name)))
	       (a {:href anchor} (strong {:style "font-family:monospace" } 
					 name))))

(defhtmltag package-symbol
    :attributes (sym type)
    :content (let* ((name (string-downcase (symbol-name sym)))
		    (anchor (format nil "_~a_~a" type name)))
	       (a {:name anchor} (strong {:style "font-family:monospace" } name))))

(defhtmltag literal
    :content (hunchentoot:escape-for-html (html _body)))

(defhtmltag sidebar
    :noendtag t)

(defhtmltag macro-docs
    :attributes (package)
    :content (loop for sym in (sorted-package-symbols package #'is-macro-p)
		collect (list (p (package-symbol {:sym sym :type "macro" }))
			      (p (documentation sym 'function)))))

(defhtmltag function-docs
    :attributes (package)
    :content (loop for sym in (sorted-package-symbols package #'is-function-p)
		collect (list (p (package-symbol {:sym sym :type "function" }))
			      (p (documentation sym 'function)))))

(defhtmltag variable-docs
    :attributes (package)
    :content (loop for sym  in (sorted-package-symbols package #'is-variable-p)
		collect (list (p (package-symbol { :sym sym :type "variable"} ))
			      (p (documentation sym 'variable)))))

(defhtmltag other-docs
    :attributes (package)
    :content (loop for sym in (sorted-package-symbols package #'is-other-symbol-p)
		collect (list (p (let* ((name (string-downcase (symbol-name sym))))
				   (strong {:style "font-family:monospace" } name))))))