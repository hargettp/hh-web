(in-tag-library :doc)

(+tag-library :html)
(+tag-library :script)

(+tag-library :ui-theme)

(defhtmltag doc-toc
    :content "")


(defun is-macro-p (symbol)
  (macro-function symbol))

(defun is-function-p (symbol)
  (and (handler-case (symbol-function symbol) 
	  (undefined-function () nil))
       (not (is-macro-p symbol))))

(defun is-variable-p (symbol)
  (and (not (is-macro-p symbol))
       (not (is-function-p symbol))
       (> (length (symbol-name symbol)) 0)
       (equal #\* (elt (symbol-name symbol) 0))))

(defun is-other-symbol-p (symbol)
  (and (not (is-macro-p symbol))
       (not (is-function-p symbol))
       (not (is-variable-p symbol))))

(defun sorted-package-symbols (package-designator predicate)
  (sort (loop for sym being the external-symbols of (find-package (string-upcase package-designator))
	   when (funcall predicate sym) 
	   collect sym)
	#'(lambda (left right)
	  (string-lessp (symbol-name left) (symbol-name right)))))

(defmacro formatted-documentation (symbol documentation-type)
  `(let ((doc (documentation ,symbol ,documentation-type)))
    (loop with start = 0
       for end = (search (format nil "~%~%") doc :start2 start)
       for paragraph = (subseq doc start end)
       collect (p paragraph)
       while end
       do (setf start (+ 2 end)))))

(defun function-lambda-list (function)
  #+sbcl
  (sb-kernel:%fun-lambda-list function)
  #+ccl
  (ccl:arglist function))

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
	       (a {:name anchor} (em (strong {:style "font-family:monospace" } name)))))

(defhtmltag lambda-list
    :attributes (symbol package function-accessor)
    :content (span {:style "font-family:monospace"}
		   (let ((*print-case* :downcase)
			 (*package* (find-package (string-upcase package)))
			 (args (function-lambda-list (funcall function-accessor symbol))))
		     (if args 
			 (format nil "~s" args)
			 `( "(" ")")))))

(defhtmltag literal
    :content (hunchentoot:escape-for-html (html _body)))

(defhtmltag sidebar
    :noendtag t)

(defhtmltag package-docs
    :attributes (package)
    :content (formatted-documentation (find-package (string-upcase package)) t))

(defhtmltag dependency-docs
    :style "
  .dependency {
  text-decoration : none;
  color : #CCC;
}
"
    :attributes (package)
    :content (ul 
	      (loop for package in (cdr (assoc 'asdf:load-op (asdf:component-depends-on 'asdf:load-op (asdf:find-system package))))
		 collect (li (a {:href (string-downcase package) :class "dependency" } package)))))

(defhtmltag macro-docs
    :attributes (package)
    :content (loop for sym in (sorted-package-symbols package #'is-macro-p)
		collect (list (p (package-symbol {:sym sym :type "macro" })
				 (lambda-list {:symbol sym :package package :function-accessor #'macro-function}))
			      (p (formatted-documentation sym 'function)))))

(defhtmltag function-docs
    :attributes (package)
    :content  (loop for sym in (sorted-package-symbols package #'is-function-p)
		 collect (list (p {:style "text-indent:-50px;padding-left:50px"} 
				  (package-symbol {:sym sym :type "function" }) 
				  (lambda-list {:symbol sym :package package :function-accessor #'symbol-function}))
			       (p (formatted-documentation sym 'function)))))

(defhtmltag variable-docs
    :attributes (package)
    :content (loop for sym  in (sorted-package-symbols package #'is-variable-p)
		collect (list (p (package-symbol { :sym sym :type "variable"}))
			      (p (formatted-documentation sym 'variable)))))

(defhtmltag other-docs
    :attributes (package)
    :content (loop for sym in (sorted-package-symbols package #'is-other-symbol-p)
		collect (list (p (let* ((name (string-downcase (symbol-name sym))))
				   (strong {:style "font-family:monospace" } name))))))

(defhtmltag doc-toc
    :style "
.doc_content {
vertical-align : text-top;
margin-left : 35px;

color : #FFF;
}

.doc_content div, .doc_content p {
  margin-top: 1em;
  margin-bottom: 1em;
}

.doc_toc {
  margin-left: 1em;
  width : 250px;
  float : right;
  background-color : rgba(0,0,0,0.15);
  display : none;
}
"
    :ready "
$('.doc_toc').fadeIn(800);
"
    :content (div {:class "doc_content" } (div {:class "doc_toc" }
			  (ul 
			   (li (a {:href "#dependencies" } "Dependencies"))
			   (li (a {:href "#macros" } "Macros"))
			   (li (a {:href "#functions" } "Functions"))
			   (li (a {:href "#variables" } "Variables"))
			   (li (a {:href "#other" } "Other Symbols"))
			   ))))

(defhtmltag doc-page
    :style "
a {
  text-decoration : none;
  color : #CCC;
}

a:visited {
  color : #CCC;
}
"
    :attributes (package)
    :content  (ui-page
	       ;; (+title "Documentation for $package")
	       (+title (format nil "Documentation for ~a" package))
	       (doc-toc)
	       (h1  "Reference for " (string-upcase package))
	       (package-docs {:package package})
	       (h2 (a {:name "dependencies"}) "Dependencies")
	       (dependency-docs {:package package})
	       (h2 (a {:name "macros" } "Macros"))
	       (macro-docs {:package package})
	       (h2 (a {:name "functions" } "Functions"))
	       (function-docs {:package package})
	       (h2 (a {:name "variables" }) "Variables")
	       (variable-docs {:package package})
	       (h2 (a {:name "other"}) "Other")
	       (other-docs {:package package})
	       ))
