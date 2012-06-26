;; Copyright (c) 2011 Phil Hargett

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

(+tag-library :html)
(+tag-library :script)

(+tag-library :docs)

(page 
 (+title "Documentation for hh-web-tags")
 (h1 "Overview")
 (p "Countless libraries exist to generate HTML, and so does $(package-reference). But the latter generates not only HTML but CSS, 
     Javascript, and even some page metadata. And it generates it all in a modular fashion.")
 (p "A fundamental insight differentiates $(package-reference) from other such libraries: in Lisp terms, most libraries regard
    HTML generation as a function with a single return value (the HTML).  Instead, $(package-reference) content generation
    as a function returning multiple values $+mdash+ a not unusual situation in Lisp, but uncommon (or unnatural) in
    other languages.")
 (p "Specifically, the central HTML generation facility of $(package-reference) (the $(html:a { :href \"#_macro_html\"} \"html\") macro) returns many values, of which the first is the HTML but also several
  other values, some of which are fragments for inclusion in the final containing HTML document:")
 (ul 
  (li "Generated HTML content")
  (li "Title for the containing document")
  (li "Links to appear in the HEAD element of the containing document")
  (li "Style sheets required by the tag")
  (li "CSS style information required by the tag")
  (li "References to script libraries required by the tag")
  (li "Javascript fragments required by the tag")
  (li "Scripts to run when the containing document is loaded in the browser for performing initialization requried by the tag"))
 (p "In typical documents, tags contain other tags (e.g., $(code (literal (div (p \"text\")))), a paragraph tag inside a div tag).  Each of the values returned by 
     the $(html:a { :href \"#_macro_html\"} \"html\") macro are always a proper aggregation of any corresponding values from tags inside the outer tag's body.  Thus,
     if an interior tag references a script library, then now the outer tag will, too.  Same for any scripts produced by interior tags: now the outer tag will 
     produce that script among it's scripts as well.")
 (h2 "Example")
 (p "For example, imagine another language (such as ColdFusion, or JSP) that defined a custom tag to create a sidebar navigation element
     on a page:")
 (code (literal (sidebar)))
 (p "In these other languages, that sidebar custom tag might result in a few divs added to a page:")
 (pre (literal (div {:id "sidebar"}
		     (div {:id "home_navigation_link" :class "navigation_link"} "Home")
		     (div {:id "products_navigation_link" :class "navigation_link"} "Products")
		     (div {:id "services_navigation_link" :class "navigation_link"} "Services")
		     (div {:id "news_navigation_link" :class "navigation_link"} "News")
		     (div {:id "contact_navigation_link" :class "navigation_link"}  "Contact Us"))))
 (p "With $(package-reference), the result includes not only the above HTML, but also the CSS:")
 (pre "
  .navigation_link {
    color : black;    
  }

  .navigation_link:hover {
    color : silver;
  }

  .navigation_link.selected {
    color : white;
    background-color : silver;
  }
  ")
 (p "And the Javascript:")
 (pre "
   $$('.file-tree-files span').click(function(evt){
         $$('.navigation_link.selected').toggleClass('selected');
         $$(this).toggleClass('selected');
         return false;        
   });
  ")
 (p "Oh, and since this specific example uses jQuery, $(package-reference) also generates a reference to jQuery:")
 (pre (literal (script {:src "/scripts/jquery/jquery-1.4.1.js"})))
 (h1 "Reference")
 (h2 "Macros")
 (macro-docs {:package :hh-web-tags})
 (h2 "Functions")
 (function-docs {:package :hh-web-tags})
 (h2 "Variables")
 (variable-docs {:package :hh-web-tags})
 (h2 "Other")
 (other-docs {:package :hh-web-tags}))