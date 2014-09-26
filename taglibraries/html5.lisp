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

;; html5 additions Copyright (c) 2014 Sabra Crolleton
(in-tag-library :html5)

;;-------- Entities ---------------
;;

(defentity +nbsp+ "&nbsp;")
(defentity +copy+ "&copy;")
(defentity +gt+ "&gt;")
(defentity +lt+ "&lt;")
(defentity +mdash+ "&mdash;")

(defhtmltag a
    :attributes (download name href hreflang media rel target type data-toggle))


(defhtmltag button
    :noendtag t
    :attributes (autofocus disabled form formaction formenctype formmethod 
                           formnovalidate formtarget name type value onclick
													 ng-app))

(defhtmltag p)

(defhtmltag div
    :attributes ())

(defhtmltag span)

(defhtmltag textarea
    :attributes (name cols rows))

(defhtmltag input
    :noendtag t
    :attributes (name type value placeholder))

(defhtmltag submit
    :attributes (value)
    :content (input (+@ :type "submit" :value (or value "OK")) ))


(defhtmltag checkbox
    :bases (input)
    :tag "input"
    :attributes (_label checked)
    :init (progn (setf type "checkbox"))
    :content (list (when _label _label)
		   (call-next-tag-method)))

(defhtmltag text-field
    :bases (input)
    :tag "input"
    :attributes (_label)
    :content (list (when _label _label)
		   (call-next-tag-method)))

(defhtmltag form
    :attributes (action method accept-charset autocomplete enctype name 
                        novalidate target)
    :init (progn (setf action (or action "GET"))))

(defhtmltag img
    :noendtag t
    :attributes (src data-src usemap alt width height border))

(defhtmltag imgmap
    :tag "map"
    :attributes (name))

(defhtmltag area
    :noendtag t
    :attributes (shape coords href))

(defhtmltag table)

(defhtmltag tr)

(defhtmltag th)

(defhtmltag td
    :attributes (colspan))

(defhtmltag b)

(defhtmltag em)

(defhtmltag strong)

(defhtmltag h1)
(defhtmltag h2)
(defhtmltag h3)
(defhtmltag h4)
(defhtmltag h5)
(defhtmltag h6)

(defhtmltag ul)

(defhtmltag ol)

(defhtmltag li)

(defhtmltag code)

(defhtmltag pre)

(defhtmltag br
    :noendtag t)

(defhtmltag iframe
    :attributes (src name sandbox seamless srcdoc width height))

(defhtmltag abbr)
(defhtmltag address)
(defhtmltag area
    :attributes (alt coords download href hreflang media nohref rel shape target type))
(defhtmltag article)
(defhtmltag aside)
(defhtmltag audio
    :attributes (autoplay controls loop muted preload src))
(defhtmltag base
    :attributes (href target))
(defhtmltag bdo
    :attributes (dir))
(defhtmltag blockquote
    :attributes (cite))
(defhtmltag canvas
    :attributes (height width))
(defhtmltag caption)
(defhtmltag cite)
(defhtmltag dfn)
(defhtmltag kbd)

(defhtmltag col
    :attributes (span))
(defhtmltag colgroup
    :attributes (span))
(defhtmltag datalist)
(defhtmltag dd)
(defhtmltag del
    :attributes (cite datetime))
(defhtmltag dfn)
(defhtmltag dl)
(defhtmltag dt)
(defhtmltag embed
    :attributes (height src type width))
(defhtmltag fieldset
    :attributes (disabled form name))
(defhtmltag figcaption)
(defhtmltag figure)
(defhtmltag footer)
(defhtmltag header)
(defhtmltag hr)
(defhtmltag ins
    :attributes (cite datetime))
(defhtmltag keygen
    :attributes (autofocus challenge disabled form keytype name))
(defhtmltag legend)
(defhtmltag main)

;;;(defhtmltag map :attributes (name))

(defhtmltag mark)
(defhtmltag meter
    :attributes (form high low max min optimum value))
(defhtmltag nav)
(defhtmltag noscript)
(defhtmltag object
    :attributes (form height name type usemap width))
(defhtmltag optgroup
    :attributes (disabled label))
(defhtmltag param
    :attributes (name value))
(defhtmltag progress
    :attributes (max value))
(defhtmltag q)
(defhtmltag rp)
(defhtmltag rt)
(defhtmltag ruby)
(defhtmltag s)
(defhtmltag samp)
(defhtmltag section)
(defhtmltag source)
(defhtmltag strike)
(defhtmltag sub)
(defhtmltag summary)
(defhtmltag sup)
(defhtmltag tfoot)

(defhtmltag track
    :attributes (default kind label src srclang))

(defhtmltag video
    :attributes (autoplay controls height loop muted poster preload src width))
(defhtmltag wbr)

(defhtmltag script
    :attributes (type src)
    :init (progn (setf type "text/javascript")))


