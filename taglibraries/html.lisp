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

(in-tag-library :html)

;;-------- Entities ---------------
;;

(defentity +nbsp+ "&nbsp;")
(defentity +copy+ "&copy;")
(defentity +gt+ "&gt;")
(defentity +lt+ "&lt;")
(defentity +mdash+ "&mdash;")

(defhtmltag p)
(defhtmltag div)
(defhtmltag span)

(defhtmltag textarea
    :attributes (name cols rows))

(defhtmltag input
    :noendtag t
    :attributes (name type value))

(defhtmltag submit
    :content (input (+@ :type "submit" :value "OK") ))

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
    :attributes (action method)
    :init (progn (setf action "GET")))

(defhtmltag img
    :noendtag t
    :attributes (src usemap alt width height border))

(defhtmltag imgmap
    :tag "map"
    :attributes (name))

(defhtmltag area
    :noendtag t
    :attributes (shape coords href))

(defhtmltag table)

(defhtmltag tr)

(defhtmltag th
    :tag "thead")

(defhtmltag td
    :attributes (colspan))

(defhtmltag a
    :attributes (name href))

(defhtmltag b)

(defhtmltag em)

(defhtmltag strong)

(defhtmltag h1)

(defhtmltag h2)

(defhtmltag h3)

(defhtmltag h4)

(defhtmltag ul)

(defhtmltag ol)

(defhtmltag li)

(defhtmltag code)

(defhtmltag pre)

(defhtmltag br
    :noendtag t)

(defhtmltag iframe
    :attributes (src frameborder allowtransparency width height))

(defhtmltag script
    :attributes (type src)
    :init (progn (setf type "text/javascript")))