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

(defpackage #:hh-web-asd
  (:use :cl :asdf))

(in-package :hh-web-asd)
(defsystem hh-web 
    :name "hh-web" 
    :version "0.02" 
    :serial t 
    :components ((:file "package")
		 (:file "utils") 
		 (:module "l10n"
			:serial t
			:components ((:file "i18n")))
		 (:module "logs"
			:serial t
			:components ((:file "outputs")
				     (:file "categories")
				     (:file "rotating")
				     (:file "misc")
				     (:file "hunchentoot")))
		 (:file "useragents")
		 (:file "tags")
		 (:file "taglibraries")
		 (:file "templates")
		 (:file "images")
		 (:file "cookies")
		 (:file "services")
		 (:file "cache")
		 (:file "urls")
		 (:file "documentation")
		 (:file "server")
		 (:module "meta"
			:serial t
			:components ((:file "localeservices") (:file "localehandlers")))
		 (:file "skeleton"))

  :depends-on (
	       ;; external packages
	       :trivial-backtrace
	       :log5
	       (:version :hunchentoot "1.2")
	       :drakma
	       :cl-fad
	       :parenscript
	       :cl-ppcre
	       :local-time
	       :vecto
	       :cl-base64
	       :ironclad
               :uuid

	       ;; project packages
               ))

(defsystem hh-web-tests 
    :name "hh-web-tests" 
    :version "0.02" 
    :serial t 
    :components ((:file "tests")) 
    :depends-on (:hh-web))

