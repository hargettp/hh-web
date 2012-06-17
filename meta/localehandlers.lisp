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

(defservice-handler locale-service

    (:method create-locale 
      (create-package-locale locale-package locale-name)
      t)

    (:method delete-locale 
      (delete-package-locale locale-package locale-name))

    (:method save-string-localization 
      ;; update description in development locale--but only if we're not
      ;; actually working with that locale (because we'd just be doing 
      ;; redundant work)
      (unless (string= locale-name *development-locale-name*)
	(load-string-localizations locale-package *development-locale-name*)
	(let* ((development-locale (gethash *development-locale-name* *locales*))
	       (localized-string (or (gethash string-name (strings development-locale))
				     (make-instance 'string-localization :name string-name))))
	  (with-slots (locale localization notes) localized-string
	    (setf locale *development-locale-name*
		  localization string-name
		  notes string-description))
	  (setf (gethash string-name (strings development-locale)) localized-string))
	(save-string-localizations locale-package *development-locale-name*))

      ;; update information in specified locale
      (load-string-localizations locale-package locale-name)
      (let* ((*locale* (gethash locale-name *locales*))
	     (localized-string (or (gethash string-name (strings *locale*))
				     (make-instance 'string-localization :name string-name))))
	(with-slots (locale localization notes) localized-string
	  (setf locale locale-name
		localization string-localization
		notes string-notes))
	(setf (gethash string-name (strings *locale*)) localized-string))
      (save-string-localizations locale-package locale-name)))