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

(defpackage :hh-web
  (:nicknames :hh-web)
  (:use :cl 
	:cl-fad
	:sb-mop 
	:cl-ppcre 
	:local-time 
	:vecto) 
  (:export 
   ;; Exported symbols go here

   ;; For tags

   #:defhtmltag
   #:defentity
   #:hout
   #:*current-tag*
   #:*this-tag*
   #:*tag-library-registry*
   #:call-next-tag-method

   #:_body
   #:body-of
   #:style-of 
   #:html-id
   #:css-class
   #:style-of
   #:rel-of
   #:id
   #:class
   #:style
   #:rel

   #:htmltag
   #:render-as-html

   #:+@
   #:+title
   #:+link

   #:expansion
   #:tags

   #:html
   #:page
   #:with-tag-reader

   ;; For tag libraries
   #:*tag-library*
   #:*tag-library-provider-registry*
   #:*package-template-folder*
   #:*package-tag-library-folder*
   #:*template*
   #:*minimum-template-stale-time*
   #:create-folder-tag-library-provider
   #:create-asdf-system-tag-library-provider

   #:in-tag-library
   #:use-tag-library
   #:+tag-library
   #:local-tag-library-provider-registry-symbol
   
   ;; For templates
   #:template
   #:deftemplate
   #:deftemplates
   #:*template*
   #:*template-provider-registry*
   #:create-folder-template-provider
   #:create-asdf-system-template-provider
   #:local-template-provider-registry-symbol
   #:load-templates

   ;; Caching
   #:get-cached-item
   #:make-fs-cache
   #:make-package-fs-cache
   #:create-file-cache-dispatcher-and-handler
   #:create-file-cache-dispatcher-and-handler-for-root
   #:create-package-file-cache-dispatcher-and-handler

   ;; Cookies
   #:defcookie
   #:with-cookies

   ;; Locales
   #:_ 
   #:set-locale
   #:gettext
   #:deflocale
   #:get-localizable-strings
   #:get-localization-contexts
   #:get-available-locales
   #:get-localizable-string-notes
   #:get-string-localization
   #:get-string-localization-notes
   #:init-localization
   #:*locale-package*
   #:locale-service

   ;; Logging

   ;; -- categories
   #:stack-trace
   #:http-access 
   #:http-message
   #:url-dispatch

   ;; -- functions
   #:log-http-access
   #:log-http-message
   #:use-backtrace-logging
   #:msg-timestamp

   ;; -- types
   #:rotating-log-sender

   ;; Server
   #:create-web-server
   #:run-web-server

   ;; Services
   #:with-http-parameters
   #:meta-service-request-p
   #:http-get-p
   #:http-post-p
   #:http-put-p
   #:http-delete-p
   #:defservice
   #:defservice-client
   #:defservice-handler
   #:service-url
   #:service-client

   ;; Skeleton
   #:make-skeleton

   ;; URLs
   #:defurl
   #:defurls
   #:create-package-folder-dispatcher-and-handler
   #:reset-urls
   #:flush-url-category
   #:dispatch-url-cache

   ;; User-agents
   #:*user-agent-strings* 
   #:*enabled-user-agents* 
   #:*user-agent* 
   #:enabled-user-agents

   ))
