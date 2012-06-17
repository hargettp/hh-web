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

  'rotating-log-sender

  ))

;;;------------------------------------------------------------------------------------
;;; Log rotation
;;;------------------------------------------------------------------------------------

(defclass rotating-log-sender (log5:stream-sender)
  ((interval
   :initform (* 24 60 60) ;; daily
   :initarg :interval 
   :accessor log-interval
   :documentation "Interval in seconds between log rotations")
   (last-write
    :initform (get-universal-time)
    :accessor last-write)
   (maxlogs
    :initform 10
    :initarg :max
    :accessor maxlogs))
  (:documentation "A variation of stream sender that rotates logs based on a configurable
   interval.  Log rotation happens just before a message is logged."))

(defun numbered-file-name (file-name number)
  (if (= 0 number)
      file-name
      (make-pathname :directory (pathname-directory file-name)
		     :name (format nil "~a.~a" (pathname-name file-name) number)
		     :type (pathname-type file-name))))

(defun next-log-file-name (log-file-name max)
  (loop for i from 1 to max
     for new-name = (numbered-file-name log-file-name i)
     for exists = (probe-file new-name)
     when (not exists ) return (values new-name exists)))

(defgeneric log-interval-expired-p ( sender )
  (:documentation "Return true if it's time to rotate the sender's logs")
  (:method ( (sender rotating-log-sender) )
    (> (- (get-universal-time) (last-write sender) )
       (log-interval sender))))

(defgeneric delete-old-logs ( sender )
  (:method ( (sender rotating-log-sender) )
    (loop for i = (maxlogs sender) then (+ i 1)
       for log-file-name = (numbered-file-name (log5:location sender) i)
       while (probe-file log-file-name)
       ;; do (format *standard-output* "Log rotation wants to delete ~s~%" log-file-name)
       do (delete-file log-file-name))))

(defgeneric rename-existing-logs ( sender )
  (:method ( (sender rotating-log-sender) )
    (loop for i from (maxlogs sender) downto 0
       for old-name = (numbered-file-name (log5:location sender) i)
       for new-name = (numbered-file-name (log5:location sender) (+ i 1) )
       when (probe-file old-name) 
       ;; do (format *standard-output* "Log rotation wants to rename ~s to ~s ~%" old-name new-name)
       do (rename-file old-name new-name))))

(defgeneric rotate-logs ( sender &optional force)
  (:method ( (sender rotating-log-sender) &optional (force nil) )
    (when (or force (log-interval-expired-p sender) )
      ;; close existing stream
      (when (log5:output-stream sender)
	(close (log5:output-stream sender) ))
      ;; if max logs already created, delete older ones
      (delete-old-logs sender)
      (rename-existing-logs sender)
      ;; create a new one
      (setf (slot-value sender 'log5:output-stream)
	    (open (log5:location sender) 
		  :direction :output
		  :if-does-not-exist :create
		  :if-exists :error))
      (setf (last-write sender) (get-universal-time)))))

(defmethod initialize-instance :after ( (sender rotating-log-sender) &key) 
  (rotate-logs sender t))

(defmethod log5:start-handling ( (sender rotating-log-sender) )
  `((rotate-logs log5::sender)))

