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

;;; TODO Most of the functions in this file were written before the author's knowledge
;;; of Lisp had begun to mature; thus, most if not all should either be rewritten
;;; or replaced as time allows

;; strings

(defmacro format-string (format-control-string &rest args)
  `(with-output-to-string (os)
     (format os ,format-control-string ,@args)
     )
  )

(defun string-starts-with (target-string search-string)
  "
  Return T if target-string starts with search-string, 
  nil otherwise 
  "
  (if (> (length search-string) (length target-string))
      nil
      (equal (subseq target-string 0 (length search-string)) 
             search-string
             )    
      )
  )

(defun tokenize (token-string token &optional (start-pos 0) )
  "Break a token-string into a list, where each element is
   separated in the original token-string by token"
  (if (> (length token-string) start-pos)
      (let ( (pos (search token token-string :start2 start-pos)) )
        (if pos
            (if (> pos 0)
                (cons 
                 (subseq token-string start-pos pos)
                 (tokenize token-string token (+ pos (length token)))
                 )
                (tokenize token-string token 1)
                )
            (list (subseq token-string start-pos))
            )
        )
      (list)
      )
  )

(defun detokenize (elements token &optional (prefix ""))
  "Combine elements into a string, separating each element by token"
  (let ( 
        (car-element (car elements))
        (cdr-elements (cdr elements))
        )
    (if cdr-elements
        (concatenate 'string car-element token
                     (detokenize 
                      cdr-elements 
                      token
                      prefix 
                      )
                     )
        car-element
        )
    )
  )

(defun string-to-bytes (a-string &key (pad-to 1))
  "Borrowed from Arnesi (http://common-lisp.net/project/bese/arnesi.html)"
  (let ( 
        (padded-length (* pad-to (ceiling (length a-string) pad-to)))
        )
    (map-into (make-array padded-length :element-type '(unsigned-byte 8))
              #'char-code 
              a-string
              )
    )
  )
  
(defun bytes-to-string (some-bytes)
  "Borrowed from Arnesi (http://common-lisp.net/project/bese/arnesi.html)"
  (map-into (make-array (length some-bytes) :element-type 'character)
            #'code-char 
            some-bytes
            )
  )

;; collections

(defun select (some-list a-test)
  "Return a list containing only those elements from some list
  for which a-test is true
  "
  (let ( (results nil) )
    (mapcar  
     (lambda (v)
       (if (funcall a-test v)
           (setf results 
		 (append results (list v)) 
		 )
           )
       ) 
     some-list
     )  
    results
    )
  )

(defmacro putend (some-object some-place)
  "Append some-object to the end of the list in some-place.
   Modifies some-place.
   "
  `(setf ,some-place
         (nconc ,some-place
                 (list ,some-object)
                 )
         )
  )

(defmacro putendnew (some-object some-place
		     &key 
		     ((:test test) 'equal)
		     )
  `(if (not (member ,some-object ,some-place :test ,test) )
       (putend ,some-object ,some-place)
       )
  )

(defmacro union-ordered (some-list some-place)
  `(dolist (item ,some-list)
     (putendnew item
		,some-place
		:test 'equal
		)
     )
  )

;; hashtables

(defun hash-contents (some-hash)
  (let ( 
        (contents nil)
        )
    (maphash 
     (lambda (k v)
       (declare (ignorable k))
       (setq contents (cons v contents)) 
       )
     some-hash
     )
    contents
    )
  )

;; UUIDs

(defun uuid-string ()
  (string-trim '(#\Space #\NewLine #\Tab)
                                     (with-output-to-string (os)
                                       (print (uuid:make-v4-uuid) os)
                                       )
                                     )
  )

(defun generate-long-uuid-string (size)
  "Utility function for generating long strings of 
  random data, such as for nonces, salts, and session cookies
  "
  (cl-base64:string-to-base64-string 
   (with-output-to-string (os) 
     (dotimes (i size)
       (print (uuid:make-v4-uuid) os) 
       )
     )
   )
  )

(defun generate-base32-uuid-string ()
  (let ( 
        (uuid-bytes (uuid:uuid-to-byte-array (uuid:make-v4-uuid)))
        (rand-value (random #x100000000))
        (bytes (make-array '(20) :element-type '(unsigned-byte 8))) 
        )
    (dotimes (i 16)
      (setf (aref bytes i) (aref uuid-bytes i))
      )
    (dotimes (i 4)
      (setf (aref bytes (+ 16 i)) (ldb (byte 8 (* i 8)) rand-value))
      )
    (bytes-to-base32 bytes)
    )
  )