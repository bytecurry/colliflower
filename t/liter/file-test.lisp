;;; filet-test.lisp
;;; Copyright (c) Thayne McCombs

(defpackage liter/file-test
  (:use :cl :prove :liter))

(in-package liter/file-test)

(defparameter *file-path* (asdf:component-pathname
                           (asdf:find-component :colliflower-test
                                                '("liter" "test.txt"))))
(defparameter *content-bytes*
  '(102 105 114 115 116 10 115 101 99 111 110 100 10 116 104 105 114 100 10))

(defparameter *content-characters* (coerce "first
second
third
" 'list))

(plan nil)

(subtest "character"
  (with-file-iterator (it *file-path* :element-type 'character)
    (is (iterator-list it)
        *content-characters*
        :test #'equalp
        "Read the fle as characters"))
  (let* ((f (make-file-iterator *file-path*))
        (it (get-iterator f)))
    (is (iter-object-next f) #\f)
    (is (iter-object-next f) #\i)
    (is (funcall it) #\r)
    (is (inext it) #\s)
    (is (inext it) #\t)
    (is (inext it) #\newline)
    (is (coerce (iterator-list it) 'string)
        "second
third
" "Iterator should match rest of string.")
    (ok (iter-object-end-p f) "Iterator should be ended.")))


(subtest "bytes"
  (with-file-iterator (it *file-path* :element-type '(unsigned-byte 8))
    (is (iterator-list it) *content-bytes* "Read the file as bytes")))

(subtest "lines"
  (with-file-iterator (it *file-path* :reader #'read-line)
    (is (iterator-list it) '("first" "second" "third") "Read the file as lines")))

(finalize)
