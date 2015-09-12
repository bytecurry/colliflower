;;; base-test.lisp
;;; Copyright (c) 2015 Thayne McCombs

(defpackage garten/base-test
  (:use :cl :prove :garten))

(in-package :garten/base-test)

(plan 7)

(subtest "vector"
  (let ((grower (make-grower 'vector)))
    (feed grower 1)
    (feed grower #\a)
    (feed grower :foo)
    (feed-iterable grower '(6 7 8))
    (feed* grower "hello" "there")
    (is (fruit grower) #(1 #\a :foo 6 7 8 "hello" "there") :test #'equalp))
  (let ((grower (make-grower 'vector :element-type 'fixnum  :size 10)))
    (is (array-element-type grower) 'fixnum)
    (is (array-total-size grower) 10)
    (is (fill-pointer grower) 0)
    (feed grower 1)
    (feed grower 2)
    (feed grower -1)
    (feed-iterable grower '(6 7 8))
    (feed* grower -2 -5)
    (is-error (feed grower :foo) 'type-error)
    (is (fruit grower) #(1 2 -1 6 7 8 -2 -5) :test #'equalp)))

(subtest "string"
  (let ((grower (make-grower 'string)))
    (feed grower #\A)
    (feed grower #\SPACE)
    (feed grower "String ")
    (feed* grower "with" #\SPACE "space")
    (feed-iterable grower ". blah")
    (is (fruit grower) "A String with space. blah") :test #'string=))

(subtest "hash-table"
  (let ((grower (make-grower 'hash-table :test #'equalp))
        result)
    (feed grower '("a" . 78))
    (feed grower '("b" . :foo))
    (feed grower '("nil" . nil))
    (feed grower '(:potato . (1 2 3 4)))
    (setf result (fruit grower))
    (is (hash-table-test result) 'equalp)
    (is (gethash "a" result) 78)
    (is (gethash "b" result) :foo)
    (is-values (gethash "nil" result) '(nil t))
    (is (gethash :potato result) '(1 2 3 4))))

(subtest "appending list"
  (let ((g (make-grower 'list)))
    (feed g :foo)
    (feed g :bar)
    (feed g 34)
    (feed g #c(0 2))
    (is (fruit g) '(:foo :bar 34 #c(0 2)))))

(subtest "prepending list"
  (let ((g (make-grower 'list :prepend t)))
    (feed g :foo)
    (feed g :bar)
    (feed g 34)
    (feed g #c(0 2))
    (is (fruit g) '(#c(0 2) 34 :bar :foo))))

(subtest "plist"
  (let ((g (make-grower :plist :default :undefined))
        plist)
    (feed g '(:foo . 6))
    (feed g :something)
    (feed g '(:name . "Bob"))
    (feed g '(:foo . 10))
    (setf plist (fruit g))
    (is (getf plist :name) "Bob" :test #'string=)
    (is (getf plist :something) :undefined)
    (is (getf plist :foo) 10)))

(subtest "alist"
  (let ((g (make-grower :alist))
        alist)
    (feed g '(1 . 0))
    (feed g '(2 . 4))
    (feed g '(3 . 9))
    (feed g '(1 . 1))
    (setf alist (fruit g))
    (is (assoc 1 alist) '(1 . 1))
    (is (assoc 2 alist) '(2 . 4))
    (is (assoc 3 alist) '(3 . 9))))

(finalize)
