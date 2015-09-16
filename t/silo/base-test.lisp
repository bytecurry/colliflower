;;; base-test.lisp
;;; Copyright (c) 2015 Thayne McCombs

(defpackage silo/base-test
  (:use :cl :prove :silo))

(in-package :silo/base-test)

(defclass test-class ()
  ((a :initarg :a)
   (b :initarg :b)
   (c :initarg :c)))

(plan nil)

(subtest "standard-object"
  (let ((obj (make-instance 'test-class)))
    (is (setf (sget obj 'a) 10) 10)
    (is (slot-value obj 'a) 10)
    (is (sget obj 'a) 10)
    (is (supdate obj 'b 4) obj)
    (is (sget obj 'b) 4)
    (setf (slocation obj 'c) '(1 2 4))
    (is (sget obj 'c) '(1 2 4))))

(subtest "hash-table"
  (let ((ht (make-hash-table)))
    (is (setf (sget ht :foo) 10) 10)
    (is (gethash :foo ht) 10)
    (is (sget ht :foo) 10)
    (is (supdate ht :bar 3) ht)
    (is (sget ht :bar) 3)
    (setf (slocation ht :name) "Bob")
    (is (sget ht :name) "Bob" :test #'string=)
    (sdel ht :foo)
    (is-values (sget ht :foo) '(nil nil))))

(subtest "array"
  (subtest "vector"
    (let ((vec (make-array 5 :initial-element nil)))
      (is (sget vec 1) nil)
      (is (setf (sget vec 0) 1) 1)
      (is (sget vec 0) 1)
      (is (supdate vec 3 :three) vec)
      (is (sget vec 3) :three)
      (is (setf (slocation vec 2) "two") "two" :test #'string=)
      (is (sget vec 2) "two" :test #'string=)
      (is (setf (sget vec '(4)) 9) 9)
      (is (sget vec '(4)) 9)))
  (subtest "multi-dimensional array"
    (let ((a (make-array '(2 2) :element-type 'integer)))
      (is (sget a '(0 0)) 0)
      (is (setf (sget a '(0 0)) 1) 1)
      (is (sget a '(0 0)) 1)
      (is (supdate a '(0 1) 2) a)
      (is (sget a '(0 1)) 2)
      (is (setf (slocation a '(1 0)) 3) 3)
      (is (sget a '(1 0)) 3))))

(subtest "lists"
  (subtest "index"
    (let ((l (list 1 2 3 4 5)))
      (is (sget l 0) 1)
      (is (sget l 4) 5)
      (is (setf (sget l 1) :one) :one)
      (is (sget l 1) :one)
      (is (supdate l 3 :three) l :test #'eq)
      (is (sget l 3) :three)
      (is (setf (slocation l 2) :two) :two)
      (is (sget l 2) :two)))
  (subtest "plist"
    (let ((p (list :foo 5 :bar :car)))
      (is (sget p (plist-key :foo)) 5)
      (is (setf (sget p (plist-key :bar)) 8) 8)
      (is (sget p (plist-key :bar)) 8)
      (is (supdate p (plist-key :new) 80) '(:new 80 :foo 5 :bar 8) :test #'equalp)
      (is (setf (slocation p (plist-key :new)) 79) 79)
      (is (sget p (plist-key :new)) 79)
      (is p '(:new 79 :foo 5 :bar 8) :test #'equalp)))
  (subtest "alist"
    (let ((a '((:foo . 5) (1 . :car))))
      (is (sget a (alist-key :foo)) 5)
      (is (sget a (alist-key 1.0 :test #'equalp)) :car)
      (is (setf (sget a (alist-key "FOO" :key (lambda (k)
                                                (if (symbolp k)
                                                    (symbol-name k)
                                                    ""))
                                   :test #'string=))
                0)
          0)
      (is (sget a (alist-key :foo)) 0)
      (is (supdate a (alist-key :new) 'integer) '((:new . integer) (:foo . 0) (1 . :car)) :test #'equalp)
      (is (setf (slocation a (alist-key :new)) 'integer) 'integer)
      (is a '((:new . integer) (:foo . 0) (1 . :car)) :test #'equalp))))

(finalize)
