;;; base-test.lisp
;;; Copyright (c) 2015 Thayne McCombs

(defpackage liter/base-test
  (:use :cl :prove :liter/base :iterate))

(in-package :liter/base-test)


(defmacro is-iterator-over (got expected &optional description)
  `(is (iterator-list ,got) ,expected ,description))

(plan 10)

(is-error (end-iteration) 'iteration-ended)

;;; first test iterator-list, since is-iterator-over depends on it being
;;; right
(subtest "iterator-list"
  (is (iterator-list (get-iterator '(1 2 3 4 5)))
      '(1 2 3 4 5))
  (is (iterator-list (get-iterator #(1 2 3 4)))
      '(1 2 3 4)))

(subtest "function iterator"
  (let* ((x 1)
         (fn (lambda ()
               (prog1 x
                 (setf x (* 2 x)))))
         (it (get-iterator fn)))
    (is it fn :test #'eq "iterator for a function should be itself")
    (is (funcall it) 1)
    (is (funcall it) 2)
    (is (funcall it) 4)
    (is (funcall it) 8)))

(subtest "list iterator(s)"
  (is-error (funcall (get-iterator nil)) 'iteration-ended
            "iterator for nil should end immediately")
  (is-iterator-over (get-iterator nil) '())
  (is-iterator-over (get-iterator (list 1 2 3 #\a #\b))
                    (list 1 2 3 #\a #\b))
  (subtest "iterator over cyclic list"
    (let* ((cyclic '#1=(1 2 . #1#))
           (it (get-iterator cyclic)))
      (is (funcall it) 1)
      (is (funcall it) 2)
      (is (funcall it) 1)
      (is (funcall it) 2))))

(subtest "arrays"
  (is-iterator-over (get-iterator #(1 #\a :foo -10))
                    '(1 #\a :foo -10))
  (is-iterator-over (get-iterator #2a((1 0 1)
                                      (3 8 2)
                                      (9 5 10)))
                    '(1 0 1 3 8 2 9 5 10)))

(subtest "hash-tables"
  (let ((ht (make-hash-table)))
    (setf (gethash :foo ht) 10)
    (setf (gethash :bar ht) 20)
    (setf (gethash :cas ht) 30)
    (let ((it (get-iterator ht)))
      (loop repeat 3
         for (key . val) = (funcall it)
         do (is (gethash key ht) val))
      (is-error (funcall it) 'iteration-ended))))

(subtest "stream"
  (with-input-from-string (stream "abcZYZ")
    (is-iterator-over (get-iterator stream)
                      '(#\a #\b #\c #\Z #\Y #\Z)))
  (with-input-from-string (stream "first line
second line
third line")
    (is-iterator-over (make-line-iterator stream)
                      '("first line" "second line" "third line"))))

(subtest "do-iterator"
  (let (result)
    (do-iterator (elem (get-iterator #(1 2 3 4)))
      (push elem result))
    (is result '(4 3 2 1))))

(subtest "do-iterable"
  (is (with-output-to-string (stream)
        (do-iterable (letter "A string")
          (write-char letter stream)
          (write-char #\- stream)))
      "A- -s-t-r-i-n-g-"
      :test #'string=))

(subtest "iterate drivers"
  (is (iter (repeat 10)
            (for x in-iterator (constantly 2))
            (sum x))
      20)
  (is (iter (for x in-iterable #2a((1 2 3)
                                   (4 5 6)))
            (collect (* 2 x)))
      '(2 4 6 8 10 12)))

(finalize)
