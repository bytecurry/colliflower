;;; tools-test.lisp
;;; Copyright (c) 2015 Thayne McCombs

(defpackage garten/tools-test
  (:use :cl :prove :garten :iterate))

(in-package :garten/tools-test)

(plan 2)

(subtest "with-grower"
  (is (with-grower (g 'string)
        (g "Hello")
        (g #\space)
        (g "World")
        (g "!"))
      "Hello World!" :test #'string=))

(subtest "growing iterate clause"
  (is (iter (for x from 1 to 10)
            (growing 'vector from (* x x)))
      #(1 4 9 16 25 36 49 64 81 100) :test #'equalp)
  (is-values (iter (for x from 1 to 5)
                   (growing 'string from (format nil "~a, " x) into s)
                   (growing 'list from x into l with (:prepend t))
                   (finally (return (values (fruit s) (fruit l)))))
      '("1, 2, 3, 4, 5, "  (5 4 3 2 1))
      :test #'equalp)
  (is (iter (for x from 1 to 10)
            (growing 'vector from (logand 1  x) with (:element-type 'bit)))
      #*1010101010
      :test #'equalp))

(finalize)
