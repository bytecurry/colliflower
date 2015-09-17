;;; generate-test.lisp
;;; Copyright (c) 2015 Thayne McCombs

(defpackage liter/generate-test
  (:use :cl :prove :liter/base :liter/generate :iterate))

(in-package :liter/generate-test)

(plan 8)


(subtest "icounter"
  (diag "No arguments")
  (let ((c (icounter)))
    (is (funcall c) 0)
    (is-values (inext c) '(1 t))
    (is (funcall c) 2)
    (is (funcall c t) 0)
    (is (funcall c) 1))
  (diag "from 10 to 20 by 2")
  (let ((c (icounter :from 10 :by 2 :to 20)))
    (is (iterator-list c) '(10 12 14 16 18) :test #'equalp))
  (diag "from 3 by -1")
  (let ((c (icounter :from 3 :by -1)))
    (is (funcall c) 3)
    (is (funcall c) 2)
    (is (inext c) 1)
    (is (inext c) 0)
    (is (inext c) -1)))

(subtest "irepeat"
  (diag "No limit")
  (let ((it (irepeat 1)))
    (is (iter (repeat 10)
              (collect (funcall it)))
        '(1 1 1 1 1 1 1 1 1 1)))
  (diag "limit to 4")
  (is (iter (for x in-iterator (irepeat (+ 2 1) 4))
            (sum x))
      (* 3 4)))

(subtest "singleton-iterator"
  (let ((it (singleton-iterator :foo)))
    (is (funcall it) :foo)
    (is-error (funcall it) 'iteration-ended)))

(subtest "make-iterator"
  (let* ((i 3)
         (it (make-iterator
               (format t ":~a" i)
               (decf i)
               (if (minusp i)
                   (end-iteration)
                   :foo))))
    (is-print (funcall it) ":3")
    (is-print (funcall it) ":2")
    (is (funcall it) :foo)
    (is-error (funcall it) 'iteration-ended)))

(subtest "icycle"
  (let ((it (icycle #(1 2 3))))
    (iter (repeat 4)
          (funcall it))
    (is (funcall it) 2)
    (is (funcall it) 3)
    (is (funcall it) 1))
  (let ((it (icycle (icounter :from 0 :to 2))))
    (is (funcall it) 0)
    (is (funcall it) 1)
    (is (funcall it) 0)))

(subtest "icycle*"
  (let ((it (icycle* :foo :bar :cat)))
    (is (funcall it) :foo)
    (is (funcall it) :bar)
    (is (funcall it) :cat)
    (is (funcall it) :foo)))

(defun next-number (n)
  (* 2 (1+ n)))

(subtest "make-state-iterator"
  (let ((it (make-state-iterator #'next-number 0)))
    (is (inext it) 2)
    (is (inext it) 6)
    (is (inext it) 14)
    (is (inext it) 30)))

(subtest "make-state-iterator*"
  (let ((it (make-state-iterator* nil (last &optional v)
                (cons v last))))
    (is (inext it) '(nil))
    (is (inext it :foo) '(:foo nil))
    (is (inext it 10) '(10 :foo nil))))

(finalize)
