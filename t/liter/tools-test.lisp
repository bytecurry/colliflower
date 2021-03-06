;;; tools-test.lisp
;;; Copyright (c) 2015 Thayne McCombs

(defpackage liter/tools-test
  (:use :cl :prove :liter))

(in-package liter/tools-test)

(plan 10)

(subtest "itransform"
  (let ((it (itransform (icounter)
                        (lambda (x)
                          (* 2 x)))))
    (is (inext it) 0)
    (is (inext it) 2)
    (is (inext it) 4)
    (is (inext it) 6))
  (let ((it (itransform  #(1 7 8 9 10 4 -2)
                         (lambda (x)
                           (* x x)))))
    (is (iterator-list it) '(1 49 64 81 100 16 4))))

(subtest "ifilter"
  (let ((it (ifilter (icounter) #'oddp)))
    (is (inext it) 1)
    (is (inext it) 3)
    (is (inext it) 5)
    (is (inext it) 7))
  (let ((it (ifilter (icounter :to 10) #'evenp)))
    (is (iterator-list it) '(0 2 4 6 8))))

(subtest "ifold"
  (is (ifold (icounter :to 5) #'+ 0) 10)
  (is (ifold (irepeat 2 10) #'* 1) (expt 2 10))
  (is (ifold #(1 2 3) #'(lambda (x y)
                          (cons y x)))
      '(3 2 1)))

(subtest "iaccumulate"
  (is (iterator-list
       (iaccumulate (icounter :to 5) #'+))
      '(0 1 3 6 10))
  (let* ((n 0)
         (it (iaccumulate (icounter :from 1)
                          (lambda (prev curr) ;;cumulative average
                            (incf n)
                            (/ (+ curr (* n prev)) (1+ n))))))
    (is (inext it) 1)
    (is (inext it) 3/2)
    (is (inext it) 2)
    (is (inext it) 5/2)
    (is (inext it) 3)))

(subtest "ichain"
  (is (iterator-list (ichain '(1 2 3)
                             (irepeat 4 2)
                             (singleton-iterator 8)))
      '(1 2 3 4 4 8))
  (let ((it (ichain (icounter :from 3 :by -1 :to 0)
                    (irepeat nil))))
    (is (inext it) 3)
    (is (inext it) 2)
    (is (inext it) 1)
    (is (inext it) nil)
    (is (inext it) nil)))

(subtest "izip"
  (is (iterator-list (izip '(1 2 3 4 5) (irepeat :foo 5)))
      '((1 :foo) (2 :foo) (3 :foo) (4 :foo) (5 :foo))
      "same length")
  (is (iterator-list (izip #(a b c) (icounter :by 2) #(6 7.2 9.3 10.0)))
      '((a 0 6) (b 2 7.2) (c 4 9.3)) "stop at shortest")
  (diag "infinite")
  (let ((it (izip (icounter) (icounter :by 2) (icounter :by 3))))
    (is (inext it) '(0 0 0))
    (is (inext it) '(1 2 3))
    (is (inext it) '(2 4 6))
    (is (inext it) '(3 6 9))))

(subtest "izip-longest"
  (is (iterator-list (izip-longest nil '(1 2 3 4) #(1 2 3)))
      '((1 1) (2 2) (3 3) (4 nil)))
  (is (iterator-list (izip-longest 0 (irepeat 1 3) (icounter :to 5)))
      '((1 0) (1 1) (1 2) (0 3) (0 4)))
  (let ((it (izip-longest :not '(:foo :bar) (icounter :by 2))))
    (is (inext it) '(:foo 0))
    (is (inext it) '(:bar 2))
    (is (inext it) '(:not 4))
    (is (inext it) '(:not 6))))

(subtest "izip-with-index"
  (is (iterator-list (izip-with-index #(7 8 9 1)))
      '((0 7) (1 8) (2 9) (3 1)))
  (let ((it (izip-with-index (icycle* 'a 'b))))
    (is (inext it) '(0 a))
    (is (inext it) '(1 b))
    (is (inext it) '(2 a))
    (is (inext it) '(3 b))))

(subtest "itee"
  (let ((c (icounter)))
    (multiple-value-bind (i1 i2) (itee c)
      (is (inext i1) 0)
      (is (inext i1) 1)
      (is (inext i1) 2)
      (is (inext i2) 0)
      (is (inext i2) 1)
      (is (inext i2) 2)
      (is (inext i2) 3)
      (is (inext i1) 3)))
  (multiple-value-bind (one two three) (itee '(:foo :bar :car) 3)
    (is (iterator-list one) '(:foo :bar :car))
    (is-error (funcall one) 'iteration-ended)
    (is (funcall two) :foo)
    (is (funcall three) :foo)
    (is (iterator-list two) '(:bar :car))
    (is (iterator-list three) '(:bar :car))))

(subtest "take and drop"
  (subtest "itake"
    (is (iterator-list (itake (icounter) 5))
        '(0 1 2 3 4))
    (is (iterator-list (itake '(1 2 3) 5))
        '(1 2 3))
    (is (iterator-list (itake '(1 2 3) 1))
        '(1))
    (is (iterator-list (itake (irepeat :foo) 0))
        nil))
  (subtest "itake-while"
    (is (iterator-list (itake-while (icounter :from 1) (lambda (n)
                                                   (< n 6))))
        '(1 2 3 4 5))
    (is (iterator-list (itake-while #(1 2 3) (constantly t)))
        '(1 2 3))
    (is (iterator-list (itake-while #(1 2 3) (constantly nil)))
        nil)
    (is (iterator-list (itake-while #(1 3 6 9 2 4 8) #'oddp))
        '(1 3)))
  (subtest "idrop"
    (is (iterator-list (idrop (icounter :to 10) 6))
        '(6 7 8 9))
    (is (iterator-list (idrop #(1 2 3) 0))
        '(1 2 3))
    (is (iterator-list (idrop #(1 2) 10))
        nil)
    (let ((it (idrop (icycle* 1 2 3) 5)))
      (is (inext it) 3)
      (is (inext it) 1)))
  (subtest "idrop-while"
    (is (iterator-list (idrop-while #(1 3 7 8 2 1 9) #'oddp))
        '(8 2 1 9))
    (is (iterator-list (idrop-while #(1 2 4 8) #'evenp))
        '(1 2 4 8))
    (is (iterator-list (idrop-while '(:foo :bar) (constantly t)))
        nil)
    (let ((it (idrop-while (icounter :from 1) (lambda (n)
                                                (< n 6)))))
      (is (inext it) 6)
      (is (inext it) 7))))

(finalize)
