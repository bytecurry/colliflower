;;; iter-object-test.lips
;;; Copyright (c) 2015 Thayne McCombs

(defpackage liter/iter-object-test
  (:use :cl :prove :liter)
  (:import-from :closer-mop
                #:funcallable-standard-class))

(in-package :liter/iter-object-test)

(plan nil)

(defclass test-iterator (iter-object)
  ((val :initform 0 :accessor test-iterator-value))
  (:metaclass funcallable-standard-class))

(defmethod iter-object-next ((obj test-iterator) &rest args)
  (declare (ignore args))
  (incf (test-iterator-value obj)))

(defmethod iter-object-prev ((obj test-iterator) &rest args)
  (declare (ignore args))
  (decf (test-iterator-value obj)))

(defmethod iter-object-end-p ((obj test-iterator))
  nil)

(subtest "funcall test-iterator"
  (diag "with closer-mop enabled")
  (let ((it (make-instance 'test-iterator)))
    (is (funcall it) 1)
    (is (inext it) 2)
    (is (get-iterator it) it :test #'eq "GET-ITERATOR should act as identity function.")))

(let* ((test-it (make-instance 'test-iterator))
       (it (get-iterator test-it)))
  (is (iter-object-next test-it) 1)
  (is (funcall it) 2)
  (is (iter-object-prev test-it) 1)
  (is (iter-object-prev test-it) 0))

(finalize)
