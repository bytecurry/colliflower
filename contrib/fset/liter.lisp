(defpackage colliflower-fset/liter
  (:use :cl :liter))

(in-package :colliflower-fset/liter)


(defmethod get-iterator ((coll fset:collection))
  (let ((fit (fset:iterator coll)))
    (lambda ()
      (multiple-value-bind (item more-p) (funcall fit :get)
        (unless more-p
          (end-iteration))
        item))))

(defmethod get-iterator ((coll fset:map))
  (let ((fit (fset:iterator coll)))
    (lambda ()
      (multiple-value-bind (key value more-p) (funcal fit :get)
        (unless more-p
          (end-iteration))
        (cons key value)))))

(defmethod get-iterator ((coll fset:bag))
  (let ((fit (fset:iterator coll :pairs? t)))
    (lambda ()
      (multiple-value-bind (item count more-p) (funcal fit :get)
        (unless more-p
          (end-iteration))
        (cons item count)))))
