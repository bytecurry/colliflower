;;; iter-object.lisp
;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(uiop:define-package :liter/iter-object
    (:nicknames :liter-object)
  (:use :cl)
  #+closer-mop
  (:import-from :closer-mop
                #:funcallable-standard-class
                #:set-funcallable-instance-function)
  (:export #:iter-object
           #:iter-object-next
           #:iter-object-prev
           #:iter-object-end-p))

(in-package :liter/iter-object)

(defclass iter-object ()
  ()
  #+closer-mop
  (:metaclass funcallable-standard-class)
  (:documentation "A class to represent an iterator object.

If closer-mop is enabled, it is a funcallable object that calls ITER-OBJECT-NEXT when called.
Otherwise GET-ITERATOR returns a function that calls ITER-OBJECT-NEXT"))

#+closer-mop
(defmethod initialize-instance :after ((object iter-object) &key &allow-other-keys)
  (set-funcallable-instance-function object (lambda (&rest args)
                                              (apply #'iter-object-next object args))))
#-closer-mop
(defmethod get-iterator ((object iter-object))
  (lambda (&rest args)
    (apply #'iter-object-next object args)))

(defgeneric iter-object-next (iter-object &rest args)
  (:documentation "Get the next object in the iter-object iterator."))

(defgeneric iter-object-prev (iter-object &rest args)
  (:documentation "Get the previous object in the iter-object iterator.

This is optional for iter-object implementations."))

(defgeneric iter-object-end-p (iter-ojbect)
  (:documentation "Predicate to test if the iterator has ended."))
