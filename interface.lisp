;;; interface.lisp
;;;
;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(uiop:define-package :colliflower/interface
    (:nicknames :colliflower :cf)
  (:documentation "Combination of the LITER, SILO, and GARTEN packages.")
  (:use-reexport :liter
                 :silo
                 :garten)
  (:export #:grow-from-iterable))

(defun grow-from-iterable (iterable type &rest type-args)
  "Create a growable of type TYPE from an iterable.

TYPE-ARGS are additional arguments to pass to MAKE-GROWER after the type."
  (let ((grower (apply #'make-grower type type-args)))
    (feed-iterable grower iterable)
    (fruit grower)))
