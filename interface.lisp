;;; interface.lisp
;;;
;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(uiop:define-package :colliflower/interface
    (:nicknames :colliflower :cf)
  (:documentation "Combination of the LITER, SILO, and GARTEN packages.")
  (:use-reexport :liter
                 ;:silo
                 :garten))
