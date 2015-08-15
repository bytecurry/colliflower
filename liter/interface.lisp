;;; interface.lisp
;;;
;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(uiop:define-package :liter/interface
  (:nicknames :liter :iterator)
  (:documentation "Liter is a collection of tools to work with basic iterators.
An iterator in Liter is just a function (usually a closure) which returns
the next value in the iterable, and signals a ITERATION-ENDED condition
if there are no more values.")
  (:use-reexport :liter/base
                 :liter/generate
                 :liter/tools
                 #+closer-mop :liter/iter-object))
