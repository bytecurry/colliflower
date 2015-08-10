;;;; iterface.lisp
;;;;
;;;; Copyright (c) 2015 Thayne McCombs <bytecurry.software@gmail.com>

(uiop:define-package :garten/interface
    (:nicknames :garten :grow)
  (:documentation "Garten is a library that provides a generic interface
to build collections.")
  (:use-reexport :garten/base
                 :garten/tools))
