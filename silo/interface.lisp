;;; interface.lisp
;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(uiop:define-package :silo/interface
    (:nicknames :silo :s)
  (:use :cl)
  (:use-reexport :silo/protocol
                 :silo/base))
