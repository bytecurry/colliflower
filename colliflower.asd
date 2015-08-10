;;; colliflower.asd
;;;
;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(in-package :asdf-user)

(defsystem "colliflower"
  :description "Generic interfaces for collections and iterators."
  :version "0.1.0"
  :author "Thayne McCombs <bytecurry.software@gmail.com>"
  :license "MIT"
  :class :package-inferred-system
  :depends-on ("liter" "garten" "colliflower/interface"))
