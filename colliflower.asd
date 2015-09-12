;;; colliflower.asd
;;;
;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(in-package :asdf-user)

(defsystem "colliflower"
  :description "Generic interfaces for collections and iterators."
  :version "0.1.1"
  :author "Thayne McCombs <bytecurry.software@gmail.com>"
  :license "MIT"
  :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  :depends-on ("liter" "garten" "colliflower/interface")
  :in-order-to ((test-op (test-op :colliflower-test))))
