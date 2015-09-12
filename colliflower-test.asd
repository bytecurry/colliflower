;;; colliflower-test.asd
;;;
;;; Copyright (c) 2015 Thayne McCombs <bytecurry.software@gmail.com>

(in-package :asdf-user)

(defsystem "colliflower-test"
  :description "Tests for colliflower"
  :version "0.1.0"
  :author "Thayne McCombs <bytecurry.software@gmail.com>"
  :license "MIT"
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:colliflower :prove)
  :components
  ((:module liter
            :pathname "t/liter"
            :components ((:test-file "base-test")))))
