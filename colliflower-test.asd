;;; colliflower-test.asd
;;;
;;; Copyright (c) 2015 Thayne McCombs <bytecurry.software@gmail.com>

(in-package :asdf-user)

(defsystem "colliflower-test"
  :description "Tests for colliflower"
  :version "0.2.0"
  :author "Thayne McCombs <bytecurry.software@gmail.com>"
  :license "MIT"
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:colliflower :prove)
  :components
  ((:module liter
            :pathname "t/liter"
            :components ((:test-file "base-test")
                         (:test-file "generate-test")
                         (:test-file "tools-test")
                         (:test-file "iter-object-test")
                         (:test-file "file-test")
                         (:static-file "test.txt")))
   (:module garten
            :pathname "t/garten"
            :components ((:test-file "base-test")
                         (:test-file "tools-test")))
   (:module silo
            :pathname "t/silo"
            :components ((:test-file "base-test"))))
  :perform (test-op (op c)
                    (uiop:symbol-call :prove '#:run c)))
