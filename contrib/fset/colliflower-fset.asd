;;; colliflower-fset.asd
;;; Copyright (c) 2015 Thayne McCombs

(in-package :asdf-user)

(defsystem colliflower-fset
  :description "Implementation of colliflower for fset."
  :version "0.1.0"
  :author "Thayne McCombs"
  :license "MIT"
  :depends-on ("fset" "colliflower")
  :components ((:file "liter")
               (:file "garten")
               (:file "silo")))
