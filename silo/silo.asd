;;; silo.asd
;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>
(in-package :asdf-user)

(defsystem "silo"
  :description "Generic getters and setters for data structures."
  :version "0.1.1"
  :author "Thayne McCombs <bytecurry.software@gmail.com"
  :license "MIT"
  :class :package-inferred-system
  :depends-on ("silo/interface"))
