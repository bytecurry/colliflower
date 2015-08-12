;;;; liter.asd
;;;;
;;;; Copyright (c) 2015 Thayne McCombs <bytecurry.software@gmail.com>
(in-package :asdf-user)

(defsystem "liter"
  :description "Collection of tools for basic iterators"
  :version "0.1.0"
  :author "Thayne McCombs <bytecurry.software@gmail.com>"
  :license "MIT"
  :class :package-inferred-system
  :depends-on ("liter/interface"))
