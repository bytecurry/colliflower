;;;; garten.asd
;;;;
;;;; Copyright (c) 2015 Thayne McCombs <bytecurry.software@gmail.com>

(asdf:defsystem #:garten
    :description "Generic interface for \"growing\" data structures/collections."
    :version "0.1.0"
    :author "Thayne McCombs <bytecurry.software@gmail.com"
    :class :package-inferred-system
    :depends-on ("garten/interface"))
