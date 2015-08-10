;;;; tools.lisp
;;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(uiop:define-package :garten/tools
    (:use :cl :garten/grower)
  (:import-from :alexandria :with-gensyms)
  (:export #:with-grower))

(in-package :garten/tools)


(defmacro with-grower ((feeder type &rest args) &body body)
  "Execute BODY where FEEDER is a function that takes a single argument, which is added
to the resulting data structure.
TYPE is the type of the structure to create. And ARGS are passed through to MAKE-GROWER.

WITH-GROWER returns the result (fruit) of the grower."
  (with-gensyms (grower item)
    `(let ((,grower (make-grower ,type ,@args)))
       (flet ((,feeder (,item)
                (feed ,grower ,item)))
         ,@body)
       (fruit ,grower))))
