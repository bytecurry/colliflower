;;;; tools.lisp
;;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(uiop:define-package :garten/tools
    (:use :cl :garten/grower :iterate)
  (:import-from :alexandria #:with-gensyms)
  (:export #:with-grower
           #:growing #:feeding))

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

(defmacro-clause (GROWING type FROM expr &optional INTO var WITH grower-args)
  "Iterate clause similar to COLLECT but can grow any growable structure.

TYPE is the type of growable to create. If TYPE is a symbol it doesn't need to be quoted.

If provided, GROWER-ARGS should be a plist of arguments to pass to MAKE-GROWER
after the TYPE parameter.

Note that VAR will contain the grower object, not the end result. If you store
the grower in a variable it is up to you to call FRUIT on the grower when needed.

EXPR is the actual expression to store in the grower."
  (let ((grower (or var iterate::*result-var*)))
    (when (symbolp type)
      (setf type (list 'quote type)))
    `(progn
       (with ,grower = (make-grower ,type ,@grower-args))
       (feed ,grower ,expr))))

(defsynonym feeding growing)
