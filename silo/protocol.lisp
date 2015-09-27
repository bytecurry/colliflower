
;;; protocol.lisp
;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(uiop:define-package :silo/protocol
    (:use :cl)
  (:export #:sget
           #:supdate #:sdel
           #:ssetf
           #:define-sgetter
           #:slocation
           #:rget #:rget-apply))

(in-package :silo/protocol)

(defgeneric sget (object key &key)
  (:documentation "Generic get function. Provides a uniform way to
access properties of an object. The s stands for super, simple, or standard.

It is setfable as long as @c((setf sget)) is defined as well, which it should be
for mutable objects."))

(defgeneric (setf sget) (value object key &key)
  (:documentation "Generic set function. companion to sget.
SSET should mutate OBJECT.
Note that for some data types (ex lists, objects) this can't be used to set new values.
You may be able to use SUPDATE in those cases."))

(defgeneric supdate (object key value &key)
  (:documentation "Generic update function. Unlike (SETF SGET) it may or may not mutate
OBJECT. Data structures should define one or both of (SETF SGET) and SUPDATE.

SUPDATE MUST return the updated object.

For immutable data structures, SUPDATE can be used to create updated data structures.
It also works for prepending to lists such as plists or alists.")

  (:method (object key value &rest key-args &key)
    "Default implementation just calls SSET and returns OBJECT."
    (apply #'(setf sget) value object key key-args)
    object))

(defgeneric sdel (object key &key)
  (:documentation "Generic delete function. Companion to sget and sset.

Like SSET it MUST return the object with changes."))

(define-modify-macro ssetf (key value &rest key-args) supdate
                     "Modify macro that sets a place equal to the result of SUPDATE.")

(define-setf-expander slocation (place key &rest args &environment env)
  "SETF expander that uses SUPDATE to compute the new value to store in PLACE.

PLACE must be a SETFable place, and since this uses SUPDATE, it is safe for operations that
don't mutate PLACE directly (such as prepending to a list)."
  (multiple-value-bind (temps vals stores setter getter) (get-setf-expansion place env)
    (let ((keytemp  (gensym))
          (store    (gensym))
          (stemp    (first stores)))
      (values (cons keytemp temps)
              (cons key vals)
              (list store)
              `(let ((,stemp (supdate ,getter ,keytemp ,store ,@args)))
                 ,setter
                 ,store)
              `(sget ,getter ,keytemp ,@args)))))

(defmacro define-sgetter (lambda-list place-expr &key declarations documentation)
  "Define SGET and (SETF SGET) for a place to use sget with."
  (let ((value (gensym)))
    `(progn (defmethod sget ,lambda-list
              ,declarations
              ,documentation
              ,place-expr)
            (defmethod (setf sget) ,(list* value lambda-list)
              ,declarations
              ,documentation
              (setf ,place-expr ,value)))))

(defmacro rget (object &rest keys)
  "Recursive get. Calls sget for each key in turn from left to right.

As long as (SETF SGET) is defined for the final result, RGET is setfable."
  (let ((current object))
    (dolist (key keys current)
      (setf current (list 'sget current key)))))

(defun rget-apply (object &rest keys+)
  "A combination of RGET and APPLY. It repeatedly calls SGET on
successive results for each key, and the last argument is a list of keys
to use."
  (let ((keys (apply 'list* keys+))
        (current object))
    (dolist (key keys current)
      (setf current (sget current key)))))
