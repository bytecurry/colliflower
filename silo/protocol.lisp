;;; protocol.lisp
;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(uiop:define-package :silo/protocol
    (:use :cl)
  (:export #:sget #:sset
           #:define-sgetter
           #:rget #:rget-apply))

(in-package :silo/protocol)

(defgeneric sget (object key &key &allow-other-keys)
  (:documentation "Generic get function. Provides a uniform way to
access properties of an object. The s stands for super, simple, or standard"))

(defgeneric sset (object key value &key &allow-other-keys)
  (:documentation "Generic set function. companion to sget.

sset MUST return the object that was set, and depending on the type, the
object passed in may or may not contain the new information (for example immutable
data structures or plists)."))

(defgeneric sdel (object key &key &allow-other-keys)
  (:documentation "Generic delete function. Companion to sget and sset.

Like SSET it MUST return the object with changes."))

(define-setf-expander sget (place key &rest args &environment env)
  "SETF expander for SGET. Uses SSET to compute the new value to store in PLACE."
  (multiple-value-bind (temps vals stores setter getter) (get-setf-expansion place env)
    (let ((keytemp  (gensym))
          (store    (gensym))
          (stemp    (first stores)))
      (values (cons keytemp temps)
              (cons key vals)
              (list store)
              `(let ((,stemp (sset ,getter ,keytemp ,store ,@args)))
                 ,setter
                 ,store)
              `(sget ,getter ,@args)))))

(defmacro define-sgetter ((&whole lambda-list obj-spec key &rest params) place-expr &key declarations documentation)
  "Define a getter and setter for a place to use sget with"
  (let ((value (gensym))
        (obj (if (atom obj-spec)
                 obj-spec
                 (car obj-spec))))
    `(progn (defmethod sget ,lambda-list
              ,declarations
              ,documentation
              ,place-expr)
            (defmethod sset ,(list* obj-spec key value params)
              ,declarations
              ,documentation
              (setf ,place-expr ,value)
              ,obj))))

(defmacro rget (object &rest keys)
  "Recursive get. Calls sget for each key in turn from left to right.

As long as SSET is defined at each step, RGET is setfable."
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
