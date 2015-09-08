;;;; base.lisp
;;;;
;;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(uiop:define-package :garten/base
    (:use :cl :liter/base)
  (:import-from :serapeum #:defalias)
  (:use-reexport :garten/grower
                 :garten/list)
  (:export #:*default-grower-size))

(in-package :garten/base)

(defparameter *default-grower-size* 8
  "The default initial size to use when creating a grower that needs
an initial size, such as a vector.")


;;; Implementations:

;;; Vector

(defmethod make-grower ((type (eql 'vector)) &key (size *default-grower-size*)
                                               (element-type t) (adjustable t))
  "Create a growable vector. @cl:param(size) is the initial size of the vector to grow,
@cl:param(element-type) is the element-type of the vector. And if @cl:param(adjustable)
is false, then the vector cannot be grown beyond SIZE.

The resulting vector will have a fill pointer."
  (make-array size :element-type element-type :adjustable adjustable :fill-pointer 0))

(defmethod feed ((grower vector) item)
  (vector-push-extend item grower))

(defmethod reset-grower ((grower vector))
  (setf (fill-pointer grower) 0))

;;; Strings and streams

(defmethod make-grower ((type (eql 'string)) &key (element-type 'character))
  "Create a grower for a string. @cl:param(element-type) is the element type
of the resulting string."
  (make-string-output-stream :element-type element-type))

(defmethod feed ((grower stream) (item string))
  (write-string item grower))
(defmethod feed ((grower stream) (item character))
  (write-char item grower))
(defmethod feed ((grower stream) (item integer))
  (write-byte item grower))
(defmethod feed ((grower stream) (item sequence))
  (write-sequence item grower))

(defmethod feed-iterable ((grower stream) (seq vector))
  (if (subtypep (array-element-type seq) (stream-element-type grower))
      (write-sequence seq grower)
      (call-next-method)))

(defmethod fruit ((grower string-stream))
  (get-output-stream-string grower))

(defmethod reset-grower ((grower string-stream))
  (get-output-stream-string grower))


;;; Hash tables

(defmethod make-grower ((type (eql 'hash-table)) &rest args &key)
  "Create a grower for a hash-table. Any additional arguments will be passed
through to MAKE-HASH-TABLE."
  (apply 'make-hash-table args))

(defmethod feed ((grower hash-table) (item cons))
  (setf (gethash (car item) grower) (cdr item)))

(defmethod reset-grower ((grower hash-table))
  (clrhash grower))
