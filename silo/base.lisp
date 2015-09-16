;;; base.lisp
;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(uiop:define-package :silo/base
    (:use :cl :silo/protocol)
  (:export #:plist-key
           #:alist-key))

(in-package :silo/base)

(define-sgetter ((object standard-object) key &key)
    (slot-value object key)
  :documentation "Access a slot on a standard-object.")

(define-sgetter ((object hash-table) key &key default)
    (gethash key object default))

(defmethod sdel ((object hash-table) key &key)
  "Delete an object from a hash-table."
  (remhash key object))


(define-sgetter ((object vector) (index integer) &key)
    (aref object index))

(define-sgetter ((object array) (subscripts list) &key)
    (apply #'aref object subscripts))

(define-sgetter ((object list) (key integer) &key)
    (nth key object)
  :documentation "default implementation for lists. Assumes a using an index")

;;; For alists and plists we need a way to specifiy a different
;;; key.

(defclass %plist-ref ()
  ((key-item :initarg :key-item)))
(defun plist-key (key-item)
  "Create a key to access an item in a plist"
  (make-instance '%plist-ref :key-item key-item))

(define-sgetter ((object list) (key %plist-ref) &key default)
    (getf object (slot-value key 'key-item) default)
  :documentation "Get an element from a plist. Not that although (setf sget) works
on an existing value, supdate is needed to add a new value and save the result.")

(defmethod supdate ((object list) (key %plist-ref) value &key)
  (setf (getf object (slot-value key 'key-item)) value)
  object)

(defclass %alist-ref ()
  ((item :initarg :item)
   (key :initarg :key :type (or function symbol))
   (test :initarg :test :initform 'eql :type (or function symbol))))
(defun alist-key (item &key key (test 'eql))
  "Create a key to access values in alists with sget"
  (declare (type (or function symbol) test key))
  (make-instance '%alist-ref :item item :key key :test test))

(define-sgetter ((object list) (key %alist-ref) &key)
    (cdr (assoc (slot-value key 'item)
                object
                :key (slot-value key 'key)
                :test (slot-value key 'test)))
  :documentation "Get an element from an alist. Note that there is currently no
way to distinguish between a nil value and not found. Also, although SSET works
for keys that exist, it is not possible to add new keys, SUPDATE can though.")

(defmethod supdate ((object list) (key %alist-ref) value &key)
  "For an ALIST supdate will either replace an existing value, or add a new cons pair
to the front of the list if the associated key isn't already in the list."
  (let* ((key-item (slot-value key 'item))
         (cell (assoc key-item
                      object
                      :key (slot-value key 'key)
                      :test (slot-value key 'test))))
    (if cell
        (progn
          (setf (cdr cell) value)
          object)
        (cons (cons key-item value) object))))
