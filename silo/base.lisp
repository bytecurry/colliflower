;;; base.lisp
;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(uiop:define-package :silo/base
    (:use :cl :silo/protocol)
  (:export #:idx-key
           #:alist-key))

(in-package :silo/base)

(define-sgetter ((object standard-object) key &key &allow-other-keys)
    (slot-value object key)
  :documentation "Access a slot on a standard-object.")

(define-sgetter ((object hash-table) key &key default)
    (gethash key object default))

(defmethod sdel ((object hash-table) key &key &allow-other-keys)
  "Delete an object from a hash-table."
  (remhash key object))

(define-sgetter ((object array) subscripts &key &allow-other-keys)
    (aref object subscripts))

(define-sgetter ((object list) key &key default)
    (getf object key default)
  :documentation "default implementation for lists. Assumes a plist")

(defmethod supdate ((object list) key value &key &allow-other-keys)
  (setf (getf object key) value)
  object)

;;; For alists and list indices we need a way to specifiy a different
;;; key.

(defclass %idx-ref () ((idx :initarg :idx :type integer)))
(defun idx-key (idx)
  (declare (integer idx))
  "Create a key to access lists by index"
  (make-instance '%idx-ref :idx idx))

(define-sgetter ((object list) (key %idx-ref) &key &allow-other-keys)
    (nth (slot-value key 'key) object)
  :documentation "Get element of list by index")

(defclass %alist-ref ()
  ((item :initarg :item)
   (key :initarg :key :type (or function symbol))
   (test :initarg :test :initform 'eql :type (or function symbol))))
(defun alist-key (item &key key (test 'eql))
  (declare (type (or function symbol) test key))
  "Create a key to access values in alists with sget"
  (make-instance '%alist-ref :item item :key key :test test))

(define-sgetter ((object list) (key %alist-ref) &key &allow-other-keys)
    (cdr (assoc (slot-value key 'item)
                object
                :key (slot-value key 'key)
                :test (slot-value key 'test)))
  :documentation "Get an element from an alist. Note that there is currently no
way to distinguish between a nil value and not found. Also, although SSET works
for keys that exist, it is not possible to add new keys, SUPDATE can though.")

(defmethod supdate ((object list) (key %alist-ref) value &key &allow-other-keys)
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
