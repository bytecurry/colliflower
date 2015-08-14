;;;; grower.lisp
;;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(uiop:define-package :garten/grower
    (:use :cl :liter/base)
  (:import-from :serapeum #:defalias)
  (:export #:make-grower
           #:feed #:feed-iterable #:feed*
           #:fruit #:grower-result
           #:reset-grower

           #:grower #:grower-state))

(in-package :garten/grower)

(defgeneric make-grower (type &key)
  (:documentation "Make a grower object that can be used to \"grow\"
a collection. The returned object must define the FEED method and in most
cases should define the FRUIT method and RESET-GROWER methods.

The TYPE argument is an object for which a MAKE-GROWER method is specialized, usually a symbol."))

(defgeneric feed (grower item)
  (:documentation "Add an item to the collection. If the collection needs a key-value
pair, ITEM should be a CONS cell with the key as the CAR and the value as the CDR."))

(defgeneric feed-iterable (grower iterable)
  (:documentation "Add all items from ITERABLE into GROWER. The default method
just iterates over iterable and calls FEED with each item.

The function is generic so that GROWERs can use more efficient methods if desired.")

  (:method (grower iterable)
    (do-iterable (item iterable)
      (feed item))))

(defgeneric fruit (grower)
  (:documentation "Return the final result of a grower.
A grower doesn't necessarily need this to be defined if there is another way to access the result,
but in most cases it should be supplied.

It is undefined what happens if this is called more than once for the same grower. Or more
specifically, the behavior depends on the grower implementation.")

  (:method (grower)
    "Default implementation is to just return the grower itself."
    grower))

(defgeneric reset-grower (grower)
  (:documentation "Reset a grower to an empty collection.

This can be used to recycle data structures if the result is no longer used.
But it is undefined whether it is safe to use the result of FRUIT after RESET-GROWER
has been called on the grower that generated it.

It also may not be defined for all growers."))

(defalias grower-result #'fruit
          "Alias for FRUIT.")

(declaim (inline feed*))
(defun feed* (grower &rest items)
  "Var-arg form of feed. Add multiple items at once."
  (feed-iterable grower items))

;;;; Grower class

(defclass grower ()
  ((grower-state :initarg :init-state :accessor grower-state))
  (:documentation "Base class for specialized growers.

Note that not all grower's will be subclasses of GROWER."))

(defmethod fruit ((grower grower))
  (grower-state grower))
