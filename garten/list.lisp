;;;; list.lisp
;;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(uiop:define-package :garten/list
    (:use :cl :garten/grower
          :iterate)
  (:import-from :serapeum
                #:queue #:enq #:clear-queue #:qlist #:qconc)
  (:export #:list-grower
           #:plist-grower
           #:prepend-list-grower))

(in-package :garten/list)

;;; grow list directly

;;; List growers
(defclass list-grower (grower)
  ((grower-state :initform (queue)))
  (:documentation "A grower for a list that allows appending elements in constant time."))

(defmethod feed ((grower list-grower) item)
  (enq item (grower-state grower)))

(defmethod feed-iterable ((grower list-grower) (items list))
  "More efficient way to append a list to a list-grower.

Note that the list passed in may be destructively modified. If you want a copy
of the list to be appended, you can use a liter iterator over the list instead of
the list itself."
  (qconc (grower-state grower) items))

(defmethod reset-grower ((grower list-grower))
  (clear-queue (grower-state grower)))

(defmethod fruit ((grower list-grower))
  (qlist (grower-state grower)))

;;; Prepending list grower

(defclass prepend-list-grower (grower) ()
  (:documentation "A List grower that prepends values rather than appending them."))

(defmethod feed ((grower prepend-list-grower) item)
  (push item (grower-state grower)))

;;; Plist

(defclass plist-grower (grower)
  ((default-value :initarg :default :reader plist-grower-default))
  (:documentation "A grower that creates a plist. Items added should be cons cells
of the key and value. If an item isn't a cons cell it is treated as a key, with a value
of DEFAULT-VALUE."))

(defun %plist-feed (grower key value)
  (setf (getf (grower-state grower) key) value))

(defmethod feed ((grower plist-grower) (item cons))
  (%plist-feed grower (car item) (cdr item)))

(defmethod feed ((grower plist-grower) item)
  (%plist-feed grower item (plist-grower-default grower)))

;;; actually define creating them

(defmethod make-grower ((type (eql 'list)) &key prepend)
  "Create a grower that creates a list.
By default items are added to the end of the list, and the list is built using a LIST-GROWER.

However, if PREPEND is true, then items are pushed onto the front of the list."
  (if prepend
      (make-instance 'prepend-list-grower :init-state nil)
      (make-instance 'list-grower)))

(defmethod make-grower ((type (eql :plist)) &key default)
  "Create a grower that create a plist. Items should be added as a cons cell
with the key and value as the car and cdr respectively.

If an item is passed to FEED that isn't a cons, then the output will use the item as a key
and the default passed as DEFAULT as the value."
  (make-instance 'plist-grower :default default))

(defmethod make-grower ((type (eql :alist)) &key)
  "Create a grower for an alist. Items should be added as cons cells.
This is basically just a shortcut for a list grower with prepend set to true."
  (make-grower 'list :prepend t))

;;; Define grow method for list, although using a list builder would be better

(defmethod feed ((grower list) item)
  "This is a shorthand for (nconc grower (list item))
It is defined for consistancy, but using a LIST-GROWER is more efficient for building lists."
  (nconc grower (list item)))
