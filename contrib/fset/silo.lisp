(defpackage colliflower-fset/silo
  (:use :cl :silo))

(in-package :colliflower-fset/silo)


(defmethod sget ((coll fset:collection) key &key)
  (fset:lookup coll key))

(defmethod supdate ((coll fset:collection) key value &key)
  (fset:with coll key value))

(defmethod supdate ((s fset:set) key value &key)
  "For a set, we treat the value as a boolean, true inserts and
nil deletes."
  (if value
      (fset:with s key)
      (fset:less s key)))

(defmethod sremove ((coll fset:collection) key &key)
  (fset:less coll key))

(defmethod contains-p ((coll fset:collection) element)
  (fset:contains? coll element))
