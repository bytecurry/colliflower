(defpackage colliflower-fset/garten
  (:use :cl :garten)
  (:export #:fset-grower
           #:fset-map-grower))

(in-package :colliflower-fset/garten)

(defclass fset-grower (grower) ())

(defclass fset-map-grower (grower) ())

(defmethod feed ((grower fset-grower) item)
  (fset:adjoinf (grower-state grower) item))

(defmethod feed ((grower fset-map-grower) (pair cons))
  (fset:adjoinf (grower-state grower) (car pair) (cdr pair)))

(defmethod make-grower ((type (eql 'fset:set)) &key)
  (make-instance 'fset-grower :init-state (fset:empty-set)))

(defmethod make-grower ((type (eql 'fset:map)) &key)
  (make-instance 'fset-map-grower :init-state (fset:empty-map)))

(defmethod make-grower ((type (eql 'fset:bag)) &key)
  (make-instance 'fset-grower :init-state (fset:empty-bag)))

(defmethod make-grower ((type (eql 'fset:seq)) &key)
  (make-instance 'fset-grower :init-state (fset:empty-seq)))

(defmethod make-grower ((type (eql 'fset:tuple)) &key)
  (make-instance 'fset-map-grower :init-state (fset:empty-tuple)))
