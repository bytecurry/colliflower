;;; file.lisp
;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(uiop:define-package :liter/file
    (:use :cl :liter/base :liter/iter-object)
  (:export #:file-iterator
           #:file-iterator-stream
           #:make-file-iterator
           #:with-file-iterator))

(in-package :liter/file)

(defclass file-iterator (iter-object)
  ((stream :initarg :file-stream  :accessor file-iterator-stream :type file-stream)
   (by-line :initarg :by-line :initform nil)
   (read-fun :reader file-iterator-reader :type function))
  #+closer-mop
  (:metaclass closer-mop:funcallable-standard-class))

(defun %update-read-fun (obj)
  (declare (file-iterator obj))
  (setf (slot-value obj 'read-fun)
        (ecase (stream-element-type (file-iterator-stream obj))
          (character (if (slot-value obj 'by-line)
                         #'read-line
                         #'read-char))
          (integer   #'read-byte))))

(defmethod initialize-instance :after ((obj file-iterator) &key &allow-other-keys)
  (%update-read-fun obj))

(defmethod (setf file-iterator-stream) :after (v (obj file-iterator))
  (%update-read-fun obj))

(defmethod iter-object-next ((obj file-iterator) &rest args)
  (declare (ignorable args))
  (let ((stream (file-iterator-stream obj)))
    (handler-case
        (funcall (file-iterator-reader obj) stream)
      (end-of-file ()
        (close stream)
        (end-iteration)))))

(defmethod iter-object-end-p ((obj file-iterator))
  (not (open-stream-p (file-iterator-stream obj))))

(defun make-file-iterator (filename &key
                                      by-line
                                      (element-type 'character)
                                      (if-does-not-exist :error)
                                      (external-format :default))
  "Create a FILE-ITERATOR using the same arguments as OPEN. In fact this is basically like OPEN, but
returns a FILE-ITERATOR instead of a stream.

If BY-LINE is true, and the ELEMENT-TYPE is a subtype of CHARACTER, then the iterator will return
whole lines at a time instead of individual characters."
  (make-instance 'file-iterator
                 :file-stream (open filename
                                    :element-type element-type
                                    :if-does-not-exist if-does-not-exist
                                    :external-format external-format)
                 :by-line by-line))

(defmacro with-file-iterator ((var filename &key
                                   by-line
                                   (element-type (quote 'character))
                                   (if-does-not-exist :error)
                                   (external-format :default)) &body body)
  "Macro similar to WITH-OPEN-FILE, but VAR is bound to a file iterator instead of a STREAM."
  (let ((stream (gensym)))
    `(with-open-file ,(list stream filename
                            :element-type element-type
                            :if-does-not-exist if-does-not-exist
                            :external-format external-format)
       (let ((,var (make-instance 'file-iterator
                                  :file-stream ,stream
                                  :by-line ,by-line)))
         ,@body))))
