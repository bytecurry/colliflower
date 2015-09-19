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
  ((stream :initarg :file-stream  :accessor file-iterator-stream :type file-stream
           :documentation "The stream that the file-iterator is backed by.")
   (reader :initarg :reader
           :accessor file-iterator-reader
           :type (function (stream) t)
           :documentation "The function to use to read from the stream. The results of calling
this function will be the elements of the iterator."))
  #+closer-mop
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "An iterator object that represents the iteration state of
iterating over a file stream."))

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
                                      reader
                                      (element-type 'character)
                                      (if-does-not-exist :error)
                                      (external-format :default))
  "Create a FILE-ITERATOR using the same arguments as OPEN. In fact this is basically like OPEN, but
returns a FILE-ITERATOR instead of a stream.

If a reader function is supplied, then that function is used to read elements from the file. So for example,
#'read-line can be used for the iterator to return lines at a time. The reader function takes an input stream
as an argument.

By default READ-CHAR is used for a character stream and READ-BYTE is used for binary streams."
  (declare (type (or null (function (stream))) reader))
  (unless reader
    (setf reader (%get-read-function element-type)))
  (make-instance 'file-iterator
                 :file-stream (open filename
                                    :element-type element-type
                                    :if-does-not-exist if-does-not-exist
                                    :external-format external-format)
                 :reader reader))

(defmacro with-file-iterator ((var filename &key
                                   reader
                                   (element-type (quote 'character))
                                   (if-does-not-exist :error)
                                   (external-format :default)) &body body)
  "Macro similar to WITH-OPEN-FILE, but VAR is bound to a file iterator instead of a STREAM.
The result is guaranteed to be an actual iterator, even if closer-mop isn't enabled."
  (let ((reader-form (if reader
                         reader
                         `(%get-read-function ,element-type)))
        (stream (gensym)))
    `(with-open-file ,(list stream filename
                            :element-type element-type
                            :if-does-not-exist if-does-not-exist
                            :external-format external-format)
       (let ((,var (get-iterator
                    (make-instance 'file-iterator
                                   :file-stream ,stream
                                   :reader ,reader-form))))
         ,@body))))

(defun %get-read-function (element-type)
  (if (subtypep element-type 'character)
      #'read-char
      #'read-byte))
