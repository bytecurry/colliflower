;;;; base.lisp

(uiop:define-package :liter/base
  (:use :cl :iterate)
  (:import-from :serapeum
                #:define-do-macro
                #:finc)
  (:export #:get-iterator
           #:make-hash-key-iterator
           #:make-hash-value-iterator
           #:make-character-stream-iterator
           #:make-byte-stream-iterator
           #:make-line-iterator
           #:iteration-ended #:end-iteration
           #:do-iterator #:do-iterable
           #:inext
           #:in-iterator #:in-iterable
           #:iterator-list))

(in-package :liter/base)

(define-condition iteration-ended (simple-condition) ()
  (:documentation "Condition signaled when an iterator has reached the end."))
(define-condition unhandled-iteration-end (error iteration-ended) ()
  (:documentation "Error signaled if an iterator is ended and the ITERATION-ENDED
condition isn't handled in some way.")
  (:report "An ITERATION-ENDED signal wasn't handled."))

(defun end-iteration ()
  "Convenience function to return values for an iterator that has reached the end."
  (restart-case (progn
                  (signal (make-condition 'iteration-ended))
                  (error (make-condition 'unhandled-iteration-end)))
    (use-value (value)
      value)))

(defgeneric get-iterator (iterable)
  (:documentation "Get an iterator for some iterable object. Implementations are provided
for builtin iterable types.")

  (:method ((f function))
    "The iterator for a function is just the function"
    f)

  (:method ((o (eql nil)))
    "The nil iterator is always empty"
    #'end-iteration)

  (:method ((l list))
    "Iterator for the items of a list, or pairs of an alist."
    (lambda ()
      (if l
          (pop l)
          (end-iteration))))

  (:method ((s vector))
    "Iterate over the items of a vector respecting the fill-counter if extant."
    (let ((i 0))
      (lambda ()
        (if (< i (length s))
            (elt s (finc i))
            (end-iteration)))))

  (:method ((a array))
    "Iterate over elements of array in row-major order."
    (let ((i 0)
          (len (array-total-size a)))
      (lambda ()
        (if (< i len)
            (row-major-aref a (finc i))
            (end-iteration)))))

  (:method ((h hash-table))
    "Iterate over elements of a hash-table. Returns a cons of the key and value.

Since a clousre over a the form from a HASH-TABLE-ITERATOR is undefiend, at the time
of creation a list of keys is created and the iterator closes over that.

If you know of a better way of doing this, please let me know."
    (let ((key-iterator (make-hash-key-iterator h)))
      (lambda ()
        (let ((key (funcall key-iterator)))
          (cons key (gethash key h))))))

  (:method ((s stream))
    "Iterate over the elements of a stream. I.e. the characters of a character
stream or the bytes of a byte stream."
    (etypecase (stream-element-type s)
      (character (make-character-stream-iterator s))
      (integer   (make-byte-stream-iterator s)))))

(defun make-hash-key-iterator (h)
  "Get an iterator over the keys of H."
  (declare (hash-table h))
  (let ((keys (loop for key being the hash-keys of h
                  collect key)))
    (get-iterator keys)))

(defun make-hash-value-iterator (h)
  "Get an iterator over the values of H."
  (declare (hash-table h))
  (let ((vals (loop for v being the hash-values of h
                  collect v)))
    (get-iterator vals)))

(defun make-character-stream-iterator (stream)
  "Get an iterator for a character input stream"
  (declare (stream stream))
  (lambda ()
    (handler-case (read-char stream)
      (end-of-file ()
        (end-iteration)))))

(defun make-byte-stream-iterator (stream)
  "Get an iterator for a binary input stream."
  (declare (stream stream))
  (lambda ()
    (handler-case (read-byte stream)
      (end-of-file ()
        (end-iteration)))))

(defun make-line-iterator (stream)
  "Get an iterator that iterates over lines in a character stream."
  (declare (stream stream))
  (lambda ()
    (handler-case (read-line stream)
      (end-of-file ()
        (end-iteration)))))

(defun inext (iterator &rest args)
  "Return the next value of an iterator and whether or not an actual value was
retrieved as values.

Any additional arguments are passed through to the iterator function."
  (handler-case (values (apply iterator args) t)
    (iteration-ended () (values nil nil))))


(define-do-macro do-iterator ((var iterator &optional return) &body body)
  "A DO macro in the style of dolist that executes body for each
item in ITERATOR."
  (let* ((it (gensym))
         (block-name (gensym)))
    `(let ((,it ,iterator))
       (loop named ,block-name do
            (handler-case (let ((,var (funcall ,it)))
                            ,@body)
              (iteration-ended () (return-from ,block-name)))))))

(defmacro do-iterable ((var iterable &optional return) &body body)
  "Loop over all items in the iterable ITERABLE, in a manner similar to dolist."
  `(do-iterator (,var (get-iterator ,iterable) ,return)
     ,@body))

(defmacro-driver (FOR var IN-ITERATOR it)
  "Iterate over all items in an iterator."
  (let ((iterator (gensym))
        (kwd (if generate 'generate 'for)))
    `(progn
       (with ,iterator = ,it)
       (,kwd ,var next (handler-case (funcall ,iterator)
                         (iteration-ended () (terminate)))))))

(defmacro-driver (FOR var IN-ITERABLE it)
  "Iterate over all items in an iterable."
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (,kwd ,var in-iterator (get-iterator ,it)))))

(defun iterator-list (iterator)
  "Create a list from an iterator. Note that this will only work
if the iterator terminates."
  (iter (for v in-iterator iterator)
        (collect v)))
