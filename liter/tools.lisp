;;;; tools.lisp

(uiop:define-package :liter/tools
    (:use :cl :liter/base
          :liter/generate
          :iterate)
  (:export #:itransform
           #:ifilter
           #:ifold #:iaccumulate
           #:ichain
           #:izip #:izip-longest
           #:itee))

(in-package :liter/tools)

(defun itransform (iterable map-fun)
  "Return an iterator that iterates over transformed values of an iterable.

ITERABLE is on object for which GET-ITERATOR is defined.
MAP-FUN is a function which is called with each value returned by the iterator
and returns the value that the new iterator should return."
  (let ((iterator (get-iterator iterable)))
    (lambda (&rest args)
      (funcall map-fun (apply iterator args)))))

(defun ifilter (iterable predicate)
  "Return a new iterator that iterates over the values of ITERABLE for which
PREDICATE is true."
  (let ((iterator (get-iterator iterable)))
    (lambda (&rest args)
      (loop
         (let ((item (apply iterator args)))
           (when (funcall predicate item)
             item))))))

(defun ifold (iterable op &optional initial)
  "Fold over an iterable.
ITERABLE is the iterable to fold over.
OP is a binary operation that takes the accumulated result, and an iterated item
and returns a new accumulated result.
INITIAL is the initial accumlated state."
  (iter (for item in-iterable iterable)
        (reducing item by op initial-value initial)))

(defun iaccumulate (iterable &optional (op '+))
  "Return an iterator that accumulates the results of appling OP to the previous
result and the current item.

IACCUMULATE is like IFOLD that keeps track of intermediary results."
  (let ((it (get-iterator iterable))
        prev prevp)
    (lambda (&rest args)
      (if prevp
          (setf prev (funcall op prev (apply it args)))
          (setf prevp t
                prev (apply it args))))))

(defun ichain (first &rest rest)
  "Return a new iterator that iterates through each of the
iterables passed to it in series."
  (let ((it (get-iterator first)))
    (lambda (&rest args)
      (loop
         (handler-case (return
                         (apply it args))
           (iteration-ended (c)
             (if rest
                 (setf it (get-iterator (pop rest)))
                 (signal c))))))))

(defun izip (&rest iterables)
  "Zip iterables together.

This returns an iterator that returns a list of the results of getting the next value from each iterable.
The iterator ends when the shortest of the iterables ends."
  (let ((iterators (mapcar #'get-iterator iterables)))
    (lambda ()
      (mapcar #'funcall iterators))))

(defun izip-longest (missing-value &rest iterables)
  "Like IZIP but stops on the longest iterable.
When a shorter iterable ends, it will continually return MISSING-VALUE until
the whole iterator ends."
  (let ((count 0)
        (max (list-length iterables)))
    (flet ((make-zip-iterator (iterable)
             (let ((it (get-iterator iterable))
                   done)
               (make-iterator
                (if done
                    missing-value
                    (handler-case (funcall it)
                      (iteration-ended ()
                        (format *terminal-io* "Ended iterator: ~a" count)
                        (incf count)
                        (when (>= count max)
                          (end-iteration))
                        (setf done t)
                        missing-value)))))))
      (let ((iterators (mapcar #'make-zip-iterator iterables)))
        (lambda ()
          (mapcar #'funcall iterators))))))

(defun itee (iterable &optional (n 2))
  "Split a single iterable into N iterators.
The iterators are returned as values.
Note that ITEE depends on the iterator of iterable signaling an END-ITERATION
if the iterator has ended, even if an END-ITERATION has already been signaled."
  (let* ((iterator (get-iterator iterable))
         (current-cell (cons nil nil)))
    (labels ((ensure-next (cell)
               (when (endp (cdr cell))
                 (setf (cdr cell) (cons (funcall iterator) nil))))
             (tee-iterator ()
               (let ((current-cell current-cell))
                 (lambda ()
                   (ensure-next current-cell)
                   (setf current-cell (cdr current-cell))
                   (car current-cell)))))
      (values-list (loop for i from 1 to n
                        collect (tee-iterator))))))
