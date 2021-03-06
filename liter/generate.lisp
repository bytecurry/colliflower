;;; generate.lisp

(uiop:define-package :liter/generate
    (:use :cl :liter/base)
  (:import-from :serapeum
                #:finc)
  (:export #:icounter
           #:irepeat #:singleton-iterator
           #:make-iterator
           #:make-state-iterator #:make-state-iterator*
           #:icycle #:icycle*))

(in-package :liter/generate)

(defun icounter (&key (from 0) (by 1) (to 0 stop-p))
  "Create an iterator that just counts up from FROM by amount BY forever.
The returned iterator takes an optional argument which resets the counter if true.

If a TO parameter is provided, then the counter will stop when the value returned would equal
the TO parameter. Note that if using a BY parameter it is possible to step over the TO
end-point, but a >= comparison is undesirable because it wouldn't work for a negative step."
  (declare (number from by to))
  (let ((i from))
    (lambda (&optional reset)
      (when reset
        (setf i from))
      (when (and stop-p (= i to))
        (end-iteration))
      (finc i by))))

(defun irepeat (v &optional (n -1))
  "Create an iterator that returns the value V N times.
If N is negative (the default) iterate forever.

Note that CONSTANTLY is equivalent to IREPEAT with a negative N,
and in fact there is a compiler macro that compiles IREPEAT as CONSTANTLY
in that case."
  (declare (integer n))
  (let ((i 0))
    (lambda ()
      (when (>= i n)
        (end-iteration))
      (incf i)
      v)))

(define-compiler-macro irepeat (&whole form v &optional (n -1))
  (if (and (numberp n) (minusp n))
      `(constantly ,v)
      form))

(defun singleton-iterator (element)
  "Create an iterator that returns element on the first iteration, and
ends on the second.

Equivalent to `(irepeat element 1)`."
  (irepeat element 1))

(defun icycle* (&rest args)
  "Create an iterator that cycles through the arguments passed to it."
  (setf (cdr (last args)) args)         ; Make the args list cyclical
  (get-iterator args))

(defun icycle (iterable)
  "Create an iterator that iterates through ITERABLE, and then cycles through
the values again.
This stores the results of iterating over ITERABLE."
  (let ((firstpass t)
        (head nil)
        (last nil)
        (iterator (get-iterator iterable)))
    (lambda ()
      (if firstpass
          (handler-case
              (let* ((res (funcall iterator))
                     (cell (cons res nil)))
                (if (null head)
                    (setf last
                          (setf head cell))
                    (setf last (setf (cdr last) cell)))
                res)
            (iteration-ended ()
              (setf firstpass nil)
              (setf (cdr last) head)
              (pop head)))
          (pop head)))))

(defun ilist (&rest elements)
  "Iterate over the arguments passed in. Then terminate."
  (get-iterator elements))

(defmacro make-iterator (&body body)
  "Create an iterator that executes BODY each time.

Basically a simple wrapper for LAMBDA.
Iteration can be ended by calling END-ITERATION."
  `(lambda ()
     ,@body))

(defun make-state-iterator (state-machine &optional (initial nil))
  "Create an iterator that starts with an initial state INITIAL and on each call to the iterator
STATE-MACHINE is called with the previous state as the first argument followed by any arguments
passed to the iterator function. The return value is then stored and used for the next iteration.

Note that STATE-MACHINE will be called on the first iteration with INITIAL as the state.

This can be used to create simple state machines."
  (let ((state initial))
    (lambda (&rest args)
      (setf state (apply state-machine state args)))))

(defmacro make-state-iterator* (initial-state (&rest lambda-list) &body body)
  "Macro form of MAKE-STATE-ITERATOR. Note that in this form INITIAL-STATE is required."
  `(make-state-iterator (lambda ,lambda-list
                          ,@body)
                        ,initial-state))
