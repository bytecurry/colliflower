;;; interface.lisp
;;;
;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(uiop:define-package :liter/interface
  (:nicknames :liter :iterator)
  (:documentation "Liter is a collection of tools to work with basic iterators.

An iterator in Liter is just a function (usually a closure) which returns
the next value in the iterable, and signals a ITERATION-ENDED condition
if there are no more values. It should follow the following rules:

* An iterator MAY be called with optional, keyword, or rest arguments, but MUST function
correctly if called with no arguments.
* If an iterator has completed, it SHOULD signal an ITERATION-ENDED condition, typically using
  END-ITERATION.
* Client code SHOULD handle any ITERATION-ENDED condition, or use a function or macro that does
(such as the iterate drivers, DO-ITERATOR, etc).
* Additional return values may be ignored by the caller of an iterator.
* By convention, key-value pairs SHOULD be represented as a cons pair. (as in alists)
* An iterator MAY be infinite, but not all liter functions can be safely used with such
iterators. Functions such as ITAKE can be used to convert an infinite iterator to a finite one.

An iterable is any object for which there is a method defined for GET-ITERATOR.
Most liter function accept an iterable, and will get an iterator using GET-ITERATOR.
There is a GET-ITERATOR method defined for iterators that is equivalent to the identity
function, so iterators can be passed to these functions.")
  (:USE-reexport :liter/base
                 :liter/generate
                 :liter/tools
                 :liter/iter-object
                 :liter/file))
