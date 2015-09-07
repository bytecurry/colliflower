# liter/iter-object

## Class: _ITER-OBJECT_ `()`

A class to represent an iterator object.

If closer-mop is enabled, it is a funcallable object that calls ITER-OBJECT-NEXT when called.
Otherwise GET-ITERATOR returns a function that calls ITER-OBJECT-NEXT.

Subclasses should use a metaclass of FUNCALLABLE-STANDARD-CLASS if mop is available.


## Generic Function: _ITER-OBJECT-NEXT_ `ITER-OBJECT&RESTARGS`
Get the next object in the iter-object iterator.


## Generic Function: _ITER-OBJECT-PREV_ `ITER-OBJECT&RESTARGS`
Get the previous object in the iter-object iterator.

This is optional for iter-object implementations.


## Generic Function: _ITER-OBJECT-END-P_ `ITER-OJBECT`
Predicate to test if the iterator has ended.

