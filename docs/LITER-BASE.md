# liter/base

## Class: _ITERATION-ENDED_ `(SIMPLE-CONDITION)`

Condition signaled when an iterator has reached the end.


## Class: _UNHANDLED-ITERATION-END_ `(ERRORITERATION-ENDED)`

Error signaled if an iterator is ended and the ITERATION-ENDED
condition isn't handled in some way.


## Function: _END-ITERATION_ ``
Convenience function to return values for an iterator that has reached the end.


## Generic Function: _GET-ITERATOR_ `ITERABLE`
Get an iterator for some iterable object. Implementations are provided
for builtin iterable types.


## Method: _GET-ITERATOR_ `(S STREAM)`



## Method: _GET-ITERATOR_ `(S STREAM)`



## Method: _GET-ITERATOR_ `(S STREAM)`



## Method: _GET-ITERATOR_ `(H HASH-TABLE)`
Iterate over elements of a hash-table. Returns a cons of the key and value.

Since a clousre over a the form from a HASH-TABLE-ITERATOR is undefiend, at the time
of creation a list of keys is created and the iterator closes over that.

If you know of a better way of doing this, please let me know.


## Method: _GET-ITERATOR_ `(H HASH-TABLE)`
Iterate over elements of a hash-table. Returns a cons of the key and value.

Since a clousre over a the form from a HASH-TABLE-ITERATOR is undefiend, at the time
of creation a list of keys is created and the iterator closes over that.

If you know of a better way of doing this, please let me know.


## Method: _GET-ITERATOR_ `(H HASH-TABLE)`
Iterate over elements of a hash-table. Returns a cons of the key and value.

Since a clousre over a the form from a HASH-TABLE-ITERATOR is undefiend, at the time
of creation a list of keys is created and the iterator closes over that.

If you know of a better way of doing this, please let me know.


## Method: _GET-ITERATOR_ `(A ARRAY)`
Iterate over elements of array in row-major order.


## Method: _GET-ITERATOR_ `(A ARRAY)`
Iterate over elements of array in row-major order.


## Method: _GET-ITERATOR_ `(A ARRAY)`
Iterate over elements of array in row-major order.


## Method: _GET-ITERATOR_ `(S VECTOR)`



## Method: _GET-ITERATOR_ `(S VECTOR)`



## Method: _GET-ITERATOR_ `(S VECTOR)`



## Method: _GET-ITERATOR_ `(L LIST)`



## Method: _GET-ITERATOR_ `(L LIST)`



## Method: _GET-ITERATOR_ `(L LIST)`



## Method: _GET-ITERATOR_ `(O (EQL NIL))`
The nil iterator is always empty


## Method: _GET-ITERATOR_ `(O (EQL NIL))`
The nil iterator is always empty


## Method: _GET-ITERATOR_ `(O (EQL NIL))`
The nil iterator is always empty


## Method: _GET-ITERATOR_ `(F FUNCTION)`
The iterator for a function is just the function


## Method: _GET-ITERATOR_ `(F FUNCTION)`
The iterator for a function is just the function


## Method: _GET-ITERATOR_ `(F FUNCTION)`
The iterator for a function is just the function


## Function: _MAKE-HASH-KEY-ITERATOR_ `H`



## Function: _MAKE-HASH-VALUE-ITERATOR_ `H`



## Function: _MAKE-CHARACTER-STREAM-ITERATOR_ `STREAM`



## Function: _MAKE-BYTE-STREAM-ITERATOR_ `STREAM`



## Function: _MAKE-LINE-ITERATOR_ `STREAM`



## Function: _INEXT_ `ITERATOR&RESTARGS`
Return the next value of an iterator and whether or not an actual value was
retrieved as values.

Any additional arguments are passed through to the iterator function.


## Macro: _DO-ITERATOR_ `(VAR ITERATOR &OPTIONAL RETURN)&BODYBODY`
A DO macro in the style of dolist that executes body for each
item in ITERATOR.


## Macro: _DO-ITERABLE_ `(VAR ITERABLE &OPTIONAL RETURN)&BODYBODY`
Loop over all items in the iterable ITERABLE, in a manner similar to dolist.


## Macro: _CLAUSE-FOR-IN-ITERATOR-3_ `&KEY((FOR VAR) NIL)((IN-ITERATOR IT) NIL)GENERATE`



## Macro: _CLAUSE-FOR-IN-ITERABLE-4_ `&KEY((FOR VAR) NIL)((IN-ITERABLE IT) NIL)GENERATE`



## Function: _ITERATOR-LIST_ `ITERATOR`
Create a list from an iterator. Note that this will only work
if the iterator terminates.

