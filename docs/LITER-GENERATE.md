# liter/generate

## Function: _ICOUNTER_ `&KEY(FROM 0)(BY 1)(TO 0 STOP-P)`



## Function: _IREPEAT_ `V&OPTIONAL(N -1)`



## Function: _SINGLETON-ITERATOR_ `ELEMENT`
Create an iterator that returns element on the first iteration, and
ends on the second.

Equivalent to `(irepeat element 1)`.


## Macro: _MAKE-ITERATOR_ `&BODYBODY`
Create an iterator that executes BODY each time.

Basically a simple wrapper for LAMBDA.
Iteration can be ended by calling END-ITERATION.


## Function: _MAKE-STATE-ITERATOR_ `STATE-MACHINE&OPTIONAL(INITIAL NIL)`
Create an iterator that starts with an initial state INITIAL and on each call to the iterator
STATE-MACHINE is called with the previous state as the first argument followed by any arguments
passed to the iterator function. The return value is then stored and used for the next iteration.

Note that STATE-MACHINE will be called on the first iteration with INITIAL as the state.

This can be used to create simple state machines.


## Macro: _MAKE-STATE-ITERATOR*_ `INITIAL-STATE(&REST LAMBDA-LIST)&BODYBODY`
Macro form of MAKE-STATE-ITERATOR. Note that in this form INITIAL-STATE is required.


## Function: _ICYCLE*_ `&RESTARGS`
Create an iterator that cycles through the arguments passed to it.


## Function: _ICYCLE_ `ITERABLE`
Create an iterator that iterates through ITERABLE, and then cycles through
the values again.
This stores the results of iterating over ITERABLE.
