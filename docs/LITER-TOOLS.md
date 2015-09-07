# liter/tools

## Function: _ITRANSFORM_ `ITERABLEMAP-FUN`
Return an iterator that iterates over transformed values of an iterable.

ITERABLE is on object for which GET-ITERATOR is defined.
MAP-FUN is a function which is called with each value returned by the iterator
and returns the value that the new iterator should return.


## Function: _IFILTER_ `ITERABLEPREDICATE`
Return a new iterator that iterates over the values of ITERABLE for which
PREDICATE is true.


## Function: _IFOLD_ `ITERABLEOP&OPTIONALINITIAL`
Fold over an iterable.
ITERABLE is the iterable to fold over.
OP is a binary operation that takes the accumulated result, and an iterated item
and returns a new accumulated result.
INITIAL is the initial accumulated state.


## Function: _IACCUMULATE_ `ITERABLE&OPTIONAL(OP '+)`
Return an iterator that accumulates the results of appling OP to the previous
result and the current item.

IACCUMULATE is like IFOLD that keeps track of intermediary results.


## Function: _ICHAIN_ `FIRST&RESTREST`
Return a new iterator that iterates through each of the
iterables passed to it in series.


## Function: _IZIP_ `&RESTITERABLES`
Zip iterables together.

This returns an iterator that returns a list of the results of getting the next value from each iterable.
The iterator ends when the shortest of the iterables ends.


## Function: _IZIP-LONGEST_ `MISSING-VALUE&RESTITERABLES`
Like IZIP but stops on the longest iterable.
When a shorter iterable ends, it will continually return MISSING-VALUE until
the whole iterator ends.


## Function: _IZIP-WITH-INDEX_ `ITERABLE`
Zip an iterable with an index.

Each element of the new iterator is a list of the index (starting with 0) and
the next element of ITERABLE.


## Function: _ITEE_ `ITERABLE&OPTIONAL(N 2)`
Split a single iterable into N iterators.
The iterators are returned as values.
Note that ITEE depends on the iterator of iterable signaling an END-ITERATION
if the iterator has ended, even if an END-ITERATION has already been signaled.


## Function: _ITAKE-WHILE_ `ITERABLEPRED`



## Function: _ITAKE_ `ITERABLEN`



## Function: _IDROP_ `ITERABLEN`



## Function: _IDROP-WHILE_ `ITERABLEPRED`


