# colliflower
Colliflower is a set of generic interfaces for lisp collections.

Colliflower has four components:
 1. liter
 2. garten
 3. silo
 4. colliflower

Each of these components are described below.

## liter
Liter provides an abstraction and tools for iterators.

A liter _iterator_ is simply a function which returns the next value with
each invocation and signals and end of iteration with an `ITERATION-ENDED`
condition.

A liter _iterable_ is any object for which there is a method defined for the
`GET-ITERATOR` generic function. An _iterator_ (or more generally a function)
is an _iterable_ whose _iterator_ is itself. The `LITER` package provides `GET-ITERATOR`
methods for lists, vectors, hash-tables, streams, and arrays, as well as functions to
get iterators over those structures in different ways (such as treating lists as plists
or alists, or iterating over the lines of a character stream).

Liter also provides:
 * macros to iterate over an _iterator_ (or _iterable_), including a driver for
 the `ITERATE` library
 * functions to generate abstract and potentially infinite iterators (including iterators that count, repeat, and cycle)
 * Several functions that perform operations on iterators, such as mapping, filtering, folding,
 zipping, etc. similar to the itertools library in python.
 * An abstract `ITER-OBJECT` class that can be used for iterators with complex state.

## garten
Garten provides a generic abstraction for building new data structures. It
is a more generic form of the `COLLECTING` macro.

The core of garten is the concept of a "grower". A _grower_ is an object
that stores the state of a collection while it is being built. The _grower_
itself may or may not be the final result. For example: the grower for a list
may keep handles for both the head and tail of the list to efficiently append
elements, or the _grower_ for an immutable data structure may itself be mutable.

The basic interface for garten consists of three generic functions:
 1. `MAKE-GROWER` creates a new grower for some type. Note that the `TYPE` argument is
 the type of the final data structure, not the type of the _grower_.
 2. `FEED` adds an item to the grower, which will eventually be part of the final
 structure.
 3. `FRUIT` retrieves the final result of the grower. In general it is not safe to call
 `FRUIT`, `FEED`, or `RESET-GROWER` after `FRUIT` has been called for a grower, as doing so
 may mutate the result returned by previous calls to `FRUIT`.

Garten defines the above interface for lists, plists, vectors, hash-tables, strings, etc.
Garten also provides `WITH-GROWER` which is similar to the oft-implemented `COLLECTING`
macro, and a `GROWING` directive for the iterate library.

## silo
Silo provides a generic way to access and set values of data structures by a key.
The key may be a normal key, or an index.

## colliflower

Colliflower is brings the four above components together. The `colliflower` package exports
all of the public symbols from `liter`, `garten`, and `silo` and provides a few additional tools that combine functionality from the packages.
