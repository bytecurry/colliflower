# liter/file

## Class: _FILE-ITERATOR_ `(ITER-OBJECT)`



### Slot: _STREAM_


### Slot: _BY-LINE_


### Slot: _READ-FUN_



## Function: _%UPDATE-READ-FUN_ `OBJ`



## Method: _FILE-ITERATOR-STREAM_ `V(OBJ FILE-ITERATOR)`



## Function: _MAKE-FILE-ITERATOR_ `FILENAME&KEYBY-LINE(ELEMENT-TYPE 'CHARACTER)(IF-DOES-NOT-EXIST
                                                                                ERROR)(EXTERNAL-FORMAT
                                                                                       DEFAULT)`
Create a FILE-ITERATOR using the same arguments as OPEN. In fact this is basically like OPEN, but
returns a FILE-ITERATOR instead of a stream.

If BY-LINE is true, and the ELEMENT-TYPE is a subtype of CHARACTER, then the iterator will return
whole lines at a time instead of individual characters.


## Macro: _WITH-FILE-ITERATOR_ `(VAR FILENAME &KEY BY-LINE
                                 (ELEMENT-TYPE ''CHARACTER)
                                 (IF-DOES-NOT-EXIST ERROR)
                                 (EXTERNAL-FORMAT DEFAULT))&BODYBODY`
Macro similar to WITH-OPEN-FILE, but VAR is bound to a file iterator instead of a STREAM.

