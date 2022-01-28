#lang racket
(require berkeley)

; As a large system with generic operations evolves, new types of data objects or 
; new operations may be needed. For each of the three strategies—generic operations 
; with explicit dispatch, data-directed style, and messagepassing-style. Describe the 
; changes that must be made to a system in order to add new types or new operations. Which 
; organization would be most appropriate for a system in which new types must often be added? 
; Which would be most appropriate for a system in which new operations must often be added?

; Explicit dispatch
;
; If you add a new operation, every data type must define the logic to handle said operation
; and a new generic procedure must be created to apply the appropiate concrete procedure 
; in terms of the data type
;
; If you add a new data type, you must let every generic procedure know, so it will
; be able to redirect correctly when the operations are to be applied onto this new type of data.
;
; Data directed 
; 
; If you add a new operation, every data type must define the logic to handle said operation as 
; an internal procedure. Then it must be added to the interface by storing in in the operation-procedure
; table with put.
;
; If you add a new data type, you do not have to do anything if you do not want it to integrate with
; other data types.
;
; Message passing
; 
; If you add a new operation, this should be defined on the data type, but that is a given. 
;
; If you add a new data type, you do not have to do anything if you do not want it to integrate with
; other data types.


; For adding new data types, data directed programming seems the more flexible, as it handles data types
; on a more global level, so that all data type have access to all other data types, thank to the tables.

; For adding new operations, message-passing seems to be the best choice, as it only requires to add a new
; clause to the dispatch procedure, and it is completely aisolated from the rest of the data types.
