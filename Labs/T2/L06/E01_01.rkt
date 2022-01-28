#lang racket
(require berkeley)

; Insatiable Enterprises, Inc., is a highly decentralized conglomerate company consisting of a large number of 
; independent divisions located all over the world. The company’s computer facilities have just been interconnected
; by means of a clever network-interfacing scheme that makes the entire network appear to any user to be 
; a single computer. Insatiable’s president, in her first attempt to exploit the ability of the network to extract 
; administrative information from division files, is dismayed to discover that, although all the division files have 
; been implemented as data structures in Scheme, the particular data structure used varies from division to division. 
; A meeting of division managers is hastily called to search for a strategy to integrate the files
; that will satisfy headquarters’ needs while preserving the existing autonomy of the divisions.
; Show how such a strategy can be implemented with data directed programming. 
;
; As an example, suppose that each division’s personnel records consist of a single file, which
; contains a set of records keyed on employees’ names. 
;
; The structure of the set varies from division to division. 
;
; Furthermore, each employee’s record is itself a set (structured differently from division to division) that contains 
; information keyed under identifiers such as address and salary.
; 
; In particular:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

; a. Implement for headquarters a get-record procedure that retrieves a specified employee’s record from a
; specified personnel file. The procedure should be applicable to any division’s file. Explain how the individual 
; divisions’ files should be structured. In particular, what type information must be supplied?

; Message passing could be used, where the operation name is passed to the data object corresponding to the division
; For example:
;
; (define (division name employees)
;   (define (dispatch args)
;     ((equal? (car args) 'get-record) (get-record (cdr args)))
;     ...
;   )
;   (dispatch)
; )
; 
; Where the employees is a data structure itself.
; Note that we pass args as an argument, because we expect the data object receives the operation name
; as the first argument and other arguments after it. In this case the rest of the arguments would be the
; employee id.
; 
; There would also be necessary to define a get-record procedure that retrieves a given employee's record.
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

; b. Implement for headquarters a get-salary procedure that returns the salary information from a given employee’s
; record from any division’s personnel file. How should the record be structured in order to make this operation work?

; Message passing could be used, where the operation name is passed to the data object corresponding to the record
; For example:
;
; (define (record id)
;   (define (dispatch args)
;     ((equal? (car args) 'get-id) id)
;     ((equal? (car args) 'get-salary) (get-salary (cdr args)))
;     ...
;   )
;   (dispatch)
; )

; The logic is the same as in the previous section.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

; c. Implement for headquarters a find-employee-record procedure. This should search all the divisions’ files
; for the record of a given employee and return the record. Assume that this procedure takes as arguments an
; employee’s name and a list of all the divisions’ files.

; Because we have decided onto message passing, the find-employee-record procedure should be fairly straight 
; forward:
;
; (define (find-employee-record divisions record)
;   (cond
;     ((null? divisions) (error "The record does not exist"))
;     (else
;       (let
;         ((
;           record-found ((car (divisions)) 'get-record (record 'get-id))
;         ))
;         (if (record-found)
;           record-found
;           (find-employee-record (cdr divisions) record)
;         )
;       )
;     )
;   )
; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

; d. When Insatiable takes over a new company, what changes must be made in order to incorporate the new 
; personnel information into the central system?
