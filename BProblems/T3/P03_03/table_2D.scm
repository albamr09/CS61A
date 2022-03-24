;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TWO-DIMENSIONAL TABLE TAD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A two dimensional table is a one dimensional table
;; where each key identifies a subtable

;; Example
; (table, .)
;         |
;        (., .)
;         |  |___(., /)  # because of cdr
;         |        |   
;         |       (st2, .) -> (e21, .) -> (e22, .)
;         |
;        (st1, .) -> (e11, .) -> (e12, .)

;;;;;;;;;;;;;;
;; CONSTRUCTOR
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; SELECTOR
;;;;;;;;;;;;;;

(define (lookup key-1 key-2 table)
  (let 
    ; Get the subtable in the table by the first key
    ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let 
        ; Obtain the record in the subtable by the second key
        ((record (assoc key-2 (cdr subtable))))
        ; If there is a record
        (if record
          ; Return record
          (cdr record)
          ; Else return false
          false
        )
      )
      ; If there is no subtable, return false
      false
    )
  )
)

;;;;;;;;;;;;;;
;; MUTATORS
;;;;;;;;;;;;;;

(define (insert! key-1 key-2 value table)
  (let 
    ; Search a subtable in the table by the first key
    ((subtable (assoc key-1 (cdr table))))
    ; If there exists a subtable
    (if subtable
      ; Search the record in the subtable by the second key
      (let ((record (assoc key-2 (cdr subtable))))
        ; If there is a record, udpate the value of the record
        (if record
          (set-cdr! record value)
          ; Else, insert new record in table, after dummy record 
          ; (see table_1D.scm for an example)
          (set-cdr! subtable
            (cons 
              (cons key-2 value)
              (cdr subtable)
            )
          )
        )
      )
      ; If there is not a subtable, make the new subtable be the first record of the
      ; table
      (set-cdr! 
        table
        ; Create the subtable, make it point to what was the first record of the table 
        (cons 
          ; Subtable
          (list 
            ; key-1: dummy entry of subtable
            key-1
            ; (key-2, value), first record of subtable
            (cons key-2 value)
          )
          ; New subtable points to what was the first record of table
          (cdr table)
        )
      )
    )
  )
    'ok
)
