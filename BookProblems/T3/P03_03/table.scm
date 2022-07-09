;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABLE OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-table)
  (let 
    ; Table as local variable
    ((local-table (list '*table*)))

  ;; Methods

  ; See table_2D.scm for explanation
  (define (lookup key-1 key-2)
    (let 
      ((subtable (assoc key-1 (cdr local-table))))
      (if subtable
        (let 
          ((record (assoc key-2 (cdr subtable))))
          (if record 
            (cdr record) 
            false
          )
        )
        false
      )
    )
  )

  ; See table_2D.scm for explanation
  (define (insert! key-1 key-2 value)
    (let 
      ((subtable (assoc key-1 (cdr local-table))))
      (if subtable
        (let 
          ((record (assoc key-2 (cdr subtable))))
          (if record
            (set-cdr! record value)
            (set-cdr! subtable
              (cons 
                (cons key-2 value)
                (cdr subtable)
              )
            )
          )
        )
        (set-cdr! 
          local-table
          (cons 
            (list key-1 (cons key-2 value))
            (cdr local-table)
          )
        )
      )
    )
    'ok
  )

  ; Generic method
  (define (dispatch m)
    (cond 
      ((eq? m 'lookup-proc) lookup)
      ((eq? m 'insert-proc!) insert!)
      (else (error "Unknown operation: TABLE" m))
    )
  )
  dispatch)
)

;;; TEST

; Create table
(define operation-table (make-table))

; Define operations on table 
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

