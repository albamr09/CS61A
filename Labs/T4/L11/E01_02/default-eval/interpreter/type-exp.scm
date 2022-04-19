;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GET TYPE OF EXPRESSION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Get the type of the expression
(define (type-exp exp)
  (cond
    ; If it is a pair, the type is the first element
    ((pair? exp) (car exp))
    ; Numbers and strings are self-evaluating
    ((or
       (number? exp)
       (string? exp)
      )
      'self-evaluating
    )
    ; Variables are symbols
    ((symbol? exp) 'variable)
    ; Else throw error
    (else 
      (error "Unknown expression type -- EVAL" exp)
    )
  )
)
