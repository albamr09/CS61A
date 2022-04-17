;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E01_01
; SICP 4.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Rewrite eval in data-directed style.
; Load eval-pkg
(load "./")


(define eval-table (make-table))

(define (type-exp exp)
  (cond
    ((pair? exp) (car exp))
    ((number? exp) 'number)
    ((string? exp) 'string)
    ((symbol? exp) 'symbol)
    (else 
      (error "Unknown expression type -- EVAL" exp)
    )
  )
)


(define (dispatch-eval exp env)
  (let
    ((type (type-exp exp)))
    (let
      ((proc (lookup type eval-table)))
      (if proc
        (proc exp env)
        ((lookup 'application eval-table) exp env)
      )
    )
  )
)

(trace type-exp)
(print (type-exp 1))
(print (lookup 'cond eval-table))
(dispatch-eval '(define a (lambda (x) x)) the-global-environment)
