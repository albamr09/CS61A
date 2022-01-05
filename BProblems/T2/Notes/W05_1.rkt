#lang racket
(require berkeley)

; REPL: read-eval-print-loop
(define (calc)
  (display "calc: ")
  ; (flush)
  ; read: Scheme primitive to read from std input
  (print (calc-eval (read)))
  (newline)
  (calc)
)

; Evaluates an expression given by exp
; in the form of a list

(define (calc-eval exp)
  (cond 
    ; If it is a number, then return the number
    ((number? exp) exp)
    ; If it is a list, apply the operator on
    ; the operands
    ((list? exp) 
      (calc-apply 
        ; Operator, first element of the list
        (car exp) 
        ; Operands, given by calling recursively
        ; cal-eval on rest of the list
        (map calc-eval (cdr exp))
      )
    )
    ; This calculator works only on numbers
    (else (error "Calc: bad expression:" exp))
  )
)

(define (calc-apply fn args)
  (cond 
    ; Apply + to all of the arguments
    ; because + is associative
    ((eq? fn '+) (accumulate + 0 args))
    ((eq? fn '-) 
      (cond 
        ; If there are no arguments, throw an error
        ((null? args) (error "Calc: no args to -"))
        ; If there is only one argument, return its inverse
        ((= (length args) 1) (- (car args)))
        (else 
          (- 
            ; Substract the sum of all of
            ; the arguments but the first to the first
            ; a - b - c = a - (b + c)
            (car args) 
            (accumulate + 0 (cdr args))
          )
        )
      )
    )
    ; Apply * to all of the arguments
    ; because * is associative
    ((eq? fn '*) (accumulate * 1 args))
    ((eq? fn '/) 
      (cond 
        ; If there are no arguments, throw an error
        ((null? args) (error "Calc: no args to /"))
        ; if there is only one argument
        ((= (length args) 1) (/ (car args)))
        (else 
          (/ 
            ; Divide the fist argument by the product of all of
            ; the arguments but the first
            ; a / b / c = a / (b * c)
            (car args) 
            (accumulate * 1 (cdr args))
          )
        )
      )
    )
    ; If the operation is not +, -, * or /, throw an error
    (else (error "Calc: bad operator:" fn))
  )
)

; (calc)

