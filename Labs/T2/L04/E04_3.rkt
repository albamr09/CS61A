#lang racket
(require berkeley)

; Here is an alternative procedural representa-
; tion of pairs. For this representation, verify that (car (cons
; x y)) yields x for any objects x and y.

(define (proc-cons x y)
  ; Return procedure
  (lambda 
    ; Formal argument: in our case
    ; m can be proc-car or proc-cdr
    (m) 
    ; Apply m procedure over x 
    ; and y
    (m x y)
  )
)

(define (proc-car z)
  ; Apply z procedure
  (z 
    ; With anonymous function as 
    ; argument
    (lambda 
      ; Formal arguments
      ; In (m x y)
      ; m is this anonymous function
      ; p = x and q = y
      (p q) 
      p
    )
  )
)

; What is the corresponding definition of cdr? 

(define (proc-cdr z)
  ; Apply z procedure
  (z 
    ; With anonymous function as 
    ; argument
    (lambda 
      ; Formal arguments
      ; In (m x y)
      ; m is this anonymous function
      ; p = x and q = y
      (p q) 
      q
    )
  )
)

;; TEST

(define pair (proc-cons 1 2))
; 1
(proc-car pair)
; 2
(proc-cdr pair)
