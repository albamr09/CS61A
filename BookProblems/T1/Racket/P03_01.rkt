#lang racket

(require berkeley)

; Cube
(define (cube x) 
  (* x x x)
)

; High order procedure to sum a series from a to b
; - term: operation over a, that defines the series
; - next: how to calculate the next term of the series
; - a: lower bound
; - b: upper bound

(define (sum term a next b)
  (if (> a b)
    0
    (+ 
      ; Apply the function over a
      (term a)
      ; Call the sum recursively, pass it a modified
      (sum term (next a) next b)
    )
  )
)

; Sum of cubes from a to b
(define (inc n) 
  (+ n 1)
)

(define (sum-cubes a b)
  ; Specify cube as the operation over each term 
  ; and inc as how to get the next a in the series
  (sum cube a inc b)
)

(sum-cubes 1 10)
; 3025

; Sum of integers

(define (identity x) x)

(define (sum-integers a b)
  ; Specify identity as the operation over earch term
  ; and inc as how to get the next a in the series
  (sum identity a inc b)
)

(sum-integers 1 10)
; 55

; Sum of pi

(define (pi-sum a b)
  ; Operation to appy over each term
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2)))
  )
  ; How to get the next a in the series
  (define (pi-next x)
    (+ x 4)
  )
  ; Call the sum procedure
  (sum pi-term a pi-next b)
)

(* 8 (pi-sum 1 1000))
; 3.139592655589783

; Calculate the integral
; - f: operation over each term
(define (integral f a b dx)
  ; How to get the next a in the series
  (define (add-dx x)
    (+ x dx)
  )
  (* 
    (sum f (+ a (/ dx 2.0)) add-dx b) 
    dx
  )
)

(integral cube 0 1 0.01)
; .24998750000000042
(integral cube 0 1 0.001)
; .249999875000001

(define (f x y)
  (
    ; Definition of the anonymous procedure
    (
      ; Definition with formal arguments
      lambda (a b)
      ; Procedure's body
      (+ (* x (square a))
      (* y b)
      (* a b))
    )
    ; Arguments for the procedure
    (+ 1 (* x y))
    (- 1 y)
  )
)
