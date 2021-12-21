#lang racket

; Get gcd function
(require "../T1/Racket/P02_05.rkt")

; Constructors
(define (make-rat n d) 
  (let
    ; Get common divisor of n and d
    ((g (gcd n d)))
    ; Divide numerator and denominator by g
    (cons (/ n g) (/ d g))
  )
)

; Selectors
(define (numer x) (car x))
(define (denom x) (cdr x))

; Operations

(define (add-rat x y)
    ; x =a/b y = c/d
    (make-rat 
        (+ 
            ; numerator: ad + bc  
            (* (numer x) (denom y))
            (* (numer y) (denom x))
        )
        ; denominator: multiply denominators of x and y
        (* (denom x) (denom y))
    )
)

(define (sub-rat x y)
    ; x =a/b y = c/d
    (make-rat 
        (- 
            ; numerator: ad + bc  
            (* (numer x) (denom y))
            (* (numer y) (denom x))
        )
        ; denominator: multiply denominators of x and y
        (* (denom x) (denom y))
    )
)

(define (mul-rat x y)
  (make-rat 
    ; Multiply numerators
    (* (numer x) (numer y))
    ; Multiply denominators
    (* (denom x) (denom y))
  )
)

(define (div-rat x y)
  ; x =a/b y = c/d
  (make-rat 
    ; Numerator: ad
    (* (numer x) (denom y))
    ; Denominator: bc
    (* (denom x) (numer y))
  )
)

(define (equal-rat? x y)
  ; x =a/b y = c/d
  ; x = y iff ad = bc
  (= 
    ; ad
    (* (numer x) (denom y))
    ; bc
    (* (numer y) (denom x))
  )
)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

(print-rat one-half)
; 1/2
(print-rat (add-rat one-half one-third))
; 5/6
(print-rat (mul-rat one-half one-third))
; 1/6
(print-rat (add-rat one-third one-third))
; 6/9

(newline)
