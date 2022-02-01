#lang racket
(require berkeley)

; Define a generic equality predicate equ? that tests the equality of 
; two numbers, and install it in the generic arithmetic package. is operation 
; should work for ordinary numbers, rational numbers, and complex numbers.

; Import apply generic
(require "../../../../BProblems/T2/P04_02/apply.rkt")
; Import table
(require "../../../../BProblems/T2/P04_02/table.rkt")
; Import arithmetic package
(require "./arithmetic-pkg.rkt")

;;;;;;;;;;;;;;;;;
;;; Constructors
;;;;;;;;;;;;;;;;;

; Scheme number
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n)
)

; Ration number
(define (make-rational n d)
  ((get 'make 'rational) n d)
)

; Complex number (rectangular representation)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y)
)

; Complex number (polar representation)
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a)
)

;;;;;;;;;;;;;;;;;
;;; Arithmetic Operations
;;;;;;;;;;;;;;;;;

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
; New operation
(define (equ? x y) (apply-generic 'equ? x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST

; (install-arithmetic-pkg)
; 
; (define complex-num-ma (make-complex-from-mag-ang 1 3))
; (define complex-num-rm (make-complex-from-real-imag 1 3))
; (define rational-num-a (make-rational 3 5))
; (define rational-num-b (make-rational 1 8))
; (define scheme-num-a (make-scheme-number 3))
; (define scheme-num-b (make-scheme-number 9))
; 
; (equ? rational-num-a rational-num-b)
; ; f
; (equ? rational-num-a rational-num-a)
; ; t
; 
; (equ? scheme-num-a scheme-num-b)
; ; f
; (equ? scheme-num-a scheme-num-a)
; ; t
; 
; (equ? complex-num-rm complex-num-ma)
; ; f
; (equ? complex-num-rm complex-num-rm)
; ; t
; (equ? complex-num-ma complex-num-ma)
; ; t
