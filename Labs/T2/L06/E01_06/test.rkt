#lang racket
(require berkeley)

; Import apply generic
(require "../../../../BProblems/T2/P04_02/apply.rkt")
; Import table
(require "../../../../BProblems/T2/P04_02/table.rkt")
; Import arithmetic package.
(require "./arithmetic-pkg.rkt")

; Define a generic predicate =zero? that tests if its argument is 
; zero, and install it in the generic arithmetic package. This operation 
; should work for ordinary numbers, rational numbers, and complex numbers.

;;;;;;;;;;;;;;;;;
;;; Constructors
;;;;;;;;;;;;;;;;;

; Scheme number
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n)
)

; Rational number
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
(define (equ? x y) (apply-generic 'equ? x y))
; New operation
(define (=zero? x) (apply-generic '=zero? x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST

; (install-arithmetic-pkg)
; 
; (define complex-num-ma-a (make-complex-from-mag-ang 0 0))
; (define complex-num-ma-b (make-complex-from-mag-ang 1 3))
; (define complex-num-rm-a (make-complex-from-real-imag 0 0))
; (define complex-num-rm-b (make-complex-from-real-imag 1 3))
; (define rational-num-a (make-rational 0 1))
; (define rational-num-b (make-rational 1 8))
; (define scheme-num-a (make-scheme-number 0))
; (define scheme-num-b (make-scheme-number 9))
; 
; (=zero? scheme-num-a)
; ; t
; (=zero? scheme-num-b)
; ; f
; 
; (=zero? rational-num-a)
; ; t
; (=zero? rational-num-b)
; ; f
; 
; (=zero? complex-num-rm-a)
; ; t
; (=zero? complex-num-rm-b)
; ; f
; 
; (=zero? complex-num-ma-a)
; ; t
; (=zero? complex-num-ma-b)
; ; f
