#lang racket
(require berkeley)

; Import apply generic
(require "../P04_02/apply.rkt")
; Import table
(require "../P04_02/table.rkt")
; Import packages
(require "./complex-pkg.rkt")
(require "./rational-pkg.rkt")
(require "./scheme-number-pkg.rkt")

(define (install-arithmetic-pkg)
  (install-complex-package)
  (install-scheme-number-package)
  (install-rational-package)
  'done
)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST

(install-arithmetic-pkg)

(define complex-num-ma (make-complex-from-mag-ang 1 3))
(define complex-num-rm (make-complex-from-real-imag 1 3))
(define rational-num-a (make-rational 3 5))
(define rational-num-b (make-rational 1 8))
(define scheme-num-a (make-scheme-number 3))
(define scheme-num-b (make-scheme-number 9))

(add scheme-num-a scheme-num-b)
; (scheme-number 12)
(sub scheme-num-a scheme-num-b)
; (scheme-number -6)
(mul scheme-num-a scheme-num-b)
; (scheme-number 27)
(div scheme-num-a scheme-num-b)
; (scheme-number 1/3)

