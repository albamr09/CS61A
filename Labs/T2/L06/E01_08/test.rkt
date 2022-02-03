#lang racket
(require berkeley)

; Import tag system
(require "../../../../BProblems/T2/P04_01/tags.rkt")
; Import apply generic
(require "../E01_07/apply.rkt")
; Import tables
(require "../../../../BProblems/T2/P04_02/table.rkt")
(require "../../../../BProblems/T2/P05_02/table-coercion.rkt")
; Import tower hierarchy package
(require "./tower-hierarchy-pkg.rkt")
; Import raise generic
(require "./raise.rkt")

; Suppose you are designing a generic arithmetic system for dealing with the tower of types shown in
; Figure 2.25: integer, rational, real, complex. For each type
; (except complex), design a procedure that raises objects of
; that type one level in the tower. Show how to install a
; generic raise operation that will work for each type (except complex).

(install-tower-hierarchy-package)

;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST


; Scheme number
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n)
)

(define (make-rational x y)
  ((get 'make 'rational) x y)
)


(define scheme-num-a (make-scheme-number 0))
(define scheme-num-b (make-scheme-number 9))
(define rational-a (make-rational 3 5))
(define rational-b (make-rational 8 10))

; Integer to rational
(raise-generic scheme-num-a)
; Rational to real
(raise-generic (raise-generic scheme-num-a))
; Real to complex
(raise-generic (raise-generic (raise-generic scheme-num-a)))

; Integer to rational
(raise-generic scheme-num-b)
; Rational to real
(raise-generic (raise-generic scheme-num-b))
; Real to complex
(raise-generic (raise-generic (raise-generic scheme-num-b)))

; Rational to real
(raise-generic rational-a)
(raise-generic rational-b)
; Real to complex
(raise-generic (raise-generic rational-a))
(raise-generic (raise-generic rational-b))
