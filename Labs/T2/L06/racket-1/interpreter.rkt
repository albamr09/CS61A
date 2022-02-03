#lang racket

(require "./racket1.rkt")

(racket-1)

;; Sample evaluation, computing factorial of 5:

;((lambda (n)
;    ((lambda (f) (f f n))
;      (lambda (f n)
;        (if (= n 0)
;          1
;          (* n (f f (- n 1))) 
;        )
;      )
;    )
;  )
;  5
;)

; 120

;; Sample evaluation, using a primitive as argument to MAP:

;((lambda (f n)
;    ((lambda (map) (map map f n))
;      (lambda (map f n)
;        (if (null? n)
;          '()
;          (cons (f (car n)) (map map f (cdr n)))
;        )
;      )
;    )
;  )
;  first
;  '(the rain in spain)
;)

; (t r i s)
