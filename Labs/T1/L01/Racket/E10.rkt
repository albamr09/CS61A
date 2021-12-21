#lang racket

(require berkeley)

; Write a procedure ends-e that takes a sentence as its argument and returns a
; sentence containing only those words of the argument whose last letter is E.

;; Exercise 10.
(define (ends-e sent)
  (cond
    ((empty? sent) '())
    ((equal? (last (first sent)) 'e) (se (first sent) (ends-e (bf sent))))
    (else (ends-e (bf sent)))
  )
)
(ends-e '(please put the salami above the blue elephant))
;; Outputs: (please the above the blue)
 
