#lang racket

(require berkeley)

(define (remove-once-helper wd sent remaining)
  (cond 
    ((empty? sent) '())
    ((and (= remaining 1) (equal? wd (first sent))) (se (remove-once-helper wd (bf sent) 0)))
    (else (se (first sent) (remove-once-helper wd (bf sent) remaining)))
  )
)

(define (remove-once wd sent)
  (remove-once-helper wd sent 1)
)

(remove-once 'morning '(good morning good morning))
; (GOOD GOOD MORNING)

(remove-once 'good '(good morning good morning))
(remove-once "" '(good morning good morning))
