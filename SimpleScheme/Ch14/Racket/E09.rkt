#lang racket

(require berkeley)

; Write a procedure called location that takes two arguments, a word and a
; sentence. It should return a number indicating where in the sentence that word can be
; found. If the word isn’t in the sentence, return . If the word appears more than once,
; return the location of the first appearance.

(define (location-helper wd sent count)
  (cond
    ((empty? sent) #f)
    ((equal? (first sent) wd) count) 
    (else (location-helper wd (bf sent) (+ 1 count)))
  )
)

(define (location wd sent)
  (location-helper wd sent 1)
)

(location 'me '(you never give me your money))
; 4
