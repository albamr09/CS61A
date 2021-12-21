#lang racket

(require berkeley)

; The following program doesn't work. Why not? Fix it.

; (define (superlative adjective word)
;   (se (word adjective 'est) word))

; It is using the keyword 'word' as an argument

(define (superlative adjective wd)
  (se (word adjective 'est) wd)
)

;; This should output (dumbest exercise)
(superlative 'dumb 'exercise)
