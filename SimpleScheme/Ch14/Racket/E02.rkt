#lang racket

(require berkeley)

(define (up wd)
  (if (empty? wd)
    '()
    (se (up (bl wd)) wd)
  )
)

(up 'town)
; (T TO TOW TOWN)
