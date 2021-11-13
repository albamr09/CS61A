;; Exercise 2.
;; Write a function that takes a word and returns the second letter.

(define (second wd)
 (first (bf wd))
)

;; This should output o
(second 'fox)
