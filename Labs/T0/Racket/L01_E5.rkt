#lang racket

(require berkeley)

; Exercise 5
; See what happens when you type the following snippets of code into the interpreter

(define (infinite-loop) (infinite-loop))

; It does not break: it evaluates the condition to false so it executes the else-case
(if (= 3 6)
  (infinite-loop)
  (/ 4 2))

(define (new-if test then-case else-case)
  (if test
    then-case
    else-case))

; Breaks: cannot divide by zero
(new-if (= 3 6)
  (/ 1 0)
  (/ 4 2))

;; Let's try it out: infinite loop, never ends
(new-if (= 3 6)
  (infinite-loop)
  (/ 4 2))
