#lang racket

(require berkeley)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Find if a number is a prime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (divides? a b) 
  ; Test if a | b
  (= (remainder b a) 0)
)

(define (find-divisor n test-divisor)
  (cond 
    ; If test-divisor > sqrt(n), then return n
    ; as the smallest divisor of itself
    ((> (square test-divisor) n) n)
    ; If test-divisor divides n, then it is its smallest divisor
    ((divides? test-divisor n) test-divisor)
    ; Else continue searching and update test-divisor adding 1
    (else (find-divisor n (+ test-divisor 1)))
  )
)

; Get the smallest divisor of n
(define (smallest-divisor n) 
  (find-divisor n 2)
)

(define (prime? n)
  ; n is primer if its smallest divisor is itself
  (= n (smallest-divisor n))
)

(prime? 5)
(prime? 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Find prime with Fermat's Little Theorem
; If n is a prime and a is a positive integer where a < n, then a^n = a (mod n)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Computes the exponential of a number modulo another number

(define (expmod base exp m)
  ; Let n = exp
  (cond 
    ; a^0 = 1, for all numbers a
    ((= exp 0) 1)
    ((even? exp)
     ; Get a^n (mod m)
      (remainder
        ; Given a^n, if n is even compute {a^(n/2)}^2, where a^(n/2) is computed recursively
        (square (expmod base (/ exp 2) m))
        m
      )
    )
    (else
     ; Get a^n (mod m)
      (remainder
        ; Given a^n, if n is odd compute a*{a^(n-1)}, where a^(n-1) is computed recursively
        (* base (expmod base (- exp 1) m))
        m
      )
    )
  )
)

; The Fermat test is performed by choosing at random 
; a number a between 1 and n−1 inclusive and checking 
; whether the remainder modulo n of the nth power of a 
; is equal to a. 

(define (fermat-test n)
  ; Is a^n = a (mod n) (congruent with, 
  ; they have the same remainder when divided by n)
  ; (expmod a n n) obtains a^n (mod n)
  (define (try-it a)
    (= (expmod a n n) a)
  )

  (try-it 
    ; Choose a random number
    ; Random returns an integer in the interval [1, n - 1)
    ; To get [1, n - 1], we sum one to the input
    (+ 
      1 
      (random (- n 1))
    )
  )
)

; ------------
; Description of the fermat test
; ------------

; Given a number n, pick a random number a < n and
; compute the remainder of an modulo n. If the result is not equal to a,
; then n is certainly not prime. If it is a, then chances are good that n is
; prime. Now pick another random number a and test it with the same
; method. If it also satisfies the equation, then we can be even more 
; confident that n is prime. By trying more and more values of a, we can
; increase our confidence in the result.

; ------------

; The procedure, as it relies on trying several time to increase the confidence
; that n is prime, run as many times as the variable times
(define (fast-prime? n times)
  (cond 
    ; If the number of iterations equal zero
    ((= times 0) true)
    ; If a^n = a (mod n), so it satisfies Fermat's Little Theorem
    ; Continue testing recursively with another random a,
    ; and decrease the number of iterations left
    ((fermat-test n) 
      (fast-prime? n (- times 1))
    )
    (else false))
)

(fast-prime? 5 100)
(fast-prime? 4 100)
