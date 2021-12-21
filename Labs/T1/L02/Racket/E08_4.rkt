#lang racket
(require berkeley)

; On accumulate combine only those terms derived from values in the range that satisfy 
; a specified condition. The resulting filtered-accumulate abstraction takes the 
; same arguments as accumulate, together with an additional predicate of one argument that 
; specifies the filter. Write filtered-accumulate as a procedure. 

(define (filtered-accumulate combiner null-value term a next b filter)
  ; Iterative procedure
  (define (iter a result)
    (cond 
      ; If the lower bound > upper bound return result
      ((> a b) result)
      ; If the term satisfies the filter apply combination
      ((filter a) 
        (iter 
          ; Obtainer next value of sequence
          (next a) 
          ; Combine the current result with the 
          ; transformation of the current term
          (combiner result (term a))
        )
      )
      ; Continue with next term without updating the result
      (else (iter (next a) result))
    )
  )
  ; First invocation of iterative method
  (iter a null-value)
)

; If prime definition
(define (prime? n)
  (define (loop i)
    (cond
      ((= i n) #t)
	    ((= (remainder n i) 0) #f)
	    (else (loop (+ i 1)))
    )
  )
  (if (<= n 1)
    #f
    (loop 2)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                               
; Sum of the squares of the prime numbers in the interval a to b
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                               

(define (sum-squares-primes a b)
  (define (inc n) (+ 1 n))
  (define (sum x y) (+ x y))
  (filtered-accumulate 
    ; Combiner
    sum 
    ; Starting value
    0 
    ; Function applied over each term
    square 
    ; Lower bound
    a
    ; How to get to the next term
    inc 
    ; Upper bound
    b
    ; Filter
    prime?
  )
)

(define (sum-squares-primes-lambda a b)
  (filtered-accumulate 
    ; Combiner
    (lambda (x y) (+ x y)) 
    ; Starting value
    0 
    ; Function applied over each term
    square 
    ; Lower bound
    a
    ; How to get to the next term
    (lambda (x) (+ 1 x))
    ; Upper bound
    b
    ; Filter
    prime?
  )
)

(sum-squares-primes 2 3)
; 13

(sum-squares-primes-lambda 2 3)
; 13


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                               
; Product of all the positive integers less than n that
; are relatively prime to n 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                               

; Greatest common divisor definition
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))
  )
)

; Product of relative primes definition
(define (product-relative-primes n)
  (define (product x y) (* x y))
  (define (identity x) x)
  (define (inc x) (+ 1 x))
  (define (is-relative-prime? x) (= (gcd x n) 1))
  (filtered-accumulate
    product
    1
    identity 
    1
    inc
    n
    is-relative-prime?
  )
)

; Product of relative primes definition with lambda
(define (product-relative-primes-lambda n)
  (filtered-accumulate
    ; Combiner
    (lambda (x y) (* x y))
    1
    ; Transform term
    (lambda (x) x)
    1
    ; Get next term
    (lambda (x) (+ 1 x))
    n
    ; Filter: is x relatively prime with n
    (lambda (x) (= (gcd x n) 1))
  )
)

(product-relative-primes 2)
(product-relative-primes-lambda 2)
; 1 and 2 relative primes: 1
(product-relative-primes 3)
(product-relative-primes-lambda 3)
; 1 and 2 relatives primes with 3: 2
(product-relative-primes 4)
(product-relative-primes-lambda 4)
; 1 and 3 relative primes with 4: 3
(product-relative-primes 5)
(product-relative-primes-lambda 5)
; 1, 2, 3 and 4 relative primes with 5: 24
