; Fibonacci Tree recursive
(define (fib n)
  (cond 
    ; Initial conditions for the sequence
    ((= n 0) 0)
    ((= n 1) 1)
    ; Recursive definition for the sequence
    (else 
      (+ 
        ; a_n-1
        (fib (- n 1))
        ; a_n-2
        (fib (- n 2))
      )
    )
  )
)

(fib 5)

; Fibonacci Iterative
(define (fib n)
  (fib-iter 1 0 n)
)

(define (fib-iter a b count)
  ; It sums iteratively a term with the previous term because a_n = a_n-1 + a_n-2
  (if (= count 0)
    ; Return b, was a before
    b 
    (fib-iter 
      ; a = a + b
      (+ a b) 
      ; b = a
      a 
      ; Update couter
      (- count 1)
    )
  )
)

(fib 5)
