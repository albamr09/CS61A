;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; E04_5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
SICP 3.27

Explain why the number of steps is proportional to n (you may want to
include a trace to explain).

Both versions of the procedures are implemented in Bproblems/Notes/W09.scm

-----------------------------------
STk> (fib 3)
.. -> fib with n = 3
.... -> fib with n = 2
...... -> fib with n = 1
...... <- fib returns 1
...... -> fib with n = 0
...... <- fib returns 0
.... <- fib returns 1
.... -> fib with n = 1
.... <- fib returns 1
.. <- fib returns 2
2
-----------------------------------
STk> (fast-fib 3)
.. -> fast-fib with n = 3
.... -> fast-fib with n = 2
...... -> fast-fib with n = 1
...... <- fast-fib returns 1
...... -> fast-fib with n = 0
...... <- fast-fib returns 0 (stored)
.... <- fast-fib returns 1 (stored)
.... -> fast-fib with n = 1
.... <- fast-fib returns 1
.. <- fast-fib returns 2
2
-----------------------------------

Basically in fast-fib, because we are remembering every computation we do for every Fibonacci 
number, the amount of fundamental operations (in this case sums) are reduced to the number of 
integers between 1 and n. That is to say, once we compute F(n), we never compute it again
Which means that the order of the algorithm is Theta(n).

Note we are not mentioning the cost of accessing memory.

Would it still work (efficiently) if we define memo-fib as (memoize
fib)?  Why or why not?

As you can see in the memo-fib procedure we call recursively 
to memo-fib, which allows us to keep track of **all** of the values calculated.
However, if we were to define it as (memoize fib) we would only store inside
the table the first value calculated, because fib does not make any reference
to the memoize procedure and just computes away.

|#

; Load table methods
(load "../../../BProblems/T3/P03_03/table_1D.scm")

; Procedure to remember results from a calculation
(define (memoize f)
  ; Initialize table: it is created with the definition of
  ; this procedure, not each time we call it
  (let ((table (make-table)))
    ; For a value x
    (lambda (x)
      (let 
        ; Search it on the table
        (
          (previously-computed-result (lookup x table))
        )
        (or 
          ; If it exists, return it (the or function returns as soon as it 
          ; reaches a true value, and every object is a true value)
          previously-computed-result
          ; else
          (let 
            ; Apply the function f to x, to obtain the value
            ((result (f x)))
            ; Store the value
            (insert! x result table)
            ; Return the value
            result
          )
        )
      )
    )
  )
)

(define memo-fib
  (memoize
    ; Function to apply n, the resulting
    ; value is stored in a table
    (lambda (n)
      (cond 
        ((= n 0) 0)
        ((= n 1) 1)
        (else 
          (+ 
            ; Remember the values of F(n-1) and F(n-2)
            (memo-fib (- n 1))
            (memo-fib (- n 2))
          )
        )
      )
    )
  )
)

; Regular implemetation of fibonacci
(define (fib n)
  (cond 
    ((= n 0) 0)
    ((= n 1) 1)
    (else 
      (+ 
        (fib (- n 1)) 
        (fib (- n 2))
      )
    )
  )
)

(trace memo-fib)
(print (memo-fib 3))
