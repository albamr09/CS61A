;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULARITY OF FUNCTIONAL PROGRAMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For an explanation on the Monte Carlo Method refer to "/BProblems/T3/P01_02.rkt"

; When using assignment in the Monte Carlo procedure we stored a local variable
; with the value of a random number, produced by a random generator.
; In the stream formulation there is no random-number generator per se,
; just a stream of random numbers produced by successive calls to randupdate:

(define random-init 1)

(define random-numbers
  (cons-stream
    random-init
    (stream-map rand-update random-numbers)
  )
)

; We use this to construct the stream of outcomes of the Cesàro experiment 
; performed on consecutive pairs in the random-numbers stream

(define (map-successive-pairs f s)
  (cons-stream
    ; First element -> boolean: for the cesaro-experiment -> are s[i] 
    ; and s[i-1] relatively prime?
    (f 
      ; s[i]
      (stream-car s) 
      ; s[i-1]
      (stream-car (stream-cdr s))
    )
    ; Next elements
    (map-successive-pairs f (stream-cdr (stream-cdr s)))
  )
)

(define cesaro-stream
  (map-successive-pairs
    ; Check if r1 and r2 are relatively prime
    ; for each succesive pair of numbers in random-number
    (lambda (r1 r2) (= (gcd r1 r2) 1))
    random-numbers
  )
)


; The cesaro-stream is now fed to a monte-carlo procedure, which produces a stream 
; of estimates of probabilities. e results are then converted into a stream of 
; estimates of π. This version of the program doesn't need a parameter telling how 
; many trials to perform. Better estimates of π (from performing more experiments) 
; are obtained by looking farther into the pi stream:

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      ; Obtain the estimate for the current iteration
      (/ passed (+ passed failed))
      ; Continue obtaining estimates for next iterations
      (monte-carlo
        (stream-cdr experiment-stream) passed failed
      )
    )
  )
  ; If the current element on the experiment stream is true
  (if (stream-car experiment-stream)
    ; Update the tests passed 
    (next (+ passed 1) failed)
    ; Otherwise update the tests failed
    (next passed (+ failed 1))
  )
)

;;;;;;;;;;;;;;;;
;; TEST
; 6/π² is the probability that two
; random numbers will be relatively prime
; Resolve the equation to obtain the pi estimate

(define pi
  (stream-map
    ; Obtain pi estimation for each element on the monte-carlo stream
    ; each element is the estimate of a iteration
    (lambda (p) (sqrt (/ 6 p)))
    ; Calculate monte-carlo stream
    (monte-carlo cesaro-stream 0 0)
  )
)

; Prints stream
(print pi)

; Print 1000th element of the pi estimate
(print
  (stream-ref 
    pi
    1000
  )
)

; Print 10000th element of the pi estimate
(print
  (stream-ref 
    pi
    10000
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONAL PROGRAMMING VIEW OF TIME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Using local variables to model state

(define (make-simplified-withdraw balance)
  (lambda 
    (amount)
    (set! balance (- balance amount))
    balance
  )
)

; Alternatively, we can model a withdrawal processor as a procedure
; that takes as input a balance and a stream of amounts to withdraw and
; produces the stream of successive balances in the account:

(define (stream-withdraw balance amount-stream)
  (cons-stream
    ; First element of the stream is the original balance of the account
    balance
    (stream-withdraw 
      ; Next values are the current balance - the first element of the amount stream
      (- balance (stream-car amount-stream))
      (stream-cdr amount-stream)
    )
  )
)

; So, given an initial balance of 100
; Initially stream-withdraw = {100, ...}
; If we have an amount-stream = {10, 10, 5, 20, ...}
; So if we feed it to stream-withdraw, we have {100, 100 - 10, 100 - 10 - 10, 100 - 10 - 10 - 5, 
; 100 - 10 - 10 - 5 -20}
; Therefore stream-withdraw = {100, 90, 80, 75, 55, ...}

(define s (stream-withdraw 100 (cons-stream 10 (cons-stream 10 the-empty-stream))))

(print (stream-ref s 0))
; 100
(print (stream-ref s 1))
; 90
(print (stream-ref s 2))
; 80
