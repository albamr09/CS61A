#lang racket
(require berkeley)

; Auxilary function to generate random numbers
(define (rand-update x)
  (let 
    ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)
  )
)

; Define random-init to be a fixed number
(define random-init 21)

; Generate random numbers

(define rand 
  ; Initialize x
  (let ((x random-init))
    (lambda ()
      ; Update the value of x
      (set! x (rand-update x))
      ; Return x
      x
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; MONTE CARLO METHOD

; ----
;; Explanation
; ----

; The Monte Carlo method consists of choosing sample experiments
; at random from a large set and then making deductions on the basis of
; the probabilities estimated from tabulating the results of those experiments. 

; ---
;; Example of application
; ---

; For example, we can approximate π using the fact that 6/π² is
; the probability that two integers chosen at random will have no factors in 
; common; that is, that their greatest common divisor will be 1.

; To obtain the approximation to π, we perform a large number of experiments. 
; In each experiment we choose two integers at random and
; perform a test to see if their gcd is 1. e fraction of times that the test
; is passed gives us our estimate of 6/π², and from this we obtain our approximation to π.


; - trials: number of times to run the experiment
; - experiment: procedure with no arguments that 
;               executes the experiment and returns true
;               if the experiment was successfull or false
;               otherwise

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond 
      ; If we have already run the experiment all of the 
      ; specified times, finish
      ((= trials-remaining 0)
        ; Return the ratio of success vs times run
        (/ trials-passed trials)
      )
      ; If the experiment was successfull
      ((experiment)
        ; Continue the approximation
        (iter 
          ; Update the times left to run
          (- trials-remaining 1)
          ; Update the successfull trials
          (+ trials-passed 1)
        )
      )
      ; If the experiment was not successfull
      (else
        ; Continue
        (iter 
          ; Update the times left to run
          (- trials-remaining 1)
          ; Do not update the successfull trials
          trials-passed
        )
      )
    )
  )
  ; Start the iterative approximation
  (iter trials 0)
)


;;;;
; TEST
;;;;

(define (estimate-pi trials)
  ; 6/π² is the probability that two
  ; random numbers will be relatively prime
  ; Resolve the equation to obtain the pi estimate
  (sqrt 
    (/ 6 
      ; Obtain the approximation for 6/π²
      (monte-carlo 
        ; Number of times to perform the experiment
        trials 
        ; Experiment (is the gcd of two random numbers a, b equal to 1?)
        cesaro-test
      )
    )
  )
)

; Are two random numbers a, b relatively prime?
; as is: gcd(a, b) = 1
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1)
)

; (estimate-pi 10000)
