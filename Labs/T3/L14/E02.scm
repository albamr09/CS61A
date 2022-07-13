(load "../../../lib/concurrency.scm")
; (load "../../../BookProblems/T3/P03_04/P01.scm")

; Which of the five possibilities in the parallel execution shown above remain 
; if we instead serialize execution as follows:

(define x 10)

(define s (make-serializer))

(parallel-execute
  ; First procedure = P1
  (lambda () 
    ; We do not serialize the writing
    (set! x 
      ; We only serialize the reading of x and the computing of x*x
      ((s (lambda () (* x x))))
    )
  )
  ; Second procedure = P2: we serialize both reading and writing
  (s 
    (lambda () (set! x (+ x 1)))
  )
)

Given the possibilities before serilizing:

; Index Result      Action Sequence                                               Still possible
; 1     101:        P1 sets x to 100 and then P2 increments x to 101.             YES
; 2     121:        P2 increments x to 11 and then P1 sets x to x * x.            YES
; 3     110:        P2 changes x from 10 to 11 between the two times that         NO
;                   P1 accesses the value of x during the evaluation of (* x x).
; 4     11:         P2 accesses x, then P1 sets x to 100, then P2 sets x.         NO
; 5     100:        P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.  YES

; The first two cases are just the sequential execution but in different orders, so the serialization does not affect them.

; In the 3rd case P2 has to finish executing before P1 can do anything, therefore this case is not possible. We do not allow P1's execution interleaved with any of P2's execution

; The 4th one cannot be done because the serializer does not allow for another process to run while the second procedure is running

; Lastly, the 5th case is still possible because:
; 1. Serialized part: P1 acceses x two times and then perform the operation x*x
; 2. Serialized part: Then P2 modifies x
;   1. It reads x
;   2. It adds 1 to x
;   3. It stores x
; 3. Non serialized: P1 stores the result of x*x for x = 10 into x

; Because we split P1 into the reading-computing part (which is the one serialized) and the writing part, P1 can be executed in between those two.
