;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; E06
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Exercise 6. Write vector-filter.

(define (vector-filter pred v) 
  ; Create a vector made up of only the elements of v that satisfy pred
  ; - i: index of the current index for newvec
  ; - j: index of the current index for filtered-vector
  ; - filtered-vector: vector made up of only the elements of f that satisfy pred
  (define (compress newvec i j filtered-vector)
    (cond
      ; If j < 0, we have reached the start of the vector, so we have finished
      ((< j 0) filtered-vector)
      ; If the current element newvec[i] is null, then it did not satisfy the predicate pred
      ; so we do not include it in the filtered vector
      ((null? (vector-ref newvec i))
        ; Keep going through the vector (backwards), note we do not update j, that is the 
        ; index for filtered-vector, because there was no update
        (compress newvec (- i 1) j filtered-vector)
      )
      ; If not
      (else
        ; Update the value of filtered-vector[j] to the value of newvec[i]
        (vector-set! filtered-vector j (vector-ref newvec i)) 
        ; Keep going through the vector (backwards), update i and j
        (compress newvec (- i 1) (- j 1) filtered-vector)
      )
    )
  )
  ; Create newvec that contains the elements of v that satisfy the predicate pred
  ; - i: index of current index we are analyzing
  ; - count: number of elements in v that satisfy pred
  ; We go through the vector backwards
  (define (loop newvec i count) 
    (cond
      ; If i < 0, we have reached the start of the vector, so we have finished
      ((< i 0)
        ; Create a vector with as many elements as elements in v satisfy pred
        (compress newvec (- (vector-length newvec) 1) (- count 1) (make-vector count))
      )
      ; If the current element v[i] satisfies pred
      ((pred (vector-ref v i))
        ; Update the value of newvec[i] to the value of v[i]
        (vector-set! newvec i (vector-ref v i)) 
        ; Keep going through the vector
        (loop newvec (- i 1) (+ count 1))
      )
      ; If not
      (else
        ; Set the value of newvec[i] to null
        (vector-set! newvec i '()) 
        ; Keep going through the vector
        (loop newvec (- i 1) count)
      )
    )
  )

  ; Start filtering
  (loop 
    (make-vector (vector-length v))
    (- (vector-length v) 1)
    0
  )
)

;; TIMED TEST
(vector-filter (lambda (x) (> x 1)) (vector 1 2 8 9 0 1))

;; TEST
; (print (vector-filter (lambda (x) (>= x 1)) (vector 1 2)))
; ; (1 2)
; (print (vector-filter (lambda (x) (> x 1)) (vector 1 2 3 4 -1)))
; ; (2 3 4)
; (print (vector-filter (lambda (x) (>= x 1)) (vector 1 2 0)))
; ; (1 2)
