;; VECTOR PROGRAMMING

; Map version with vectors
(define (vector-map fn v) 
  (define (loop newvec i) 
    ; Loop through vector and make changes
    (if (< i 0) 
      ; When the index is less than 0, we have finished
      ; return modified vector
      newvec 
      ; Else
      (begin 
        ; Save value of applying fn on newvec[i]
        (vector-set! 
          newvec 
          i 
          ; Apply function fn on v[i]
          (fn (vector-ref v i))
        ) 
        ; Continue looping and update index to index - 1
        (loop newvec (- i 1))
      )
    )
  )
  ; Start looping
  (loop 
    ; New vector where modified values are stored
    (make-vector (vector-length v))
    ; Starting index
    (- (vector-length v) 1)
  )
)

(define (vector-addup v)
  (define (loop i)
    ; If the index is less than zero, we have gone
    ; through all the vector
    (if (< i 0)
      ; Return zero to stop summing recursively
      0
      ; Else continue summing recursively
      (+
        ; Sum the current value of v[i]
        (vector-ref v i)
        ; To rest of values in the vector (updating index to index - 1)
        (loop (- i 1))
      )
    )
  )
  ; Start recursive proccess to index to (len v) - 1
  ; because index of vector start in 0
  (loop 
    (- (vector-length v) 1)
  )
)

(vector-addup (vector 1 2 3))
; 6


;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLE: SHUFFLING
;;;;;;;;;;;;;;;;;;;;;

; List representation of sequence: time complexity is O(n^2) (Theta, not Big-O)
; - O(n) to go through all of the elements of a vector of lenght n
; And for each element:
;   - O(n) to find element of the given index
; So the time complexity yields O(n^2)

(define (list-shuffle! lst) 
  (if (null? lst) 
    '() 
    (let 
      ; Create random index within the list range
      ((index (random (length lst)))) 
      (let 
        (
          (pair ((repeated cdr index) lst)) 
          ; Save first element of the list
          (temp (car lst))
        )
        ; Update the value of the first element in the 
        ; list, to the one stored in the random index
        (set-car! lst (car pair)) 
        ; Store the value of the first element of the list (tmp)
        ; into the i-th (randomly chosen) element of the list
        (set-car! pair temp) 
        ; Keep shuffling the rest of the list
        (list-shuffle! (cdr lst)) 
        ; Return shuffled list
        lst
      )
    )
  )
)

; Vector representation of sequence: temporal complexity is O(n)
; - O(1) to find element of the given index
; - O(n) to go through all of the elements of a vector of lenght n

(define (vector-shuffle! vec) 
  (define (loop n) 
    ; If the index is equal to 0, we have gone through all the vector
    (if (= n 0) 
      ; So return the shuffled vector
      vec 
      (let 
        (
          ; Create the random index
          (index (random n)) 
          ; Save the value of the element in the (n-1)-th position
          (temp (vector-ref vec (- n 1)))
        ) 
        ; Update the (n-1)-th element with the index-th value
        (vector-set! vec (- n 1) (vector-ref vec index)) 
        ; Update the index-th element with the (n-1)-th value
        (vector-set! vec index temp) 
        ; Continue going through the vector
        (loop (- n 1)) 
      )
    )
  )
  ; Start looping with index = length of vector
  (loop (vector-length vec))
)