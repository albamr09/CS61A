; Write an analogous procedure called product that returns the product of the values of a
; function at points over a given range. 

; (define (sum term a next b)
;   (define (iter a result)
;     (if ⟨??⟩
;       ⟨??⟩
;       (iter ⟨??⟩ ⟨??⟩)
;     )
;   )
;   (iter ⟨??⟩ ⟨??⟩)
; )

; Function to increment on one
(define (inc n) (+ n 1))

; Product of integers between a and b
; term: function to apply over a 
; a: lower bound
; next: how to get next element in sequence
; b: upper bound

(define (product term a next b)
  ; Iterative procedure
  (define (iter a result)
    ; If the lower limit > upper limit
    (if (> a b)
      ; Return result
      result
      ; Else continue
      ; Obtain the next value by applying the next function on a
      ; Update result by applying the term function on a 
      (iter (next a) (* result (term a)))
    )
  )
  ; First call to the iterative procedure with 0 as the starting result
  (iter a 1)
)

(product inc 1 inc 5) 

