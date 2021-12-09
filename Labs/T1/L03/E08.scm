; Define a procedure cont-frac such that evaluating (cont-frac n d k)
; computes the value of the k-term finite continued fraction. 
; Where n and d are procedures of one argument 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recursive process
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cont-frac-rec-helper n d k i)
  ; If counter exceeds the upper limit of sequence
  (if (> i k) 
    ; Return 0.0 to sum
    0.0
    ; Else 
    (/ 
      ; Obtain the k-term finite continued fraction recursively
      ; i.e. for k = 2
      ; 1. (N_1 / 
      ;         (D_1 + (cont-frac-rec-helper n d k 2) )
      ; ), so i = 2
      ; 2. (N_1 / 
      ;         (D_1 + 
      ;           (N_2 / 
      ;               (D_2 + (cont-frac-rec-helper n d k 3))
      ;           )
      ;         )
      ; ), so i = 3
      ; 3. (N_1 / 
      ;         (D_1 + 
      ;           (N_2 / 
      ;               (D_2 + 0.0)
      ;           )
      ;         )
      ; )
      (n i) 
      (+ 
        (d i)
        (cont-frac-rec-helper n d k (+ i 1))
      )
    )
  )
)

(define (cont-frac-rec n d k)
  (cont-frac-rec-helper n d k 1)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Iterative process
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cont-frac-iter-helper n d k accumulated)
  ; If all k terms have been summed
  (if (= k 0)
    ; Return the accumulated variable
    accumulated
    (cont-frac-iter-helper
      n
      d
      ; Update counter
      (- k 1)
      ; Update accumulated
      ; i.e. for k = 2
      ; 1. Set accumulated = N_2 / (D_2 + accumulated), where accumulated = 0.0 set k = 1
      ; 2. Set accumulated = N_1 / (D_1 + accumulated), set k = 0
      ; 3. Return accumulated 
      (/ 
        (n k)
        (+
          (d k)
          accumulated
        ) 
      )
    )
  )
)

(define (cont-frac-iter n d k)
  (cont-frac-iter-helper n d k 0.0)
)

; Approximate inverse of golden ratio
(cont-frac-rec 
  (lambda (i) 1.0)
  (lambda (i) 1.0)
  25
)
; .6180339887498951

(cont-frac-iter 
  (lambda (i) 1.0)
  (lambda (i) 1.0)
  25
)
; .6180339887498951
