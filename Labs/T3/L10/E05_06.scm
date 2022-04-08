;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E05_06
;; SICP: 3.55
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Define a procedure partial-sums that takes as argument a stream S 
; and returns the stream whose elements are S0, S0+S1, S0+S1+S2; ....
; For example, (partialsums integers) should be the stream 1, 3, 6, 10, 15, ....

; Given the procedure add streams
(define (add-streams s1 s2) (stream-map + s1 s2))

(define (partial-sums stream)
  (define (partial-sums-helper original summation)
    (cons-stream
      ; Obtain the fist element of the summation
      ; restul from summing Sn (from the orginal)
      ; with S1 + S2 + ... + S(n-1) (from summation)
      (stream-car summation)
      ; Every time we call (cdr stream) we call this second part, which 
      ; evaluates (add-streams), this will sum one element per iteration, these will be
      ; - the last sum made in summation: S0 + S1 + ... + S(n-1)
      ; - the last corresponding element in the orginal: Sn
      (partial-sums-helper
        ; Keep a copy of the orginal
        original
        (add-streams
          original
          (stream-cdr summation)
        )
      )
    )
  )
  ; Start the summation with original = stream 
  ; and modified = stream
  (partial-sums-helper stream stream)
)


;; TEST

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers))
)

(define p (partial-sums integers))
(print (stream-ref p 0))
; 1
(print (stream-ref p 1))
; 3
(print (stream-ref p 2))
; 6
(print (stream-ref p 3))
; 10
(print (stream-ref p 4))
; 15
