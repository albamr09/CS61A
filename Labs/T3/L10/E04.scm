;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Exercise 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The hailstone sequence is defined as follows:

;  A sequence of positive integers s1,s2,… where s1 is some positive integer and, for all n>1,

; if sn is odd, then s_(n+1) = 3·s_n + 1;
; if sn is even, then s_(n+1)= s_n÷2.

; Write a procedure num-seq that, given a positive integer n as argument, returns the 
; hailstone sequence for n. For example, (num-seq 7) should return the stream representing 
; the sequence 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1, 4, 2, 1, ...

(define (num-seq n)
  ; Create stream
  (cons-stream
    ; First value: s_n
    n
    ; Next values of the sequence: s_(n+1)
    (num-seq 
      (if (= (remainder n 2) 0)
        ; If n is even
        (/ n 2)
        ; If n is odd
        (+ 1 (* 3 n))
      )
    )
  )
)

; Write a procedure seq-length that, given a stream produced by num-seq, returns the number 
; of values that occur in the sequence up to and including the first 1. For example, (seq-length 
; (num-seq 7)) should return 17. You should assume that there is a 1 somewhere in the sequence.

(define (seq-length stream)
  (if (= (stream-car stream) 1)
    ; If we find a 1, sum one last 1 to the lenght of the stream
    1
    ; Else sum 1, because we have explored yet another element that is
    ; not a 1
    (+
      1
      ; Continue examining the next elements
      (seq-length
        (stream-cdr stream)
      )
    )
  )
)

(define s (num-seq 7))

(print (stream-ref s 0))
; 7
(print (stream-ref s 1))
; 22
(print (stream-ref s 2))
; 11
(print (stream-ref s 3))
; 34
(print (seq-length s))
; 17
