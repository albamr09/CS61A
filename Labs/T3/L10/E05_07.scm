;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E05_07
;; SICP: 3.56
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A famous problem, first raised by R. Hamming, is to enumerate, in ascending order 
; with no repetitions, all positive integers with no prime factors other than
; 2, 3, or 5. One obvious way to do this is to simply test each
; integer in turn to see whether it has any factors other than
; 2, 3, and 5. But this is very inefficient, since, as the integers
; get larger, fewer and fewer of them fit the requirement. As
; an alternative, let us call the required stream of numbers S
; and notice the following facts about it.

; • S begins with 1.
; 
; • The elements of (scale-stream S 2) are also elements of S.
; 
; • The same is true for (scale-stream S 3) and (scalestream 5 S).
; 
; • These are all the elements of S

; Now all we have to do is combine elements from these sources.
; For this we define a procedure merge that combines two ordered streams 
; into one ordered result stream, eliminating repetitions:

; Define an elementwise product between a stream 
(define (scale-stream s n) 
  (define scalar (cons-stream n scalar))
  (stream-map
    *
    scalar
    s
  )
)

(define (merge s1 s2)
  (cond 
    ; If one of the streams is empty stop merging
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    ; Else
    (else
      ; Obtain the first elements of both streams
      (let 
        (
          ; Let m = first element of s1
          (s1car (stream-car s1))
          ; Let n = first element of s2
          (s2car (stream-car s2))
        )
        (cond 
          ; If m < n
          ((< s1car s2car)
            ; Place m before n in the stream, and keep merging s1 (without m) and s2
            (cons-stream s1car (merge (stream-cdr s1) s2))
          )
          ; If m > n
          ((> s1car s2car)
            ; Place n before m in the stream, and keep merging s1 and s2 (without n)
            (cons-stream s2car (merge s1 (stream-cdr s2)))
          )
          ; Else insert one them indifferently (m in this case), discard the other and keep
          ; mergin s1 (without m) and s2 (without n)
          (else
            (cons-stream 
              s1car
              (merge 
                (stream-cdr s1)
                (stream-cdr s2)
              )
            )
          )
        )
      )
    )
  )
)

(define S 
  (cons-stream 1 
    ; Merge and filter duplicates of multiples of 2
    ; and multiples of 3 and multiples of 5
    (merge 
      ; Merge and filter duplicates of multiples of 
      ; 2 and multiples of 3
      (merge
        ; Multiples of 2
        (scale-stream S 2)
        ; Multiples of 3
        (scale-stream S 3)
      )  
      ; Multiples of 5
      (scale-stream S 5)
    )
  )
)

;; TEST

; (print (stream-ref S 0))
; ; 1
; (print (stream-ref S 1))
; ; 2
; (print (stream-ref S 2))
; ; 3
; (print (stream-ref S 3))
; ; 4
; (print (stream-ref S 4))
; ; 5
; (print (stream-ref S 5))
; ; 6
; (print (stream-ref S 6))
; 8
(print (stream-ref S 7))
; 9
