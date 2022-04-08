;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E05_05
;; SICP: 3.54
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define a procedure mul-streams, analogous
; to add-streams, that produces the elementwise product of
; its two input streams. Use this together with the stream of
; integers to complete the following definition of the stream
; whose nth element (counting from 0) is n + 1 factorial:



(define (mul-streams s1 s2)
  (stream-map
    *
    s1
    s2
  )
)

; Create stream of integers greater than 2
(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 2 (add-streams ones integers))
)

; Refer to ./E05_04.scm for an explanation of how streams work
; in an example in detail
(define factorials
  (cons-stream 
    ; If n = 1, n! = 1
    1 
    ; For every integer n, with n > 1, suppose n = 2
    ; 1. We evaluate this stream-map where we call
    ; 2. (* (stream-car integer) (stream-car factorials))
    ; 3. Because it is the first iteration, 
    ;   - (stream-car factorials) = 1
    ;   - (stream-car integer) = 2
    ; 4. So we create a stream with 
    ;   - stream-car = 2
    ;   - stream-cdr = (mul-streams 
    ;                     (stream-cdr integers) 
    ;                     (stream-cdr factorials)
    ;                   )
    ; Which is a delayed object
    ; 5. When we try to do (cdr (cdr factorials)), that is, 
    ;    when n = 3, we will evaluate this delayed object, and so call 
    ;     (mul-streams 
    ;       (2, (delayed ...))
    ;       (3, (delayed ...)) 
    ;     )
    ; Which will create a new entry in the factorials stream equal to 2*3 = 6 = 3!
    (mul-streams  
      factorials
      integers
    )
  )
)

;;TEST

(define s1 
  (cons-stream
    1
    (cons-stream
      4
      the-empty-stream
    )
  )
)

(define s2
  (cons-stream
    2
    (cons-stream
      3
      the-empty-stream
    )
  )
)

(define prod (mul-streams s1 s2))
(print (stream-ref prod 0))
; 2
(print (stream-ref prod 1))
; 12

(print (stream-ref factorials 0))
; n! = 1! = 1
(print (stream-ref factorials 1))
; n! = 2! = 2
(print (stream-ref factorials 2))
; n! = 3! = 6
(print (stream-ref factorials 3))
; n! = 4! = 24
(print (stream-ref factorials 4))
; n! = 5! = 120
