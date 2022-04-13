;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E05_08
;; SICP: 3.64
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Write a procedure stream-limit that takes
; as arguments a stream and a number (the tolerance). It should
; examine the stream until it finds two successive elements
; that differ in absolute value by less than the tolerance, and
; return the second of the two elements. Using this, we could
; compute square roots up to a given tolerance.

(define (stream-limit stream tolerance)
  (cond
    ; Check if the first or the second element is null
    ; If so, then no pair of elements satisfied the tolerance
    ; so return empty stream
    ((stream-null? stream) the-empty-stream)
    ((stream-null? (stream-cdr stream)) the-empty-stream)
    ; Else obtain difference
    (else
      (let
        ((delta 
          ; Obtain absolute value of the difference of the two first values
          ; in the stream
          (abs 
            (- 
              (stream-car stream) 
              (stream-car (stream-cdr stream))
            )
          )
        ))
        ; Check if delta satisfies the tolerance
        (if (<= delta tolerance)
          ; Return the second value
          (stream-car (stream-cdr stream))
          ; Else compute the different on the next elements
          ; of the stream
          (stream-limit
            (stream-cdr stream)
            tolerance
          )
        )
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQRT STREAM DEFINITION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (average x y)
  (/ 
    (+ x y)
    2
  )
)

(define (sqrt-improve guess x)
  (average guess (/ x guess))
)


(define (sqrt-stream x)
  (define guesses
    (cons-stream
      ; Starting guess
      1.0
      (stream-map 
        ; Apply sqrt-improve
        (lambda (guess) (sqrt-improve guess x))
        ; From the first guess
        guesses
      )
    )
  )
  guesses
)


;;;;;;;;;;;;;;;;;;;;;;;
;;; TEST

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance)
)

(print (sqrt 2 0.1))
; 1.41666666666667
(print (sqrt 2 0.01))
; 1.41421568627451
