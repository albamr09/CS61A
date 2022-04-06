;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STREAMS ADT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTRUCTOR
;;;;;;;;;;;;;;;;;;;;;;;

(define (cons-stream a b)
  (cons
    a
    ; Promise to evaluate b
    (delay b)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;
;; SELECTORS
;;;;;;;;;;;;;;;;;;;;;;;

(define (stream-car stream) (car stream))
(define (stream-cdr stream) 
  ; Evaluate b: make the promise return the value
  (force (cdr stream))
)

;;;;;;;;;;;;;;;;;;;;;;;
;; OPERATORS
;;;;;;;;;;;;;;;;;;;;;;;

(define (stream-ref s n)
  ; If we have reached the index, n = 0
  (if (= n 0)
    ; Return the first element
    (stream-car s)
    ; Else keep advancing on the stream
    (stream-ref (stream-cdr s) (- n 1))
  )
)

(define (stream-map proc s)
  ; If the stream is null
  (if (stream-null? s)
    ; Object that represents an empty stream
    the-empty-stream
    ; Construct a new stream with the result of the
    ; transformation of each object
    (cons-stream 
      ; Apply procedure to the first element in the stream
      (proc (stream-car s))
      ; Keep applyin proc to every element
      (stream-map proc (stream-cdr s))
    )
  )
)

(define (stream-for-each proc s)
  ; If the stream is empty we finish iterating
  (if (stream-null? s)
    'done
    ; Else
    (begin 
      ; Apply the procedure to the first element in the stream
      (proc (stream-car s))
      ; Keep iterating through the stream
      (stream-for-each proc (stream-cdr s))
    )
  )
)

(define (stream-filter pred stream)
  (cond 
    ; If there are no more elements in the stream: return the empty stream
    ((stream-null? stream) the-empty-stream)
    ; If the predicate is satisfied by the first element
    ((pred (stream-car stream))
      ; Create stream formed by this first element
      (cons-stream 
        (stream-car stream)
        ; Continue filtering the rest of the elements
        (stream-filter 
          pred
          (stream-cdr stream)
        )
      )
    )
    ; Otherwise keep filtering the rest of the elements in the stream
    (else (stream-filter pred (stream-cdr stream)))
  )
)

; Print

(define (display-line x) (newline) (display x))

(define (display-stream s)
  (stream-for-each display-line s)
)

; Implementation of delay and force

; delay must package an expression so that it can be evaluated later on demand, 
; and we can accomplish this simply by treating the expression as the body of a procedure
(define (delay exp)
  (lambda
    ()
    exp
  )
)

; force simply calls the procedure (of no arguments) produced by delay, 
; so we can implement force as a procedure:
(define (force delayed-object)
  (delayed-object)
)

; In many applications, we end up forcing the same delayed object many times. This can lead to serious 
; inefficiency in recursive programs involving streams. The solution is to build delayed objects so that 
; the first time they are forced, they store the value that is computed. Subsequent forcings will simply 
; return the stored value without repeating the computation. In other words, we implement delay as a special-purpose 
; memoized procedure.

; Memoization procedure
(define (memo-proc proc)
  (let 
    ; Initialize control values
    (
      ; The first time it is called the procedure was 
      ; never run
      (already-run? false) 
      ; So the result is non-existent, i.e. false
      (result false)
    )
    (lambda 
      ()
      ; If it was never computed
      (if (not already-run?)
        (begin 
          ; Obtain the value and store it in result
          (set! result (proc))
          ; Set control variable to true
          (set! already-run? true)
          result
        )
        ; If it was computed, simply return the result stored
        result
      )
    )
  )
)

; Implementation of delay with memoization
(define (delay e)
  (memo-proc
    (lambda
      ()
      e
    )
  )
)

; And force remains unchanged
