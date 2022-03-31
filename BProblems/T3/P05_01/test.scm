(load "./streams.scm")

(define x 1)
(define y 2)

(define s1 (cons-stream x y))
(print (stream-car s1))
; 1
(print (stream-cdr s1))
; 2

; Some operations with streams

(define (print-stream s)
  (define (loop-stream s)
    ; If the stream is not empty
    (if (not (stream-null? s))
      ; Concatenate elements of the stream recursively
      (se
        ; Obtain first element
        (safe-stream-car s)
        ; Keep obtaining rest of the elements
        (loop-stream (safe-stream-cdr s))
      )
      ; If the stream is emtpy, return empty list
      '()
    )
  )
  ; Print the result
  (print (loop-stream s))
)

; Because streams are not like lists
; and when you do cdr, you do not get a list
; if there is only one element in the cdr, you
; get the literal element
(define (safe-stream-car s)
  ; If there are more than one element
  (if (pair? s)
    ; Obtain it
    (stream-car s)
    ; Else return s, as it is the first element (and only element)
    s
  )
)

; Same with cdr
(define (safe-stream-cdr s)
  ; If there are more than one element
  (if (pair? s)
    ; Obtain the all but the first elements of the stream
    (stream-cdr s)
    ; Else return the empty stream, because the first element
    ; is the car of the stream
    the-empty-stream
  )
)

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
      (proc (safe-stream-car s))
      ; Keep applyin proc to every element
      (stream-map proc (safe-stream-cdr s))
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
      (proc (safe-stream-car s))
      ; Keep iterating through the stream
      (stream-for-each proc (safe-stream-cdr s))
    )
  )
)

; Another way to print

(define (display-line x) (newline) (display x))
(define (display-stream s)
  (stream-for-each display-line s)
)

(print (stream-ref s1 0))
; 1
(print (stream-ref s1 1))
; 2

; ; Multiply the elements of the stream by 2
; (stream-for-each (lambda (x) (* 2 x)) s1)
; (print-stream s1)
; ; (2 4)
; 
; (print-stream
;   ; Multiply the elements of the stream by 10
;   (stream-map (lambda (x) (* 10 x)) s1)
; )
; ; (20 40)
; 
; (print-stream s1)
; ; (2 4)
; 
; (display-stream s1)
; 
