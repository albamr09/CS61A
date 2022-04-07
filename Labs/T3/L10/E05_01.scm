;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Exercise 5
;;3.50
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Complete the following definition, which
; generalizes stream-map to allow procedures that take multiple arguments

(define (stream-map proc . argstreams)
  ; If there are no more elements in the first stream
  (if (empty? (car argstreams))
    ; Return the empty stream
    the-empty-stream
    ; Else
    (cons-stream
      ; Apply the procedure "proc" to all the first elements of each stream in
      ; argstreams
      (apply proc (map stream-car argstreams))
      ; Continue applying to all the rest of the elements of each stream 
      ; in argstreams
      (apply 
        stream-map
        (cons proc (map stream-cdr argstreams))
      )
    )
  )
)

(define sum 
  (stream-map 
    + 
    ; s1
    (cons-stream
      1
      (cons-stream
        2
        the-empty-stream
      )
    )
    ; s2
    (cons-stream
      1
      (cons-stream
        2
        the-empty-stream
      )
    )
  )
)

(print (stream-ref sum 0))
; 2
(print (stream-ref sum 1))
; 4
