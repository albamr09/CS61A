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

(trace stream-cdr)
