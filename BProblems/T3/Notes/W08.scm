; Load OOP library
(load "../../../lib/obj.scm")

; Create a local state variable with let and lambda

(define count
  (let 
    ; Local state variable, is only binded once
    ; when count is defined
    ((result 0))
    (lambda 
      ; Formal parameters
      ()
      ; Update the variable
      (set! result (+ result 1))
      ; Return the variable
      result
    )
  )
)

(count)
; 1
(count)
; 2
(count)
; 3

; Create a procedure to create a new counter

(define (make-count) 
  (let 
    ; Local state variable
    ((result 0)) 
    (lambda 
      ; Formal parameters
      () 
      ; Update the variable
      (set! result (+ result 1)) 
      ; Return the variable
      result
    )
  )
)

(define counter-1 (make-count))
(define counter-2 (make-count))

(counter-1)
; 1
(counter-1)
; 2
(counter-2)
; 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CLASS AND INSTANCE VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-count
  ; Class environment
  (let 
    ; Class variable
    ((glob 0))
    (lambda 
      ()
      ; Instance environment
      (let 
        ; Instance variable
        ((loc 0))
        (lambda 
          ()
          (set! loc (+ loc 1))
          (set! glob (+ glob 1))
          (list loc glob)
        )
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CLASS AND INSTANCE VARIABLES WITH MESSAGE PASSING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-count
  ; Class environment
  (let 
    ; Class variable
    ((glob 0))
    (lambda 
      ()
      ; Instance environment
      (let 
        ; Local variable
        ((loc 0))
        (lambda 
          ; Message passing
          (msg)
          ; Dispathing
          (cond 
            ; Update local variable
            ((eq? msg 'local)
              (lambda 
                ()
                (set! loc (+ loc 1))
                loc
              )
            )
            ; Update global variable
            ((eq? msg 'global)
              (lambda 
                ()
                (set! glob (+ glob 1))
                glob
              )
            )
            (else (error "No such method" msg)) 
          )
        )
      )
    )
  )
)

(define c1 (make-count))
(define c2 (make-count))

;;;;;;;;;;;;;
; How the different environmnets are created
; c1 -> first lambda (global env)
c1
; (c1 'local) -> second lambda, this returns a procedure (instance env)
(c1 'local)
; ((c1 'local)) -> third lambda, evaluate the procedure
;;;;;;;;;;;;;

; The local variables are independent
((c1 'local))
; 1
((c1 'local))
; 2
((c1 'local))
; 3
((c2 'local))
; 1
; The global variables are shared
((c1 'global))
; 1
((c2 'global))
; 2
