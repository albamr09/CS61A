#lang racket
(require berkeley)

; Try inventing higher-order procedures; since you don't have define 
; you'll have to use the Y-combinator trick, like this:

(
  ; Define high order functions over lists
  (lambda 
    ; Parameters
    ; logic: what to do for every iteration
    ;         on the list (i.e. filter, map, accumulate)
    ; f: procedure to apply to each argument
    ; n: list to apply the high order procedure
    (logic f n) 
    ; Body
    (
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ; Anonymous function: first call
      ; of the first order function
      (lambda 
        (hof) 
        (hof 
          hof f n
        )
      ) 
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ; hof definition: argument to
      ; the previous anonymous function
      ; (hof 
      ;  itself
      ;  f
      ;  n
      ; )
      (lambda 
        ; Formal parameters
        ; hof: itself (this anonymous function)
        ; f: procedure to apply to each argument
        ; n: list to apply the high-order procedure on
        (hof f n) 
        ; Apply the logic of the high-order procedure
        (logic 
          hof 
          f 
          n
        )
      )
    )
  ) 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; logic: MAP
  ;(lambda
  ;  (hof f n)
  ;  (if (null? n)
  ;    '()
  ;    (cons 
  ;      (f (car n)) 
  ;      (hof hof f (cdr n))
  ;    )
  ;  )
  ;)
  ;; f
  ;first                                ; the argument f for filter
  ;; n
  ;'(the rain in spain)                 ; the argument n for filter

  ;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; logic: FILTER
  ;(lambda
  ;  (hof f n)
  ;  (if (null? n)
  ;    '()
  ;    ; If the first element of n satisfies f
  ;    (if (f (car n))
  ;      ; Append the elemnt to the final filtered list
  ;      (cons 
  ;        (car n) 
  ;        ; Keep filtering
  ;        (hof hof f (cdr n))
  ;      )
  ;      ; Else keep filtering the of the elements
  ;      (hof hof f (cdr n))
  ;    )
  ;  )
  ;)
  ;number?                     ; the argument f for filter
  ;'(0 1 bird)                 ; the argument n for filter

  ;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; logic: ACCUMULATE
  (lambda
    (hof f n)
    (if (null? n)
      0
      ; If the first element of n satisfies f
      (+ 
        (car n)
        (hof hof f (cdr n))
      )
    )
  )
  ; f: identity function
  (lambda (x) x)                   
  ; n
  '(0 1 2)                 
)

