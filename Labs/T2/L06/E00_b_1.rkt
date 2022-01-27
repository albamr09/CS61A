#lang racket
(require berkeley)

; Same as "./E00.rkt" but with less abstraction

; Try inventing higher-order procedures; since you don't have define 
; you'll have to use the Y-combinator trick

;;;;;;;;;;;;;;;
;;; FILTER
;;;;;;;;;;;;;;;

(
  ; Filter definition
  (lambda 
    ; Parameters
    ; f: procedure to filter each argument
    ; n: list to apply filter (list of arguments)
    (f n) 
    ; Body
    (
      ; Anonymous function
      (lambda 
        (my-filter) 
        (my-filter 
          my-filter f n
        )
      ) 
      ; my-filter definition: argument to
      ; the previous anonymous function
      ; (my-filter 
      ;  itself
      ;  f
      ;  n
      ; )
      (lambda 
        ; Formal parameters
        ; my-filter: 
        ; f: procedure to filter with
        ; n: list of arguments to filter
        (my-filter f n) 
        (if (null? n) 
          '() 
          ; If the first element of n satisfies f
          (if (f (car n))
            ; Concatenate the first element, with the
            ; rest of the filter
            (cons 
              (car n) 
              ; this function comes from this 
              ; very anonymous function
              (my-filter my-filter f (cdr n))
            )
            ; Else keep filtering the of the elements
            (my-filter my-filter f (cdr n))
          )
        )
      )
    )
  ) 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  number?                     ; the argument f for filter
  '(0 1 bird)                 ; the argument n for filter
)

;;;;;;;;
;;; MAP
;;;;;;;;

(
  ; Map definition
  (lambda 
    ; Parameters
    ; f: procedure to apply to each argument
    ; n: list to apply map (list of arguments)
    (f n) 
    ; Body
    (
      (lambda 
        (map) 
        (map map f n)
      ) 
      (lambda 
        (map f n) 
        (if (null? n) 
          '() 
          (cons 
            (f (car n)) 
            (map map f (cdr n))
          )
        )
      )
    )
  ) 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  first                     ; the argument f for map
  '(the rain in spain)      ; the argument n for map
)

; (t r i s)

;;;;;;;;;;;;;;;
;;; ACCUMULATE
;;;;;;;;;;;;;;;

(
  ; Map definition
  (lambda 
    ; Parameters
    ; f: procedure to apply to each argument
    ; n: list to apply map (list of arguments)
    (f n) 
    ; Body
    (
      (lambda 
        (accum) 
        (accum accum f n)
      ) 
      (lambda 
        (accum f n) 
        (if (null? n) 
          0 
          (+ 
            (f (car n)) 
            (accum accum f (cdr n))
          )
        )
      )
    )
  ) 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; f: indentity function
  (lambda (x) x)
  ; n
  '(1 2 3 4)                     
)

; 10
