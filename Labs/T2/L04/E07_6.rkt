#lang racket
(require berkeley)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The procedures +, *, and list take arbitrary numbers of arguments. One way to 
; define such procedures is to use define with dotted-tail notation. In a procedure 
; definition, a parameter list that has a dot before the last parameter name indicates 
; that, when the procedure is called, the initial parameters (if any) will have as values 
; the initial arguments, as usual, but the final parameter's value will be a list of any 
; remaining arguments. For instance, given the definition

;;; (define (f x y . z) <body>)

; the procedure f can be called with two or more arguments. If we evaluate

; -> (f 1 2 3 4 5 6)

; then in the body of f, x will be 1, y will be 2, and z will be the list '(3 4 5 6). Given the definition

;;; (define (g . w) <body>)

; the procedure g can be called with zero or more arguments. If we evaluate

; -> (g 1 2 3 4 5 6)

; then in the body of g, w will be the list '(1 2 3 4 5 6).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Use this notation to write a procedure same-parity that takes one or more integers and returns a list of 
; all the arguments that have the same even-odd parity as the first argument. For example,


; Returns true if x and y have the
; same parity

(define (same-parity? x y) 
  ; If they have the same
  ; remainder when / 2, then 
  ; they have the same parity
  (= 
    (remainder x 2)
    (remainder y 2)
  )
)

; - s: sequence of numbers

(define (same-parity . s)
  (define (same-parity-helper s first lst)
    (cond 
      ((null? s) lst)
      ; If they have the same parity: both 
      ; odd or both even
      ((same-parity? first (car s)) 
        ;Continue going though the list
        (same-parity-helper 
          ; Delete the current element
          (cdr s) 
          ; Save the first element of list
          first 
          ; Update same parity list
          (append lst (list (car s)))
        )
      )
      (else 
        ; If not just delete the current
        ; element from s
        (same-parity-helper 
          (cdr s) 
          first 
          lst
        )
      )
    )
  )
  (same-parity-helper s (car s) '())
)

(same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)

(same-parity 2 3 4 5 6 7)
; (2 4 6)

(define (same-parity-rec . s)
  (define (same-parity-rec-helper n)
    (cond
      ; If there is only one element left
      ; that element is the first one
      ((= (count n) 1) '())
      ; If the current element has the same parity 
      ; as the first
      ((same-parity? (car n) (cadr n))
       (append 
         ; Append the element with the same parity
         (list (cadr n))
         ; Keep searching
         (same-parity-rec-helper 
          ; Update s
          (cons 
            ; First element
            (car n) 
            ; All but second and first element
            (cddr n)
          )
         )
        )
      )
      ; Else, do not append current element
      (else
       (same-parity-rec-helper
          ; Remove second element
          (cons 
            ; First element
            (car n) 
            ; All but first and second
            (cddr n)
          )
        )
      )
    )
  )
  ; Append first element to result
  (cons (car s) (same-parity-rec-helper s))
)

(same-parity-rec 1 2 3 4 5 6 7)
; (1 3 5 7)

(same-parity-rec 2 3 4 5 6 7)
; (2 4 6)
