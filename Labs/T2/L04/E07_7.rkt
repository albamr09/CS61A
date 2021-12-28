#lang racket
(require berkeley)

;Louis Reasoner tries to rewrite the first square-list procedure of Exercise 2.21 so that it evolves an iterative process:

(define (square-list items)
  ; - things: remainig list
  ; - answer: list of squares
  (define (iter things answer)
    ; If there are no elements left
    (if (null? things)
        ; return the list
        answer
        (iter 
          ; Update remaining by removing the first
          (cdr things) 
          ; Append the square of the current element to
          ; the start of the list of squares
          ; THIS is why it is reversed, it should be appended
          ; to the end of the list
          (cons 
            (square (car things))
            answer
          )
        )
    )
  )
  (iter items nil)
)

(square-list (list 1 2 3))

; Unfortunately, defining square-list this way produces the answer list in the reverse order of the one desired. Why?
; Louis then tries to fix his bug by interchanging the arguments to cons:

(define (square-list-rev items)
  (define (iter things answer)
    (if (null? things)
        ; return the list
        answer
        (iter 
          (cdr things)
          ; Append the aswer to the end of 
          ; the list
          (cons 
            ; THIS is the problem the first element is a pair
            ; for example: on the first iteration 
            ; answer = '() and (square(car(things))) = 1
            ; so if you append them you get the pair ('(), 1)
            ; when answer = ('(), 1), and the current square is 4,
            ; if you append them you get the pair (('(), 1), 4)
            answer
            (square (car things))
          )
        )
    )
  )
  (iter items nil)
)

; This doesn't work either. Explain.


(square-list-rev (list 1 2 3))
