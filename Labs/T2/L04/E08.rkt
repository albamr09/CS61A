#lang racket
(require berkeley)

; Write a procedure my-substitute that takes three arguments: a list, an old word
; and a new word. It should return a copy of the list, but with every occurrence 
; of the old word replaced by the new word, even in sublists.

; You might find the procedure list? useful:

(define (my-substitute lst replacee replacer)
  (cond 
    ; If there are no element, stop going through the list
    ; terminate recursive process
    ((null? lst) '())
    ; If it is a list
    ((list? (car lst))
     (cons
      ; Go thourgh the sublist replacing if necessary
      (my-substitute (car lst) replacee replacer)
      ; Keep going though the main list, and update
      ; the main list by not selecting the first element
      (my-substitute (cdr lst) replacee replacer)
     )
    )
    ; If it is an element 
    (else 
      (if (equal? (car lst) replacee)
        ; If current element = replacee
        (cons
          ; Replace by replacer
          replacer
          ; Update lst by not selecting the first element
          (my-substitute (cdr lst) replacee replacer)
        )
        ; Else keep it
        (cons
          ; Do not replace by replacer
          (car lst)
          ; Update lst by not selecting the first element
          (my-substitute (cdr lst) replacee replacer)
        )
      )
    )
  )
)

(my-substitute 
  '((lead guitar) (bass guitar) (rhythm guitar) drums)
  'guitar
  'axe
)
; ((lead axe) (bass axe) (rhythm axe) drums)

(my-substitute 
  '((lead (drums (bar guitar))) (bass guitar) (rhythm guitar) drums)
  'guitar
  'axe
)
; ((lead (drums (bar axe))) (bass axe) (rhythm axe) drums)
