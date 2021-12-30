#lang racket
(require berkeley)

; Now write my-substitute2 that takes a list, a list of old words, and a list of new words; 
; the last two lists should be the same length. It should return a copy of the first argument
; but with each word that occurs in the second argument replaced by the corresponding word 
; of the third argument:

; Get index of element x if it is on the list replacee, else
; return -1

(define (equal-replacee? x replacee)
   (define (equal-replacee-helper x replacee n)
     (cond
       ; If it was not on the list and the list is empty 
       ; return -1
       ((null? replacee) -1)
       ; If it is on the list return the current index n
       ((equal? x (car replacee)) n)
       ; If the x != current element of list
       ; update list and update index n
       (else (equal-replacee-helper x (cdr replacee) (+ 1 n)))
     )
   )
   (equal-replacee-helper x replacee 0)
)

(define (my-substitute2 lst replacee replacer)
  (cond 
    ; If there are no element, stop going through the list
    ; terminate recursive process
    ((null? lst) '())
    ; If it is a list
    ((list? (car lst))
     (cons
      ; Go thourgh the sublist replacing if necessary
      (my-substitute2 (car lst) replacee replacer)
      ; Keep going though the main list, and update
      ; the main list by not selecting the first element
      (my-substitute2 (cdr lst) replacee replacer)
     )
    )
    ; If it is an element 
    (else 
      (let
        ; get the index in the list where the element 
        ; of lst = some replacee element
        ((index (equal-replacee? (car lst) replacee)))
        ; If it does not return -1, then the current element
        ; of lst is on the replacee list
        (if (>= index 0)
          ; If current element = some replacee
          (cons
            ; Replace by replacer, select by index
            (list-ref replacer index)
            ; Update lst by not selecting the first element
            (my-substitute2 (cdr lst) replacee replacer)
          )
          ; Else keep it
          (cons
            ; Do not replace by replacer
            (car lst)
            ; Update lst by not selecting the first element
            (my-substitute2 (cdr lst) replacee replacer)
          )
        )
      )
    )
  )
)

(my-substitute2 
  '((4 calling birds) (3 french hens) (2 turtle doves))
  '(1 2 3 4)
  '(one two three four)
)

; ((four calling birds) (three french hens) (two turtle doves))
