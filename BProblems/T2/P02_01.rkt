#lang racket
(require berkeley)

; Sequences as pairs

(cons 1
  (cons 2
    (cons 3
      (cons 4 nil)
    )
  )
)

; Sequences as lists

(list 1 2 3 4)


; Selectors on lists

(define one-to-four (list 1 2 3 4))

; Fisrt element of the list
 (car one-to-four)
; Sublist without the first element
 (cdr one-to-four)
 (car (cdr one-to-four))

(cons 10 one-to-four)

; List operations

(define (list-ref items n)
  ; If we have reached the index
  (if (= n 0)
    ; Get first element of item
    (car items)
    ; Else keep iterating
    (list-ref 
      ; Sublist without the first item
      (cdr items)
      ; Update n
      (- n 1)
    )
  )
)

(define squares (list 1 4 9 16 25))
(list-ref squares 3)

; Recurive method
(define (length items)
  ; Is the list empty
  (if (null? items)
    ; Return zero
    0
    ; Add one to the length
    (+ 
      1 
      ; Iterate on the sublist without
      ; the first element
      (length (cdr items))
    )
  )
)

; Iterative method
(define (length-iterative items)
  ; Keep count of the lenght
  (define (length-iter a count)
    ; If the list is empty
    (if (null? a)
      ; Return the number of items
      count
      ; Else 
      (length-iter 
        ; Update the list: delete first element
        (cdr a) 
        ; Update count: add one
        (+ 1 count)
      )
    )
  )
  ; Start iterative process
  (length-iter items 0)
)

(define odds (list 1 3 5 7))
(length odds)
(length-iterative odds)


(define (append list1 list2)
  ; If list1 empty
  (if (null? list1)
    ; The result is list2
    list2
    (cons 
      ; Create pair of first element
      ; and the rest of list1 appended to 
      ; list2
      (car list1) 
      ; Append sublist of list1 (without first element)
      ; to list2
      (append 
        (cdr list1) 
        list2
      )
    )
  )
)

(trace append)

(append squares odds)
(append odds squares)
