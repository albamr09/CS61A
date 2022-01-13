#lang racket
(require berkeley)

; Two lists are said to be equal if they contain equal elements arranged in the same order. For example,

; (equal? '(this is a list) '(this is a list))

; is true, but

; (equal? '(this is a list) '(this (is a) list))

; is false. To be more precise, we can define equal? recursively in terms of 
; the basic eq? equality of symbols by saying that a and b are equal? 
; if they are both symbols and the symbols are eq?, or if they are both lists 
; such that (car a) is equal? to (car b) and (cdr a) is equal? to (cdr b). 
; Using this idea, implement equal? as a procedure.

; Note: you should know by now that equal? is a built-in procedure as well. 
; This means your definition will overwrite the built-in definition.

; - a: first list
; - b: second list
(define (equal? a b)
  (define (check-elements a b)
    (cond
      ; WLOG check if a is empty, (do not check b because they are of the
      ; same length). If it is then every element is equal, return true
      ((null? a) #t)
      ; If the nth element of a equals the nth element of b, keep 
      ; going through the list
      ((equal? (car a) (car b)) (check-elements (cdr a) (cdr b)))
      ; Else if they are not equal return true
      (else #f)
    )
  )
  (cond
    ; If a and b are not lists, compare them as primitive entities
    ((and (not (pair? a)) (not (pair? b))) (eqv? a b))
    ; If they do not have the same lenght they can hardly be equal
    ((not (= (count a) (count b))) #f)
    ; Else check every element to see if the are the same
    (else (check-elements a b))
  )
)


;(equal? '(this is a list) '(this is a list))
;; #t
;
;(equal? '(this is a list) '(this (is a) list))
;; #f

