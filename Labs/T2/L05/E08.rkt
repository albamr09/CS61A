#lang racket
(require berkeley)

; We can represent a set as a list of distinct elements, and we can 
; represent the set of all subsets of the set as a list of lists. 
; For example, if the set is (1 2 3), then the set of all subsets is 
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following 
; definition of a procedure that generates the set of subsets of a set 
; and give a clear explanation of why it works:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; EXAMPLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given a set S = {1, 2, 3, 4}
; For n = 1
; > (subsets (list 1 2 3 4))
; Obtain all of the subsets of of S - n
; >> rest = (subsets '(2 3 4))
; For n = 2, S = {2, 3, 4}
; Obtain all of the subsets of of S - n
; >>> rest = (subsets '(3 4))
; For n = 3, S = {3, 4}
; Obtain all of the subsets of of S - n
; >>>> rest = (subsets '(4))
; For n = 4, S = {4}
; Obtain all of the subsets of of S - n
; >>>>> rest = (subsets '())
; <<<<< return '(), because the list is empty
; Apppend 4 to each of the subsets of S - n = {'()}, and concatenate
; with the current set of subsets P = {'()}
; <<<< return ('() (4))
; Apppend 3 to each of the subsets of S - n = {'(), (4)}, and concatenate
; with the current set of subsets P = {'(), (4)}
; <<<< return ('() (4) (3) (3 4))
; Apppend 2 to each of the subsets of S - n = {'(), (4), (3), (3 4)}, and concatenate
; with the current set of subsets P = {'(), (4), (3), (3 4)}
; <<<< return ('() (4) (3) (3 4) (2) (2 4) (2 3) (2 3 4))
; Apppend 1 to the each of subsets of S - n = {'(), (4), (3), (3 4), (2), (2 4), (2 3), (2 3 4)}, and concatenate
; with the current set of subsets P = {'(), (4), (3), (3 4), (2), (2 4), (2 3), (2 3 4)}
; <<<< return ('() (4) (3) (3 4) (2) (2 4) (2 3) (2 3 4), (1), (1 4), (1 3), (1 3 4), (1 2), (1 2 4), (1 2 3), (1 2 3 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (subsets s)
  (if (null? s)
    (list nil)
    (let 
      ; Rest is the set that contains all
      ; of the subsets of s - (firs element of s)
      ((rest (subsets (cdr s))))
      (append 
        rest 
        (map 
          (lambda
            (x)
            ; Append the first element of the 
            ; sequence to every element of rest
            (append
              (list (car s))
              x 
            )
          )
          rest
        )
      )
    )
  )
)

(trace subsets)
(subsets (list 1 2 3))
; (() (3) (2) (2 3) (1) (1 2) (1 3) (1 2 3))
