#lang racket
(require berkeley)

; Load Huffmann Encoded Trees ADT
(require "../../../BProblems/T2/P03_04.rkt")

; The encode procedure takes as arguments a message and a tree and produces 
; the list of bits that gives the encoded message

; This algorithm is in "../../../BProblems/T2/P03_04.rkt"

; encode-symbol is a procedure, which you must write, that returns the 
; list of bits that encodes a given symbol according to a given tree. You 
; should design encode-symbol so that it signals an error if the symbol is 
; not in the tree at all

(define (encode message tree)
  (define (encode-symbol character current-branch)
    ; If it is a leaf, we have finished encoding this character
    (if (leaf? current-branch)
      '()
      ; If the current character is on the list of symbols within the left branch
      (if (member? character (symbols (left-branch current-branch)))
        ; Append to 0 the encoded string of bits, and keep encoding through the left branch
        (append (list 0) (encode-symbol character (left-branch current-branch)))
        ; Append to 1 the encoded string of bits, and keep encoding through the right branch
        (append (list 1) (encode-symbol character (right-branch current-branch)))
      )
    )
  )
  (if (null? message)
    '()
    ; Append the result of encoding the first character of the string
    ; with the result of encoding the rest of the characters
    (append 
      (encode-symbol 
        (car message) 
        tree
      )
      (encode (cdr message) tree)
    )
  )
)


;;;;;;;;;;;;;;;
;;;; TEST
;;;;;;;;;;;;;;;

; (define sample-tree
;   (make-code-tree 
;     (make-leaf 'A 4)
;     (make-code-tree
;       (make-leaf 'B 2)
;       (make-code-tree
;         (make-leaf 'D 1)
;         (make-leaf 'C 1)
;       )
;     )
;   )
; )
; 
; (encode '(A D A B B C A) sample-tree)
; ; (0 1 1 0 0 1 0 1 0 1 1 1 0)
