#lang racket
; Naming problems with make-tree
; (require berkeley)

; Load binary tree methods
(require "../../../BProblems/T2/P03_03.rkt")

; Using adjoin-set, construct the trees:

(define (make-adjoint-tree set tree)
  (if (null? set)
    tree
    (make-adjoint-tree
      (cdr set)
      (adjoint-set-tree
        (car set)
        tree
      )
    )
  )
)


; ------
;     7
;    / \
;   3   9
;  / \   \
; 1   5  11

(make-adjoint-tree (list 7 3 1 5 9 11) '())
; (
;   7 
;   (
;     3
;     (1 () ())
;     (5 () ())
;   )
;   (
;     9
;     ()
;     (11 () ())
;   )
; )

; ------
;     3
;    / \
;   1   7
;      / \
;     5   9
;          \
;          11

(make-adjoint-tree (list 3 1 7 5 9 11) '())
; (
;   3 
;   (
;     1 () ()
;   )
;   (
;     7
;     (5 () ())
;     (
;       9 
;       () 
;       (11 () ())
;     )
;   )
; )

; ------
;     5
;    / \
;   3   9
;  /   / \
; 1   7   11

(make-adjoint-tree (list 5 3 1 9 7 11) '())

; (
;   5 
;   (
;     3 
;     (1 () ()) 
;     ()
;   )
;   (
;     9
;     (7 () ())
;     (11 () ())
;   )
; )

