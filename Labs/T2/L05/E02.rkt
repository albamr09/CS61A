#lang racket
(require berkeley)

; A binary mobile consists of two branches, a left branch and a right branch. 
; Each branch is a rod of a certain length, from which hangs either a 
; weight or another binary mobile. We can represent a binary mobile using compound data 
; by constructing it from two branches (for example, using list):

(define (make-mobile left right)
  ;(list left right)
  (cons left right)
)

; A branch is constructed from a length (which must be a number) 
; together with a structure, which may be either a number (representing 
; a simple weight) or another mobile:

(define (make-branch len structure)
  ;(list len structure)
  (cons len structure)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a. Write the corresponding selectors left-branch and right-branch, which return 
; the branches of a mobile, and branch-length and branch-structure, which return 
; the components of a branch.

(define (left-branch mobile)
  (car mobile)
)
(define (right-branch mobile)
  ;(car (cdr mobile))
  (cdr mobile)
)

(define (branch-length branch)
  (car branch)
)
(define (branch-structure branch)
  ;(car (cdr branch))
  (cdr branch)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; b. Using your selectors, define a procedure total-weight that returns the total 
; weight of a mobile.

(define (total-weight mobile)
  ; If it is not a pair, then there is 
  ; only one element in the branch structure, that is the weight
  (if (not(pair? mobile)) 
    ; Return mobile as the weight
    mobile
    ; Else sum the weights of the two branches
    (+
      (total-weight-branch (left-branch mobile))
      (total-weight-branch (right-branch mobile))
    )
  )
)

(define (total-weight-branch branch)
  ; If the branch is null return 0
  (if (null? branch) 
    0
    ; Else return the weight of the structure
    ; which might be an interger or another mobile
    ; both cases are handled by total-weight
    (total-weight (right-branch branch))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; c. A mobile is said to be balanced if the torque applied by its 
; top-left branch is equal to that applied by its top-right branch (that is, 
; if the length of the left rod multiplied by the weight hanging from that 
; rod is equal to the corresponding product for the right side) and if 
; each of the submobiles hanging off its branches is balanced. Design a predicate that 
; tests whether a binary mobile is balanced.

(define (balanced? mobile)
  (if (null? mobile)
    #f
    (and
      ; Check that the weight of the right branch 
      ; multiplied by its length equals the weight of the
      ; left branch multiplied by its lenght
      (equal-torque? mobile)
      ; Check that the submobiles of the left branch
      ; are balanced
      (balanced-branch? (left-branch mobile))
      ; Check that the submobiles of the right branch
      ; are balanced
      (balanced-branch? (right-branch mobile))
    )
  )
)


(define (balanced-branch? branch)
  (if (has-mobile? branch)
    ; If it has a mobile check if the mobile is balanced
    (balanced? (branch-structure branch))
    ; Else return true
    #t
  )
)

;;;;;;;;;
; Helper functions
;;;;;;;;;

(define (equal-torque? mobile)
  (=
    (*
      ; Multiply the length of the left branch
      (branch-length (left-branch mobile))
      ; by the weight of the left branch
      (total-weight-branch (left-branch mobile))
    )
    (*
      ; Multiply the length of the right branch
      (branch-length (right-branch mobile))
      ; by the weight of the right branch
      (total-weight-branch (right-branch mobile))
    )
  )
)

(define (has-mobile? branch)
  ; If the structure of the branch is a pair
  ; then it is a mobile
  (pair? (branch-structure branch))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; d. Suppose we change the representation of mobiles so that the constructors are

;(define (make-mobile left right) 
;  (cons left right)
;)
;
;(define (make-branch len structure)
;  (cons len structure)
;)

; How much do you need to change your programs to convert to the new representation?
;;;;;;;;
; 1. We change the constructors
; 2. Then we change the selectors that use (cdr). Instead of (car (cdr x)), we use just (cdr x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TEST

(define test-mobile
  (make-mobile
    (make-branch 
      1 
      (make-mobile
        (make-branch 11 11)
        (make-branch 12 12)
      )
    )
    (make-branch 2 22)
  )
)

(define test-mobile-semi-balanced
  (make-mobile
    (make-branch 
      1 
      (make-mobile
        (make-branch 11 11)
        (make-branch 12 12)
      )
    )
    (make-branch 
      1 
      (make-mobile
        (make-branch 11 11)
        (make-branch 12 12)
      )
    )
  )
)

(define test-mobile-balanced
  (make-mobile
    (make-branch 
      1 
      (make-mobile
        (make-branch 11 11)
        (make-branch 11 11)
      )
    )
    (make-branch 
      1 
      (make-mobile
        (make-branch 11 11)
        (make-branch 11 11)
      )
    )
  )
)


;(left-branch test-mobile)
;; '(1 ((11 11) (12 12)))
;(right-branch test-mobile)
;; '(2 22)

;(branch-length (left-branch test-mobile))
;; 1
;(branch-length (right-branch test-mobile))
;; 2
;
;(branch-structure (left-branch test-mobile))
;; ((11 11) (12 12))
;(branch-structure (right-branch test-mobile))
;; 22
;(left-branch (branch-structure (left-branch test-mobile)))
;; (11 11)
;(right-branch (branch-structure (left-branch test-mobile)))
;; (12 12)

;(total-weight test-mobile)
;;; 45
;
;(balanced? test-mobile)
;; f
;(balanced? test-mobile-semi-balanced)
;; f
;(balanced? test-mobile-balanced)
;; t
