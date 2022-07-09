; Write a rule that reverses a list
;;; Query input:
; (reverse ?what (a b c)) 
;;; Query results:
; (REVERSE (C B A) (A B C)) 
;;; Query input:
; (reverse (a b c) ?what) 
;;; Query results:
; (REVERSE (A B C) (C B A))

; This initializes the interpreter
(load "../../../lib/query.scm")
(initialize-data-base microshaft-data-base)
(query-driver-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Forward recursion
; Given original list, find reversed list
; (forward-reverse (a b c) ?what)

; Base case for recursion: on empty list return empty list
(assert!
  (rule 
    (forward-reverse () ())
  )
)

; Recursive case
(assert!
  (rule 
    ; Split list into x=first element and v=rest of list
    ; the first element, x, is the last element of the reversed list
    ; the rest of the list, has yet to be reversed, once reversed it equals y
    (forward-reverse (?x . ?v) (?y . ?x))
    ; Apply forward-reverse on rest of list, v (note that the result of the 
    ; reversion of v is y)
    (forward-reverse ?v ?y)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Backward recursion
; Given reversed list, find original list
; (backward-reverse ?what (a b c))

; Base case for recursion: on empty list return empty list
(assert!
  (rule 
    (backward-reverse () ())
  )
)

; Recursive case
(assert!
  (rule 
    ; Split list into x=first element of reversed list and v=rest of list
    ; the first element, x, is the last element of the original list
    ; the rest of the list, has yet to be un-reversed, once un-reversed it equals y
    (backward-reverse (?y . ?x) (?x . ?v))
    ; Apply backward-reverse on rest of list, v (note that the result of the 
    ; un-reversion of v is y)
    (backward-reverse ?y ?v)
  )
)

;; Unify both cases of forward and backward reverse
(assert!
  (rule
    (reverse ?original ?reversed)
    ; Either match forward or backward reverse
    (or
      (forward-reverse ?original ?reversed)
      (backward-reverse ?original ?reversed)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TEST

;;; Query input:
 (reverse ?what (a b c)) 

;;; Query results:
 (REVERSE (C B A) (A B C)) 

;;; Query input:
 (reverse (a b c) ?what) 

;;; Query results:
 (REVERSE (A B C) (C B A))
