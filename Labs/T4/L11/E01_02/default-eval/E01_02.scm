;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E01_02
; SICP 4.6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; let expressions are derived expressions, because

; >>	(let 
; >>		(
; >>			(⟨var1⟩ ⟨exp1⟩) 
; >>			... 
; >>			(⟨varn⟩ ⟨expn⟩)
; >>		)
; >>		⟨body⟩
; >>	)

; is equivalent to

; >>	(
; >>		(lambda 
; >>			(⟨var1⟩ ... ⟨varn⟩)
; >>			⟨body⟩
; >>		)
; >>		; Call lambda function with exp as arguments
; >>		⟨exp1⟩
; >>		...
; >>		⟨expn⟩
; >>	)

; Implement a syntactic transformation let->combination that reduces 
; evaluating let expressions to evaluating combinations of the type 
; shown above, and add the appropriate clause to eval to handle let expressions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Changes:

; 1. We added the expression type and its corresponding evaluation
;     to the table of expression types-evaluation rule
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Data directed version
(load "./interpreter/eval-pkg")

; Run interpreter
(driver-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST

; Run this first
(define (append x y)
(if (null? x)
y (
cons (car x) (append (cdr x) y))))

; Test this second
(let
  (
    (s1 (append '(a b c) '(d e f)))
  )
  (let
    (
      (s2 (append '(g h i) '(j k l)))
      (s3 (append '(m n o) '(p q r)))
    )
    (append (append s1 s2) s3)
  )
)


