;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E01_05
; SICP 4.11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Instead of representing a frame as a pair of
; lists, we can represent a frame as a list of bindings, where
; each binding is a name-value pair. Rewrite the environment
; operations to use this alternative representation.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Changes:
; 1. Change make-frame, so now each frame is a list of pairs variable-value
; 2. Change frame-variables and frame-values selectors to select correctly the
;     the variables and values
; 3. Change add-binding-to-frame! to update correctly the frame, adding a new 
;     pair to it
; 4. Change lookup-variable-value to use assoc to search for the variable by 
;     its name in the list of pairs
; 5. Change set-variable-value! to use assoc to search for the variable by 
;     its name in the list of pairs, and update the value correctly
; 6. Change define-variable! to use assoc to search for the variable by 
;     its name in the list of pairs, and update the values of the variable 
;     correctly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Expression representation 
(load "./interpreter/P01_02.scm")
; Environment
(load "./interpreter/P01_03.scm")
; Load changed eval
(load "./interpreter/P01_01.scm")
; Load interpreter
(load "./interpreter/P01_04.scm")

; Run interpreter
(driver-loop)

;;;;;;;;;;;;;;
; TEST
;;;;;;;;;;;;;;

; First
(define (append x y)
(if (null? x)
y (
cons (car x) (append (cdr x) y))))

; Then
(append '(a b c) '(d e f))
; (a b c d e f)

; Run interactively:
(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) x)
; 3

(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
; 39

(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (begin (+ x 1) (* z z)))
; 169

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
