;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E04_04
; SICP 4.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Install and and or as new special forms for the evaluator by
; defining appropriate syntax procedures and evaluation procedures 
; eval-and and eval-or. Alternatively, show how to implement and and 
; or as derived expressions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Changes:
; 1. Added representation for and and or in interpreter/P01_02.scm
; 2. Added evaluation rules for and and or in interpreter/P01_01.scm
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

(and (= 1 1) (= 0 0))
; t
(and (= 1 1) (= 1 0))
; f
(or (= 1 1) (= 1 0))
; t
(or (= 1 0) (= 1 2))
; f
