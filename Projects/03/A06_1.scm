;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A06_1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. Create a place called jail
;; 2. Jail has no exits
;; 3. Create a method for persons and thieves called go-directly-to:  does not require that 
;;    the new-place be adjacent to the current-place

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGES
;; - Create jail class, child of place
;; - Create go-directly-to method in person's class
;;   it uses the logic from go
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./adv-world.scm")

(define Gulag (instantiate jail 'Gulag))
(can-go Gulag 'north BH-Office)
(can-go BH-Office 'south Gulag)

; (ask Brian 'go 'south)
; (print (ask Brian 'place))
; (ask Brian 'go 'north)
; Ohoho, one does not leave prison that easily
(ask Brian 'go-directly-to Soda)
