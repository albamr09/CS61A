;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A06_2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. Modify the thief class so that a thief won't try to leave a place with no exits.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGES
;; - Created new has-exit? property in jail, controls if a user can leave
;; - Added check in run method in the thief's class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./adv-world.scm")

(define Doras-Basement (instantiate jail 'Doras-Basement))
(can-go Doras-Basement 'north BH-Office)
(define swipper (instantiate thief 'Swipper Doras-Basement))

; Before go to the thief class and change the behavior 
; instance variable to run, so the thief runs when
; Brians enters Doras-Basement
(ask Brian 'go-directly-to Doras-Basement)

