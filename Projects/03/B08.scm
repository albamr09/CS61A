;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B08
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. Reorganize take
;; 2. If its possessor is'no-one, go ahead and take it as always.
;; 3. Otherwise:
;;    - Invoke (ask thing 'may-take? receiver)
;; 4. may-take? method of the thing
;;    - Compare the strength of its owner with the strength of 
;;      the requesting person
;; 5. If the receiver has the same strength as the holder, the 
;;    receiver may take the object.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGES
;; - Added may-take? method to thing class
;; - Changed take thing method of person's class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./adv-world.scm")

(define radio (instantiate thing 'radio))
(define phone (instantiate thing 'phone))
(define pipe (instantiate thing 'pipe))
(ask BH-Office 'appear radio)
(ask BH-Office 'appear phone)
(ask BH-Office 'appear pipe)
(define Hulk (instantiate police 'Hulk BH-Office))
(define Freud (instantiate person 'Freud BH-Office))


(ask Brian 'take radio)
; Brian took radio
(ask Freud 'take pipe)
; Freud took pipe
(ask Brian 'take pipe)
; Brian took pipe

;; TEST ERRORS
(ask Hulk 'take phone)
; Hulk took phone
(ask Brian 'take phone)
; May the force be with you
