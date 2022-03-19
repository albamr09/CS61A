;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B06_1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. Create a food class
;; 2. Two properties, an edible? property and a calories property.
;; 3. Replace the procedure named edible?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGES
;; - Create food class
;; - Edit edible? procedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./adv-world.scm")

(define pesto-pasta (instantiate food 'pasta 150))
(print (ask pesto-pasta 'calories))
; 150
(print (ask pesto-pasta 'edible?))
; #t
(print (ask bagel edible?))
; #f
(print (edible? pesto-pasta))
; #t
(print (edible? bagel))
; #f


