;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B06_3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. Make an eat method for people.
;; 2. should look at your possessions and filter for all the ones that are edible
;; 3. should then add the calories value of the foods to your strength
;; 4. Then it should make the foods disappear (no longer be your possessions and 
;;    no longer be at your location)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGES
;; - Create eat method in person's class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./adv-world.scm")

(define Gus (instantiate person 'Gus BH-Office))
(define cheese (instantiate food 'cheese 150))
(print (ask BH-Office 'things))
; ()
(ask BH-Office 'appear cheese)
(print (ask BH-Office 'things))
; (cheese)

(print (ask Gus 'strength))
; 50
(ask Gus 'eat)
; Does nothing
(print (ask Gus 'strength))
; 50

(ask Gus 'take cheese)
(print (ask Gus 'possessions))
; (cheese)
(ask Gus 'eat)
(print (ask Gus 'strength))
; 200
(print (ask Gus 'possessions))
; ()
(print (ask BH-Office 'things))
; ()
