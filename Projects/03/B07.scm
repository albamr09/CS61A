;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B07
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. Define the police class
;; 2. When the police notices a new person entering where he is, the 
;;    police checks to see if that person is a thief.
;; 3. If the person is a thief 
;;    - The police says "Crime Does Not Pay"
;;    - Takes away all the thief's possessions
;;    - Sends the thief directly to jail
;; 4. Give thieves and police default strengths
;;    - Thieves should start out stronger than persons
;;    - Police should be stronger than thieves

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGES
;; - Created police class
;; - Modified enter method in place's class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./adv-world.scm")

(define somepolice (instantiate police 'grammarpolice Soda))
(define jeff-bezos (instantiate thief 'Jeff-Bezos art-gallery))
(define amazon (instantiate thing 'amazon))
(ask art-gallery 'appear amazon)
(ask jeff-bezos 'take amazon)

(ask jeff-bezos 'go 'down)
; Crime Does Not Pay
(print (ask somepolice 'possessions))
; (amazon)
(print (ask (ask jeff-bezos 'place) 'name))
; (azkaban)
(print (ask Soda 'people))
; (somepolice)
(print (ask art-gallery 'people))
; ()

(print (ask somepolice 'strength))
; 200
(print (ask jeff-bezos 'strength))
; 150
