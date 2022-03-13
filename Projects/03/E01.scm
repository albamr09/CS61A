(load "./adv-world.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E01
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1. Instantiate a new Person object to represent yourself. 
; 2. Put yourself in a new place called dormitory (or wherever you live) and 
; 3. connect it to campus so that it is a reachable place. 
; 4. Create a place called kirin, north of soda. (It's actually on Solano Avenue.) 
; 5. Put a thing called potstickers there. 
; 6. Then give the necessary commands to move your character to kirin, 
; 7. take the potstickers, 
; 8. then move yourself to where Brian is, 
; 9. put down the potstickers, 
; 10. and have Brian take them. 
; 11. Then go back to the lab and get back to work. 

; (There is no truth to the rumor that you'll get an A in the course for doing this in real life!) 
; All this is just to ensure that you know how to speak the language of the adventure program.

; List all messages that are sent during this episode. It's a good idea to see if you can work this out 
; in your head, at least for some of the actions that take place, but you can also trace the ask procedure to get a 
; complete list. You don't have to hand in this listing of messages. (You must turn in a transcript of the episode 
; without the tracing.) The purpose of this exercise is to familiarize you with the ways in which the different 
; objects send messages back and forth as they do their work.

; [Tip: we have provided a move-loop procedure that you may find useful as an aid in debugging your work. You can use it to move a person repeatedly.]

;;; 1. Instantiate a new Person object to represent yourself. 
;;; 2. Put yourself in a new place called dormitory (or wherever you live) and 

; Create the place where i live
(define SimCity (instantiate place 'sim-city))

; Create myself, indicate where i live
(define Alba (instantiate person 'Alba SimCity))

;;; 3. connect it to campus so that it is a reachable place. 

(can-go SimCity 'down Soda)

;;; 4. Create a place called kirin, north of soda. (It's actually on Solano Avenue.) 

(define Kirin (instantiate place 'kirin))

; Kirin is north of art-gallery and art-gallery is north of Soda, so by transitivity Kirin 
; is north of Soda
(can-go Kirin 'down art-gallery)
(can-go art-gallery 'up Kirin)

;; 5. Put a thing called potstickers there. 

(define potstickers (instantiate thing 'potstickers))
(ask Kirin 'appear potstickers)

;;; 6. Then give the necessary commands to move your character to kirin, 

; Interactive method:
; (move-loop Alba)
; - down: Move from SimCity to Soda
; - up: Move from Soda to art-gallery
; - up: Move from art-gallery to Kirin
; - stop: Finish moving
; Direct method:
(ask Alba 'go 'down)
(ask Alba 'go 'up)
(ask Alba 'go 'up)

; 7. take the potstickers

(ask Alba 'take potstickers)

; 8. then move yourself to where Brian is, 

; Interactive method:
; (move-loop Alba)

; Brian is in BH-Office
; - down: Move from Kirin to art-gallery
; - west: Move from art-gallery to BH-Office
; - stop: Finish moving

; Direct method:
(ask Alba 'go 'down)
(ask Alba 'go 'west)

; 9. put down the potstickers, 

(ask Alba 'lose potstickers)

; 10. and have Brian take them. 

(ask Brian 'take potstickers)

; 11. Then go back to the lab and get back to work. 

; Interactive method
; (move-loop Alba)
; - east: Move from BH-Office to art-gallery
; - down: Move from art-gallery to Soda
; - down: Move from Soda to 61A-Lab
; - stop: Finish moving

; Direct method:
(ask Alba 'go 'east)
(ask Alba 'go 'down)
(ask Alba 'go 'down)

