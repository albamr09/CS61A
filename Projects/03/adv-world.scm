(load "./adv.scm")
;;;  Data for adventure game.  This file is adv-world.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting up the world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Soda (instantiate place 'Soda))
(define BH-Office (instantiate place 'BH-Office))
(define MJC-Office (instantiate place 'MJC-Office))
(define art-gallery (instantiate place 'art-gallery))
(define Pimentel (instantiate place 'Pimentel))
(define 61A-Lab (instantiate place '61A-Lab))
(define Sproul-Plaza (instantiate place 'Sproul-Plaza))
(define Telegraph-Ave (instantiate place 'Telegraph-Ave))
(define Noahs (instantiate place 'Noahs))
(define Intermezzo (instantiate place 'Intermezzo))
(define Haas (instantiate place 'Haas-Business-School))
(define s-h (instantiate place 'sproul-hall))


(can-go Soda 'up art-gallery)
(can-go art-gallery 'down Soda)
(can-go art-gallery 'west BH-Office)
(can-go BH-Office 'east art-gallery)
(can-go art-gallery 'east MJC-Office)
(can-go MJC-Office 'west art-gallery)
(can-go Soda 'down 61A-Lab)
(can-go 61A-Lab 'up Soda)
(can-go Soda 'south Pimentel)
(can-go Pimentel 'north Soda)
(can-go Pimentel 'south Haas)
(can-go Haas 'north Pimentel)
(can-go Haas 'west s-h)
(can-go s-h 'east Haas)
(can-go Sproul-Plaza 'east s-h)
(can-go s-h 'west Sproul-Plaza)
(can-go Sproul-Plaza 'north Pimentel)
(can-go Sproul-Plaza 'south Telegraph-Ave)
(can-go Telegraph-Ave 'north Sproul-Plaza)
(can-go Telegraph-Ave 'south Noahs)
(can-go Noahs 'north Telegraph-Ave)
(can-go Noahs 'south Intermezzo)
(can-go Intermezzo 'north Noahs)

;; Some people.
; MOVED above the add-entry-procedure stuff, to avoid the "The computers
; seem to be down" message that would occur when hacker enters 61a-lab
; -- Ryan Stejskal

(define Brian (instantiate person 'Brian BH-Office))
(define hacker (instantiate person 'hacker 61A-Lab))
(define nasty (instantiate thief 'nasty Sproul-Plaza))

(define (sproul-hall-exit)
  (error "You can check out any time you'd like, but you can never leave")
)

(define (bh-office-exit)
  (print "What's your favorite programming language?")
  (let ((answer (read)))
    (if (eq? answer 'scheme)
	    (print "Good answer, but my favorite is Logo!")
	    (begin 
        (newline) 
        (bh-office-exit)
      )
    )
  )
)

(ask s-h 'add-entry-procedure
 (lambda () (print "Miles and miles of students are waiting in line..."))
)

(ask s-h 'add-exit-procedure sproul-hall-exit)
(ask BH-Office 'add-exit-procedure bh-office-exit)

(ask Noahs 'add-entry-procedure
 (lambda () (print "Would you like lox with it?"))
)

(ask Noahs 'add-exit-procedure
 (lambda () (print "How about a cinnamon raisin bagel for dessert?"))
)

(ask Telegraph-Ave 'add-entry-procedure
 (lambda () (print "There are tie-dyed shirts as far as you can see..."))
)

(ask 61A-Lab 'add-entry-procedure
 (lambda () (print "The computers seem to be down"))
)

(ask 61A-Lab 'add-exit-procedure
 (lambda () (print "The workstations come back to life just in time."))
)

;; Some things.

(define bagel (instantiate thing 'bagel))
(ask Noahs 'appear bagel)

(define coffee (instantiate thing 'coffee))
(ask Intermezzo 'appear coffee)


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

