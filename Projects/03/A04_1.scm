(load "adv-world.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A04
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Modify the enter method for places, so that in addition to what that 
; method already does, it sends a notice message to each person in that 
; place other than the person who is entering. The notice message should 
; have the newly-entered person as an argument.

(define singer (instantiate person 'rick Sproul-Plaza))
(ask singer 'set-talk "My funny valentine, sweet comic valentine")

(define preacher (instantiate person 'preacher Sproul-Plaza))
(ask preacher 'set-talk "Praise the Lord")

(define street-person (instantiate person 'harry Telegraph-Ave))
(ask street-person 'set-talk "Brother, can you spare a buck")

; Try walking around to sproul-plaza and telegraph-ave to see if the messages 
; are triggered.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For this exercise the method "enter" inside adv.scm has been modified
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Remove exit methods, to avoid errors from sproul-hall
(ask s-h 'clear-all-procs)

; BH-Office to art-gallery
(ask Brian 'go 'east)
; art-gallery to Soda
(ask Brian 'go 'down)
; Soda to Pimentel 
(ask Brian 'go 'south)
; Pimentel to Haas-Business-School
(ask Brian 'go 'south)
; Haas-Business-School to sproul-hall
(ask Brian 'go 'west)

; sproul-hall to Sproul-Plaza
(ask Brian 'go 'west)
; Triggers the singer's and the preacher's message

; Sproul-Plaza to Telegraph-Ave
(ask Brian 'go 'south)
; Triggers the street-person's message
