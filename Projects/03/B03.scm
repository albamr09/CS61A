;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B03
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define a method take-all for people. If given that message, a person should take all 
; the things at the current location that are not already owned by someone

(load "adv-world.scm")

(define rolex (instantiate thing 'Rolex))
(ask BH-Office 'appear rolex)

(define pikachu (instantiate thing 'Pikachu))
(ask BH-Office 'appear pikachu)

(define usb (instantiate thing 'USB))
(ask 61A-Lab 'appear usb)

(print (ask BH-Office 'things))
; (rolex, pikachu)
(print (ask Brian 'possessions))
; ()
(ask Brian 'take-all)
(print (ask Brian 'possessions))
; (rolex, pikachu)

(ask hacker 'take usb)
(ask Brian 'go 'east)
; art-gallery 
(ask Brian 'go 'down)
; Soda
(ask Brian 'go 'down)
; 61A-Lab

(ask Brian 'take-all)
(print (ask Brian 'possessions))
; (rolex, pikachu)
(ask hacker 'lose usb)
(ask Brian 'take-all)
(print (ask Brian 'possessions))
; (rolex, pikachu, usb)
