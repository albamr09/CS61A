(load "adv-world.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E04_2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1. Invent a may-enter? message for places that takes a person as an argument and always returns #t.

; 2. Invent a locked-place class in which the may- enter? method returns #t if the place is unlocked, or #f if it's locked.

; 3. The locked-place class must also have an unlock message. For simplicity, write this method with no arguments and have it always succeed. 

; EXTRA. In a real game, we would also invent keys, and a mechanism requiring that the person have the correct key in order to unlock the door. (That's why may-enter? takes the person as an argument.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The modifications where made in "adv.scm"
; 1. We created a new classe, the locked-place class
; 2. We added the may-enter? enter method to the place class
; 3. We modified the enter method in the person class, so it checks
;    if the new place is locked
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define janitors-office (instantiate locked-place 'Janitor-Office 'test-lock))

(can-go BH-Office 'north janitors-office)
(can-go janitors-office 'south BH-Office)

(ask Brian 'go 'north)
; Where do you think you are going?!?! The place is locked!

(ask janitors-office 'unlock 'test-lock)
(ask Brian 'go 'north)

