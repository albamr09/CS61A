;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A05
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1. A vehicle is just a thing
; 2. You'll have to invent a special kind of place called a garage. 
;   - Garages have two methods (besides the ones all places have): park and unpark. 
;   - The park method takes a vehicle (a thing) as its argument. 
;       a. First check to be sure that the vehicle is actually in the garage. (The 
;         person who possesses the vehicle will enter the garage, then ask to park it, 
;         so the vehicle should have entered the garage along with the person before the 
;         park message is sent.) 
;       b. Generate a ticket with a unique serial number. (The counter for serial numbers 
;           should be shared among all garages,
;       c. You'll associate the ticket number with the vehicle in a key-value table, we 
;           need a separate table for every garage.
;       d. Make a table entry with the ticket number as the key, and the vehicle as the value. 
;       e. Ask the vehicle's owner to lose the vehicle and take the ticket.
;   - The unpark method takes a ticket as argument. 
;       a. First make sure the object you got is actually a ticket (by checking the name)
;       b. Look up the ticket number in the garage's table. If you find a vehicle, ask the 
;           ticket's owner to lose the ticket and take the vehicle. 
;       c. Insert #f in the table for that ticket number, so that people can't unpark the vehicle twice.
; 3. You'll also need a special kind of thing called a ticket; 
;   - It has a number as an instantiation variable.
;   - Every ticket should have the name ticket.

(load "adv-world.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGES, all in adv.scm
;; 1. We created a child class of place: garage
;; 2. We created a child class of thing: ticket (same syntanx as the thing class)
;; 3. We created an auxiliary procedure: ticket?
;; 4. We create and auxiliary procedure: get-ticket, filter-ticket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mercadona-parking (instantiate garage 'Mercadonas-Parking))
(can-go BH-Office 'west mercadona-parking)
(can-go mercadona-parking 'east BH-Office)

(define lone-car (instantiate thing 'Lone-car))
(ask mercadona-parking 'appear lone-car)
(define batmans-car (instantiate thing 'Batmans-Car))
(ask BH-Office 'appear batmans-car)
(define barbies-car (instantiate thing 'Barbies-Car))
(ask BH-Office 'appear barbies-car)
(ask Brian 'take barbies-car)

;;;;;;;;;;;;;;;;;;;;
;; TEST PARK
(ask Brian 'go 'west)
(ask mercadona-parking 'park barbies-car)
; Brian took ticket
(ask Brian 'go 'east)

;; TEST UNPARK

(ask Brian 'go 'west)
(ask mercadona-parking 'unpark (get-ticket Brian))
; Brian took Barbies-car


;; TEST ERRORS

; Unpark the same car
; (ask mercadona-parking 'unpark (instantiate ticket 1))
; Hmm, I cannot seem to find your car miss...

; (ask mercadona-parking 'unpark barbies-car)
; Oh dear! This is not a ticket, you can only unpark with a ticket

; (ask mercadona-parking 'park batmans-car)
; I am sorry, but you cannot a park a car if it is not in the park

; (ask mercadona-parking 'unpark (instantiate ticket 10))
; Hmm, I cannot seem to find your car miss...

; (ask mercadona-parking 'park lone-car)
; Wait a minute! This car has no owner!
