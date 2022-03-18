;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B05_2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Invent the laptop class. 
; 1. A laptop has one instantiation variable, its name.
; 2. A laptop is a thing that has two extra methods: 
;   - Connect, with a password as argument, sends a connect message to the place 
;     where the laptop is. If the password is wrong, return an error.
; 3. Surf, with a URL text string as argument, sends a surf message to the place 
;   where it is.
; 4. Whenever a laptop enters a new hotspot, the user must ask to connect to that hotspot's network; 
; 5. When the laptop leaves the hotspot, it must automatically be disconnected from the network.
; 6. If it's in a place other than a hotspot, the surf message won't be understood; 
; 7. If it's in a hotspot but not connected, return an error.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGES
;; - Created laptop class
;; - Extended enter procedure in hotspot class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./adv-world.scm")

(define nokia-6310 (instantiate laptop 'nokia-6310))
(define Tima (instantiate laptop 'Tima))
(define Ingsoc (instantiate hotspot 'Ingsoc 'Big-Brother))
(define Winston (instantiate person 'Winston BH-Office))
(ask BH-Office 'appear nokia-6310)
(ask Ingsoc 'appear Tima)
(ask Winston 'take nokia-6310)
(can-go Ingsoc 'north BH-Office)
(can-go BH-Office 'south Ingsoc)

(ask Winston 'go 'south)
; If you do not enter 'Big-Brother in the input you get a password error
(print (ask Ingsoc 'connections))
; (nokia-6310)

; (ask nokia-6310 'surf "google.com")
; Opens firefox tab (if you have installed it obviously)

(ask Winston 'go 'north)
(print (ask Ingsoc 'connections))
; ()


;; TEST ERRORS

; (ask Winston 'lose nokia-6310)
; (ask Winston 'go 'south)
; (ask Winston 'take Tima)
; (ask Tima 'surf "google.com")
; You have to connect before you surf The Internet, hmm...

; (ask Winston 'go 'south)
; (ask nokia-6310 'connect 'Big-Br)
; It sure seems the password is not correct miss

; (ask Winston 'lose nokia-6310)
; (ask nokia-6310 'connect 'Big-Brother)
; This is confusing, this laptop has no owner, how can it connect itself?

; (ask Winston 'lose nokia-6310)
; (ask nokia-6310 'surf 'Big-Brother)
; Hah? This laptop does not have an owner!
