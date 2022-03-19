;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A07_2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. We're going to invent a new kind of place, called a restaurant
;; 2. Each restaurant serves only one kind of food
;; 3. When a restaurant is instantiated, it should have two extra arguments
;;    - The class of food objects that this restaurant sells
;;    - The price of one item of this type
;; 4. The menu method returns a list containing the name and price of the 
;;    food that the restaurant sells.
;; 5. The sell method takes two arguments, the person who wants to buy something 
;;    and the name of the food that the person wants. 
;;    - Check the restaurant sells that kind of food
;;    - Check if the person has enough money
;;    - If so, instantiate the food class and return the new food object

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGES
;; - Created restaurant class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./adv-world.scm")
(define Gusteaus (instantiate restaurant 'Gusteaus pasta 10))

(print (ask Gusteaus 'menu))
; (pasta 10)
(ask Brian 'go-directly-to Gusteaus)
(print (ask (ask Gusteaus 'sell Brian 'pasta) 'name))
; pasta


;; TEST ERRORS
(ask Brian 'pay-money 90)
; (print (ask (ask Gusteaus 'sell Brian 'pasta) 'name))
; "Your card has been declined ma'am"

; (ask Gusteaus 'sell Brian 'burger-kangreburger)
; We do not sell this... thing you call burger-kangreburger

(ask Brian 'go-directly-to BH-Office)
; (print (ask (ask Gusteaus 'sell Brian 'pasta) 'name))
; Oh nonono, we do not do delivery in here!
