;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A07_1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. Give person objects a money property
;; 2. We'll have every person start out with $100
;; 3. Create two methods for people, get-money and pay-money
;;    Each of which takes a number as argument and updates the person's money 
;;    value appropriately
;; 4. Pay-money must return true or false depending on whether the person had 
;;    enough money.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGES
;; - Add new property "money" to person's class
;; - Initialize "money" to 100 in person's class
;; - Create method get-money in person's class
;; - Create method pay-money in person's class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./adv-world.scm")

(print (ask Brian 'money))
; 100
(ask Brian 'get-money 20)
(print (ask Brian 'money))
; 120

(ask Brian 'pay-money 30)
(print (ask Brian 'money))
; 90

(print (ask Brian 'pay-money 100))
; #f
(print (ask Brian 'money))
; 90



