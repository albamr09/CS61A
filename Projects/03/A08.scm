;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A08
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. Create a buy method in people class
;; 2. It should take as argument the name of the food we want to buy
;; 3. The method must send a sell message to the restaurant.
;;    - Succeeds: The new food should be added to the person's possessions.
;;    - Fails: return an error 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./adv-world.scm")

(define Gusteaus (instantiate restaurant 'Gusteaus pasta 10))
(define Remy (instantiate person 'Remy Gusteaus))

(print (ask (ask Remy 'buy 'pasta) 'name))
; pasta
(ask Remy 'pay-money 90)


;; TEST ERRORS

; (print (ask (ask Remy 'buy 'pasta) 'name))
; Your card has been declined ma'am
; Well, I guess you should try another restaurant

; (print (ask (ask Remy 'buy 'big-mac) 'name))
; We do not sell this... thing you callbig-mac
; Well, I guess you should try another restaurant
