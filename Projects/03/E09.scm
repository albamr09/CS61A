;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E09
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. Edit buy method
;;    - When a police asks to buy some food, the restaurant does 
;;      not charge them any money


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGES
;; - Modified sell method of restaurant's class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./adv-world.scm")

(define Gusteaus (instantiate restaurant 'Gusteaus pasta 10))
(define very-morally-correct-policeman (instantiate police 'literally-all-police Gusteaus))

(ask Brian 'go-directly-to Gusteaus)
(print (ask (ask Gusteaus 'sell Brian 'pasta) 'name))
; pasta
(print (ask (ask Gusteaus 'sell very-morally-correct-policeman 'pasta) 'name))
; pasta

(print (ask Brian 'money))
; 90
(print (ask very-morally-correct-policeman 'money))
; 100
