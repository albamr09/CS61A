;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B06_2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. Make a pasta class that inherits from food
;; 2. Pasta should not have any instantiation variable
;; 3. Give the pasta a class-variable called name whose value is the 
;     word pasta

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGES
;; - Created pasta class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./adv-world.scm")
(define pesto-pasta (instantiate pasta))
(print (ask pesto-pasta 'calories))
; 150
(print (ask pesto-pasta 'edible?))
; #t
