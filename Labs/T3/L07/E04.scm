; Load OOP
(load "../../../lib/obj.scm")

; Define the class coke-machine. The instantiation arguments for a coke-machine are 
; the number of Cokes that can fit in the machine and the price (in cents) of a Coke. 

; creates a machine that can hold 80 Cokes and sells them for 70 cents each. coke-machine 
; objects must accept the following messages:

; (ask my-machine 'deposit 25) means deposit 25 cents. You can deposit several coins 
; and the machine should remember the total.

; (ask my-machine 'coke) means push the button for a Coke. The machine then either 
; 1) prints "Not enough money", 
; 2) prints "Machine empty", or 
; 3) returns the amount of change you get. The error messages should be printed using 
; display (for example, (display "Machine empty")). (After a successful transaction, 
; no money is left in the machine; i.e., change does not stay in the machine.)

; (ask my-machine 'fill 60) means add 60 Cokes to the machine.

(define-class (coke-machine cokes cost)
  (instance-vars
    (inserted 0)
  )
  (method (deposit amount)
    ; Update the total money inserted in the 
    ; machine
    (set! inserted (+ inserted amount))
  )
  (method (fill amount)
    ; Update the total cokes inserted in the
    ; machine
    (set! cokes (+ cokes amount))
  )
  (method (coke)
    ; If there are no cokes
    (if (= cokes 0)
      "Machine empty"
      ; If not enough money was inserted
      (if (< inserted cost)
        "Not enough money"
        (begin
          ; Else update all the variables
          (let 
            ((total-inserted inserted))
            (set! inserted 0)
            (set! cokes (- cokes 1))
            (- total-inserted cost)
          )
        )
      )
    )
  )
)

(define my-machine (instantiate coke-machine 80 70))
(ask my-machine 'fill 60)
(ask my-machine 'deposit 25)
(ask my-machine 'coke)
; "Not enough money"
;
(ask my-machine 'deposit 25) ;; Now there's 50 cents in there.
(ask my-machine 'inserted)
; 50
(ask my-machine 'deposit 25) ;; Now there's 75 cents.
(ask my-machine 'inserted)
; 75
(ask my-machine 'coke)
;; 5 

