; Cy D. Fect, looking forward to the day when
; he will rise in the organization, gives a query to find all the
; wheels (using the wheel rule of Section 4.4.1):

; (wheel ?who)

; To his surprise, the system responds

;;; Query results:

; (wheel (Warbucks Oliver))
; (wheel (Bitdiddle Ben))
; (wheel (Warbucks Oliver))
; (wheel (Warbucks Oliver))
; (wheel (Warbucks Oliver))
 
; Why is Oliver Warbucks listed four times?

; This initializes the interpreter
(load "../../../lib/query.scm")
(initialize-data-base microshaft-data-base)
(query-driver-loop)

; Copy and paste this to the terminal to save the rule in the database
(assert! (rule (wheel ?person)
  (and 
    (supervisor ?middle-manager ?person)
    (supervisor ?x ?middle-manager)
  )
))

; Because there are four ?x that satisfy the rules:

(wheel (warbucks oliver) (cratchet robert) (scrooge eben))
(wheel (warbucks oliver) (tweakit lem e) (bitdiddle ben))
(wheel (warbucks oliver) (fect cy d) (bitdiddle ben))
(wheel (warbucks oliver) (hacker alyssa p) (bitdiddle ben))
(wheel (bitdiddle ben) (reasoner louis) (hacker alyssa p))
