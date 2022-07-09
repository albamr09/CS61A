(load "../../../lib/query.scm")
(query)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATABASE

(initialize-data-base microshaft-data-base)
; Same as running all of this
(assert! (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
(assert! (job (Bitdiddle Ben) (computer wizard)))
(assert! (salary (Bitdiddle Ben) 60000))
(assert! (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(assert! (job (Hacker Alyssa P) (computer programmer)))
(assert! (salary (Hacker Alyssa P) 40000))
(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))
(assert! (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(assert! (job (Fect Cy D) (computer programmer)))
(assert! (salary (Fect Cy D) 35000))
(assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))
(assert! (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(assert! (job (Tweakit Lem E) (computer technician)))
(assert! (salary (Tweakit Lem E) 25000))
(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))
(assert! (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(assert! (job (Reasoner Louis) (computer programmer trainee)))
(assert! (salary (Reasoner Louis) 30000))
(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))
(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
(assert! (address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(assert! (job (Warbucks Oliver) (administration big wheel)))
(assert! (salary (Warbucks Oliver) 150000))
(assert! (address (Scrooge Eben) (Weston (Shady Lane) 10)))
(assert! (job (Scrooge Eben) (accounting chief accountant)))
(assert! (salary (Scrooge Eben) 75000))
(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))
(assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(assert! (job (Cratchet Robert) (accounting scrivener)))
(assert! (salary (Cratchet Robert) 18000))
(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))
(assert! (address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(assert! (job (Aull DeWitt) (administration secretary)))
(assert! (salary (Aull DeWitt) 25000))
(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))
(assert! (can-do-job (computer wizard) (computer programmer)))
(assert! (can-do-job (computer wizard) (computer technician)))
(assert! (can-do-job (computer programmer) (computer programmer trainee)))
(assert! (can-do-job (administration secretary) (administration big wheel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Simple query

;;; Query Input
(job ?x (computer programmer))
;;; Query results:
(job (fect cy d) (computer programmer))
(job (hacker alyssa p) (computer programmer))

;;; Query Input
(job ?x (computer ?type))
;;; Query results:
(job (tweakit lem e) (computer technician))
(job (fect cy d) (computer programmer))
(job (hacker alyssa p) (computer programmer))
(job (bitdiddle ben) (computer wizard))

;;; Query Input
(job ?x (computer . ?type))
;;; Query results:
(job (reasoner louis) (computer programmer trainee))
(job (tweakit lem e) (computer technician))
(job (fect cy d) (computer programmer))
(job (hacker alyssa p) (computer programmer))
(job (bitdiddle ben) (computer wizard))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compound queries

;;; Query Input
(and 
  (job ?person (computer programmer))
  (address ?person ?where)
)
;;; Query results:
(and (job (fect cy d) (computer programmer)) (address (fect cy d) (cambridge (ames street) 3)))
(and (job (hacker alyssa p) (computer programmer)) (address (hacker alyssa p) (cambridge (mass ave) 78)))

;;; Query Input
(or 
  (supervisor ?x (Bitdiddle Ben))
  (supervisor ?x (Hacker Alyssa P))
)
;;; Query results:
(or (supervisor (tweakit lem e) (bitdiddle ben)) (supervisor (tweakit lem e) (hacker alyssa p)))
(or (supervisor (reasoner louis) (bitdiddle ben)) (supervisor (reasoner louis) (hacker alyssa p)))
(or (supervisor (fect cy d) (bitdiddle ben)) (supervisor (fect cy d) (hacker alyssa p)))
(or (supervisor (hacker alyssa p) (bitdiddle ben)) (supervisor (hacker alyssa p) (hacker alyssa p)))

;;; Query Input
(and (salary ?person ?amount) (lisp-value > ?amount 30000))
;;; Query results:
(and (salary (scrooge eben) 75000) (lisp-value > 75000 30000))
(and (salary (warbucks oliver) 150000) (lisp-value > 150000 30000))
(and (salary (fect cy d) 35000) (lisp-value > 35000 30000))
(and (salary (hacker alyssa p) 40000) (lisp-value > 40000 30000))
(and (salary (bitdiddle ben) 60000) (lisp-value > 60000 30000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Rules

;;; Query Input
(assert! (rule (same ?x ?x)))
(assert! (rule (lives-near ?person-1 ?person-2)
  (and 
    (address ?person-1 (?town . ?rest-1))
    (address ?person-2 (?town . ?rest-2))
    (not (same ?person-1 ?person-2))
  )
))

;;; Query Input
(lives-near ?x (Bitdiddle Ben))
;;; Query results:
(lives-near (Reasoner Louis) (Bitdiddle Ben))
(lives-near (Aull DeWitt) (Bitdiddle Ben))

;;; Query Input
(assert! (rule (wheel ?person)
  (and 
    (supervisor ?middle-manager ?person)
    (supervisor ?x ?middle-manager)
  )
))

;;; Query Input 
(assert! (rule (outranked-by ?staff-person ?boss)
  (or 
    (supervisor ?staff-person ?boss)
    (and 
      (supervisor ?staff-person ?middle-manager)
      (outranked-by ?middle-manager ?boss)
    )
  )
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Logic

;;; Query Input 
(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule 
  (append-to-form (?u . ?v) ?y (?u . ?z))
  (append-to-form ?v ?y ?z)
))

;;; Query Input 
(append-to-form (a b) (c d) ?z)
;;; Query results:
(append-to-form (a b) (c d) (a b c d))

;;; Query input:
(append-to-form (a b) ?y (a b c d))
;;; Query results:
(append-to-form (a b) (c d) (a b c d))

;;; Query input:
(append-to-form ?x ?y (a b c d))
;;; Query results:
(append-to-form () (a b c d) (a b c d))
(append-to-form (a) (b c d) (a b c d))
(append-to-form (a b) (c d) (a b c d))
(append-to-form (a b c) (d) (a b c d))
(append-to-form (a b c d) () (a b c d))
