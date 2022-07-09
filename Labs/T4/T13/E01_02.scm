; Define a rule that says that person 1 can replace person 2 if either person 1 does the same job as person
; 2 or someone who does person 1’s job can also do person 2’s job, and if person 1 and person 2 
; are not the same person. Using your rule, give queries that find the following:

; a. all people who can replace Cy D. Fect;

; b. all people who can replace someone who is being paid more than they are, together with the two salaries.

; This initializes the interpreter
(load "../../../lib/query.scm")
(initialize-data-base microshaft-data-base)
(query-driver-loop)

; Copy and paste all the rules to the terminal to save them in the database

(assert! (rule (same-person ?x ?x)))

(assert! 
  (rule 
    (same-job ?person-1 ?person-2)
    (and
      ; Both people have the same job
      (job ?person-1 ?position)
      (job ?person-2 ?position)
      ; They are not the same person
      (not (same-person ?person-1 ?person-2))
    )
  )
)

(assert!
  (rule
    (person-can-do-job ?person-1 ?person-2)
    (and
      ; Get each persons job
      (job ?person-1 ?job-1)
      (job ?person-2 ?job-2)
      ; Chck if one can do the job of the other
      (can-do-job ?job-1 ?job-2)
    )
  )
)

(assert! 
  (rule
    (replace ?person-1 ?person-2)
    (or
      ; Either both people have the same job
      (same-job ?person-1 ?person-2)
      ; or person 1 can do the job of person 2
      (person-can-do-job ?person-1 ?person-2)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test:
;;; Query input:
(replace ?x ?y)

;;; Query results:
(replace (hacker alyssa p) (fect cy d))
(replace (aull dewitt) (warbucks oliver))
(replace (fect cy d) (hacker alyssa p))
(replace (fect cy d) (reasoner louis))
(replace (hacker alyssa p) (reasoner louis))
(replace (bitdiddle ben) (tweakit lem e))
(replace (bitdiddle ben) (fect cy d))
(replace (bitdiddle ben) (hacker alyssa p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert!
  (rule
    (replace-bigger-salary ?person-1 ?person-2 ?salary-1 ?salary-2)
    (and
      ; Check for salaries
      (salary ?person-1 ?salary-1)
      (salary ?person-2 ?salary-2)
      ; Check salary of person1 < than of person2
      (lisp-value < ?salary-1 ?salary-2)
      ; Check if person1 can replace person2
      (replace ?person-1 ?person-2)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test:
;;; Query input:
(replace-bigger-salary ?x ?y ?a ?b)

;;; Query results:
(replace-bigger-salary (aull dewitt) (warbucks oliver) 25000 150000)
(replace-bigger-salary (fect cy d) (hacker alyssa p) 35000 40000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
