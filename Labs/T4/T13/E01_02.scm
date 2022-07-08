; Define a rule that says that person 1 can replace person 2 if either person 1 does the same job as person
; 2 or someone who does person 1’s job can also do person 2’s job, and if person 1 and person 2 
; are not the same person. Using your rule, give queries that find the following:

; a. all people who can replace Cy D. Fect;

; b. all people who can replace someone who is being paid more than they are, together with the two salaries.

; Start
(load "../../../lib/query.scm")
(initialize-data-base microshaft-data-base)
(query-driver-loop)

(assert! (rule (same-person ?x ?x)))
(assert! 
  (rule 
    (same-job ?person-1 ?person-2)
    (and
      (job ?person-1 ?position)
      (job ?person-2 ?position)
      (not (same-person ?person-1 ?person-2))
    )
  )
)

(assert!
  (rule
    (person-can-do-job ?person-1 ?person-2)
    (and
      (job ?person-1 ?job-1)
      (job ?person-2 ?job-2)
      (can-do-job ?job-1 ?job-2)
    )
  )
)

(assert! 
  (rule
    (replace ?person-1 ?person-2)
    (or
      (same-job ?person-1 ?person-2)
      (person-can-do-job ?person-1 ?person-2)
    )
  )
)
