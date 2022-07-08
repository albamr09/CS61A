; Formulate compound queries that retrieve the following information:

; Start
(load "../../../lib/query.scm")
(initialize-data-base microshaft-data-base)
(query-driver-loop)

; a. the names of all people who are supervised by Ben Bitdiddle, together with their addresses;

(and
 (supervisor ?x (Bitdiddle Ben))
 (address ?x ?y)
)

; b. all people whose salary is less than Ben Bitdiddle’s, together with their salary and Ben Bitdiddle’s salary

(and 
  (salary (bitdiddle ben) ?ben-salary)
  (salary ?person ?salary)
  (lisp-value > ?salary ?ben-salary)
)

; c. all people who are supervised by someone who is not in the computer division, together with the supervisor’s name and job.

(and
  (not 
    (and
      (supervisor ?x ?y)
      (job ?y ?x)
      (not (job ?y (computer . ?type)))
    )
  )
  (job ?x ?z)
)
