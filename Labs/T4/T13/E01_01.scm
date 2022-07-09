; Formulate compound queries that retrieve the following information:

; This initializes the interpreter
(load "../../../lib/query.scm")
(initialize-data-base microshaft-data-base)
(query-driver-loop)

; a. the names of all people who are supervised by Ben Bitdiddle, together with their addresses;

; Copy and paste this to the terminal to save the rule in the database
(and
 (supervisor ?x (Bitdiddle Ben))
 (address ?x ?y)
)

; b. all people whose salary is less than Ben Bitdiddle’s, together with their salary and Ben Bitdiddle’s salary

; Copy and paste this to the terminal to save the rule in the database
(and 
  (salary (bitdiddle ben) ?ben-salary)
  (salary ?person ?salary)
  (lisp-value > ?salary ?ben-salary)
)

; c. all people who are supervised by someone who is not in the computer division, together with the supervisor’s name and job.

; Copy and paste this to the terminal to save the rule in the database
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
