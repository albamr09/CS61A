;  Define a rule that says that a person is a “big
; shot” in a division if the person works in the division but
; does not have a supervisor who works in the division.

; This initializes the interpreter
(load "../../../lib/query.scm")
(initialize-data-base microshaft-data-base)
(query-driver-loop)

; Copy and paste this to the terminal to save the rule in the database
(assert!
  (rule
    (big-shot ?person)
    (and
      ; Check person works in division
      (job ?person (?division . ?type))
      (or
        ; Either it has a supervisor but the supervisor
        ; does not work in the same department
        (and
          (supervisor ?person ?boss)
          (not (job ?boss (?division . ?other-type)))
        )
        ; or it does not have a supervisor
        (not (supervisor ?person ?boss))
      )
    )
  )
)

; Test: copy this in the terminal
(big-shot ?x)
