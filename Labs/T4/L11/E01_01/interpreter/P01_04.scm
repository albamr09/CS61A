;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVALUATOR AS A PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Load interpreter
(load "../../../../BProblems/T4/P01_04.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RUNNING THE INTERPRETER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Override driver loop to use dispatch-eval instead of eval
(define (driver-loop)
  ; Enter input
  (prompt-for-input input-prompt)
  (let 
    ; Read input
    ((input (read)))
    (let 
      ; Evaluate expression
      ((output (dispatch-eval input the-global-environment)))
      ; Print output
      (announce-output output-prompt)
      (user-print output)
    )
  )
  (driver-loop)
)

