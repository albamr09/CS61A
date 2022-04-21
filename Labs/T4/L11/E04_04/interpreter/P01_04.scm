;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVALUATOR AS A PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;
;; Represent primitive procedures

; Check if the first element of the expresion is "primitive"
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive)
)

; The primitive function is the second element of the expression
(define (primitive-implementation proc) (cadr proc))

; setup-environment will get the primitive names and implementation
; procedures from a list

(define primitive-procedures
  (list 
    (list 'car car)
    (list 'cdr cdr)
    (list 'cons cons)
    (list 'exit exit)
    (list 'null? null?)
    ;; E01_03: added primitive procedures
    (list '+ +)
    (list '- -)
    (list '* *)
    (list '/ /)
    ;; E04_04: added primitive procedures
    (list '= =)
  )
)

; Obtain the names of the procedures
(define (primitive-procedure-names)
  (map car primitive-procedures)
)

; Obtain the names of the reference to the procedures
(define (primitive-procedure-objects)
  (map 
    (lambda 
      (proc) 
      (list 'primitive (cadr proc))
    )
    primitive-procedures
  )
)

; We thus set up a global environment that
; associates unique objects with the names of the primitive procedures
; that can appear in the expressions we will be evaluating. e global
; environment also includes bindings for the symbols true and false

(define (setup-environment)
  (let 
    ((initial-env
        (extend-environment 
          ; Bind the primitive procedures: name to object
          (primitive-procedure-names)
          (primitive-procedure-objects)
          ; Bind them with the empty environment as the enclosing env
          the-empty-environment
        )
    ))
    ; Bindings for true and false
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    ; Return environment
    initial-env
  )
)

; Create global environment
(define the-global-environment (setup-environment))

; To apply a primitive procedure, we simply apply the implementation
; procedure to the arguments

; Abstraction of the apply procedure
(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RUNNING THE INTERPRETER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  ; Enter input
  (prompt-for-input input-prompt)
  (let 
    ; Read input
    ((input (read)))
    (let 
      ; Evaluate expression
      ((output (eval input the-global-environment)))
      ; Print output
      (announce-output output-prompt)
      (user-print output)
    )
  )
  (driver-loop)
)


;;; Printing procedures
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline)
)

(define (announce-output string)
  (newline) (display string) (newline)
)

; special printing procedure, user-print, to avoid printing the
; environment part of a compound procedure, which may be a very long
; list

(define (user-print object)
  (if (compound-procedure? object)
    ; If it is a procedure, display its parts
    (display 
      (list 
        'compound-procedure
        (procedure-parameters object)
        (procedure-body object)
        '<procedure-env>
      )
    )
    ; Else display the complete object
    (display object)
  )
)

