;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA STRUCTURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Load expressions

;; TESTING OF PREDICATES

; For conditionals, we accept anything to be true that is not the explicit
; false object.

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

;; REPRESENTING PROCEDURES

; Create the procedure by appending:
; - tag = procedure
; - formal parameters: parameters
; - body
; - env: current environment
(define (make-procedure parameters body env)
  (list 'procedure parameters body env)
)

; Check if the first element is "procedure"
(define (compound-procedure? p)
  (tagged-list? p 'procedure)
)

;;; E02: Now the formal parameters can be a list 
;;; with the predicate the parameter has to satisfy
;;; or a single element
;;; example 
;;; (define (foo (number? x) (number? y)) x)
;;; the parameters are ((number? x) (number? y))

; The parameters are the second element 
; map to obtain only the parameters (second element), not the
; predicate
(define (procedure-parameters p) 
  (map  
    (lambda
      (parameter)
      (if (pair? parameter)
        (cadr parameter)
        parameter
      )
    )
    (cadr p)
  )
)
; The parameters are the second element
; map to obtain only the predicate (first element), not the
; parameters
(define (procedure-predicates p) 
  (map  
    (lambda
      (parameter)
      (if (pair? parameter)
        ; Obtain the predicate
        (car parameter)
        ; Return nothing
        '()
      )
    )
    (cadr p)
  )
)

; The body is the third element
(define (procedure-body p) (caddr p))
; The environment is the fourth element
(define (procedure-environment p) (cadddr p))

;; ENVIRONMENTS

; We represent an environment as a list of frames.

; The enclosing environment cdr of the list
(define (enclosing-environment env) (cdr env))
; The first frame is the car of the list 
(define (first-frame env) (car env))
; The empty environment is an empty list
(define the-empty-environment '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E01_05
; Change the representation of the enviroment from a pair of lists 
; where one are the variables and the other are the values binded
; to a list of pairs of variables and valus

; Each frame of an environment is represented as a list of pairs
; of variables with its value

(define (make-frame variables values)
  ; Create a list of pairs variable-value
  (map
    (lambda
      (variable value)
      (cons variable value)
    )
    variables values
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E01_05
; The variables of the frame are the first element of each pair in the frame
(define (frame-variables frame) (map car frame))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E01_05
; The variables of the frame are the second element of each pair in the frame
(define (frame-values frame) (map cadr frame))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E01_05
; Now we only need to append a pair to the list of
; pairs in the frame
(define (add-binding-to-frame! var val frame)
  ; Add a pair to the end of the list of pairs
  (set-cdr!
    frame
    (cons
      ; Add pair to the start of the cdr of the frame
      (cons var val)
      (cdr frame)
    )
  )
)

; To extend an environment by a new frame that associates variables with
; values, we make a frame consisting of the list of variables and the list
; of values, and we adjoin this to the environment. We signal an error if
; the number of variables does not match the number of values

(define (extend-environment vars vals base-env)
  ; If number of variables = number of values
  (if (= (length vars) (length vals))
    ; Add a frame to the front of the list of frames=base-env
    (cons 
      ; Create the frame from the list of variables and values
      (make-frame vars vals) 
      base-env
    )
    ; Else throw error (check which kind of error to throw)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals)
    )
  )
)


; To look up a variable in an environment, we scan the list of variables
; in the first frame. If we find the desired variable, we return the corresponding 
; element in the list of values. If we do not find the variable in the current frame, 
; we search the enclosing environment, and so on.

; If we reach the empty environment, we signal an “unbound variable”
; error.

(define (lookup-variable-value var env)
  (define (env-loop env)
    ; If the environment is the empty environment, return error
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      ; Else, search for the variable
      (let 
        ; Get the current frame
        ((frame (first-frame env)))
        ; Check if the variable is in the frame
        (let
          ((bound-variable (assoc var frame)))
          ; If the variable exists
          (if bound-variable
            ; Return the value
            (cdr bound-variable)
            ; Else keep searching in the next frame
            (env-loop (enclosing-environment env))
          )
        ) 
      )
    )
  )
  (env-loop env)
)

; To set a variable to a new value in a specified environment, we scan for
; the variable, just as in lookup-variable-value, and change the corresponding 
; value when we find it.

(define (set-variable-value! var val env)
  (define (env-loop env)
    ; If the environment is the empty environment, return error
    (if (eq? env the-empty-environment)
      (error "Unbound variable: SET!" var)
      ; Else, search for the variable
      (let 
        ; Get the current frame
        ((frame (first-frame env)))
        ; Check if the variable is in the frame
        (let
          ((bound-variable (assoc var frame)))
          ; If the variable exists
          (if bound-variable
            ; Update the value of the variable
            (set-cdr! bound-variable val)
            ; Else keep searching in the next frame
            (env-loop (enclosing-environment env))
          )
        ) 
      )
    )
  )
  (env-loop env)
)

; To define a variable, we search the first frame for a binding for the
; variable, and change the binding if it exists (just as in set-variable-value!). 
; If no such binding exists, we adjoin one to the first frame.

(define (define-variable! var val env)
  (let 
    ((frame (first-frame env)))
    (let
      ; Check if the variable exists in the frame
      ((bound-variable (assoc var frame)))
      ; If the variable exists
      (if bound-variable
        ; Update the value of the variable
        (set-cdr! bound-variable val)
        ; Create a binding for that variable and value in the frame
        (add-binding-to-frame! var val frame)
      )
    ) 
  )
)
