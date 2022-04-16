;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA STRUCTURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

; The parameters are the second element
(define (procedure-parameters p) (cadr p))
; The body is the third element
(define (procedure-body p) (caddr p))
; The environment is the fourth element
(define (procedure-environment p) (cadddr p))

;; ENVIRONMENTS

; We represent an environment as a list of frames.

; The enclosing environment cdr of the list
define (enclosing-environment env) (cdr env))
; The first frame is the car of the list 
(define (first-frame env) (car env))
; The empty environment is an empty list
(define the-empty-environment '())

; Each frame of an environment is represented as a pair of lists: 
; - a list of the variables bound in that frame 
; - a list of the associated values

(define (make-frame variables values)
  ; Create a list of the list of variables and the list of values
  (cons variables values)
)

; The first element of the frame is the list of variables
(define (frame-variables frame) (car frame))
; The second element of the frame is the list of values
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  ; Update the variable
  (set-car! 
    frame 
    ; Add variable to front of list of variables
    (cons 
      var 
      ; List of variables
      (car frame)
    )
  )
  ; Update the value
  (set-cdr! 
    frame 
    ; Add value to front of list of values
    (cons 
      val 
      ; List of values
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
    (define (scan vars vals)
      (cond 
        ; If there are no more variables bound in the current environment
        ((null? vars) 
          ; Check in the enclosing-environment
          (env-loop (enclosing-environment env))
        )
        ; If the variable we are searching for equals the current
        ; variable we are scanning in the environment, return the 
        ; value bounded
        ((eq? var (car vars)) (car vals))
        ; Else keep searching in the rest of the (variables, values) in the
        ; current environment
        (else (scan (cdr vars) (cdr vals))))
    )
    ; If the environment is the empty environment, return error
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      ; Else, search for the variable
      (let 
        ; Get the current frame
        ((frame (first-frame env)))
        ; Check if the variable is in the frame
        (scan 
          ; Variables bound in the current frame
          (frame-variables frame)
          ; Value bound in the current frame
          (frame-values frame)
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
    (define (scan vars vals)
      (cond 
        ; If there are no more variables bound in the current environment
        ((null? vars) 
          ; Check in the enclosing-environment
          (env-loop (enclosing-environment env))
        )
        ; If the variable we are searching for equals the current
        ; variable we are scanning in the environment, change its value
        ; to the new value val
        ((eq? var (car vars)) 
          (set-car! vals val)
        )
        ; Else keep searching in the rest of the (variables, values) in the
        ; current environment
        (else (scan (cdr vars) (cdr vals)))
      )
    )
    ; If the environment is the empty environment, return error
    (if (eq? env the-empty-environment)
      (error "Unbound variable: SET!" var)
      ; Else, search for the variable
      (let 
        ; Get the current frame
        ((frame (first-frame env)))
        ; Check if the variable is in the frame
        (scan 
          ; Variables bound in the current frame
          (frame-variables frame)
          ; Values bound in the current frame
          (frame-values frame)
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
    (define (scan vars vals)
      (cond 
        ; If there are no more variables bound in the current environment
        ((null? vars)
          ; Create a binding for that variable and value in the frame
          (add-binding-to-frame! var val frame)
        )
        ; If the variable already exists in the frame, change its value
        ((eq? var (car vars)) (set-car! vals val))
        ; Else keep searching in the variables and values defined in the frame
        (else (scan (cdr vars) (cdr vals))))
    )
    ; Start the scan through the variable and values of the frame=env
    (scan (frame-variables frame) (frame-values frame))
  )
)
