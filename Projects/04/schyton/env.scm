;; Variables and Assignment: taken mostly from Abelson and Sussman's

;; Metacircular Evaluator (SICP, Chapter 4)

; Helper method: create env with nothing in it
(define the-empty-environment '())
; Helper method: define the global environment as the empty environment
(define the-global-environment the-empty-environment)

(define (enclosing-environment env) (cdr env))

; A frame is made up from two lists (variables) and (values) equal in lenght
; where each element in variables is associated to and element in values
(define (make-frame variables values)
  (cons variables values)
)
; First frame = first element in the list "env"
(define (first-frame env) (car env))
; The variables of a frame is the first list
(define (frame-variables frame) (car frame))
; The values of a frame is the second list
(define (frame-values frame) (cdr frame))

; Create new pair (variable) (value) in a given frame
(define (add-binding-to-frame! var val frame)
  ; Update variables list
  (set-car! frame (cons var (car frame)))
  ; Update values list
  (set-cdr! frame (cons val (cdr frame)))
)

; Add a new frame to the base environment (this will act as the enclosing environment)
(define (extend-environment vars vals base-env)
  ; Check if the numbero of variables and the number of values are the same
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (py-error "Too many arguments supplied " vars " " vals)
      (py-error "Too few arguments supplied " vars " " vals)
    )
  )
)

; Search for the value of a variable in an environment
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond 
        ; If there are no more variables to search
        ((null? vars)
          ; Start searching in enclosing environment
          (env-loop (enclosing-environment env))
        )
        ; If the current var is the one we are searching
        ((eq? var (car vars))
          ; Return the associated value
          (car vals)
        )
        ; Else keep searching in this frame
        (else (scan (cdr vars) (cdr vals)))
      )
    )
    ; If the current env the empty environment, then return false
    (if (eq? env the-empty-environment)
	    #f
      ; Else search for it: from the first frame
      (let ((frame (first-frame env)))
        (scan 
          (frame-variables frame)
          (frame-values frame)
        )
      )
    )
  )
  ; Start searching
  (env-loop env)
)

; Update the value of a variable
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond 
        ; If there are no more variables
        ((null? vars)
          ; Continue searching in the next environment
          (env-loop (enclosing-environment env))
        )
        ; If the variable is the one we are going to update
        ((eq? var (car vars))
          ; Update the value of the variable
          (set-car! vals val)
        )
        ; Else keep searching in the rest of the frame
        (else (scan (cdr vars) (cdr vals)))
      )
    )
    ; If the current environment is emtpy throw error
    (if (eq? env the-empty-environment)
      (py-error "NameError: Unbound variable " var)
      ; Else search in the variables of the current frame
      (let ((frame (first-frame env)))
        (scan 
          (frame-variables frame)
          (frame-values frame)
        )
      )
    )
  )
  ; Start searching for the variable
  (env-loop env)
  *NONE*
)

; Create new variable and add it to the environment
(define (define-variable! var val env)
  ; Obtain the first frame of environment
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond 
        ; If there are no more variables
        ((null? vars)
          ; Create a new variable in the frame
          (add-binding-to-frame! var val frame)
        )
        ; If this is the variable we want to update
        ((eq? var (car vars))
          ; Update its value
          (set-car! vals val)
        )
        ; Else keep searching
        (else (scan (cdr vars) (cdr vals)))
      )
    )
    ; Search for the variable in the current frame
    (scan 
      (frame-variables frame)
      (frame-values frame)
    )
  )
  *NONE*
)

; Initialize the python env
(define (initialize-python)
  ; Create the global environment with no variables-values and no enclosing environment
  (set! the-global-environment (extend-environment '() '() '()))
  (define-variable! 
    '__name__
    (make-py-string "__main__")
    the-global-environment
  )
  ; Create primitive methods
  (define-primitives!)
  ; Start interpreter
  (driver-loop)
)
