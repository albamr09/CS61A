;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selectors for py-eval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bool? token) (memq token '(|True| |False|)))
(define (none? token)
  (or 
    (eq? token *NONE*)
    (eq? token '|None|)
  )
)
(define (if? token) (eq? token 'if))
(define (for? token) (eq? token 'for))
(define (while? token) (eq? token 'while))
(define (def? token) (eq? token 'def))
(define (print? token) (eq? token 'print))
(define (assignment? token line-obj)
  (and 
    ; If the token is a variable
    (variable? token)
    (not (ask line-obj 'empty?))
    ; Obtain next token
    (let ((next (ask line-obj 'peek)))
      ; If the next token is a equal sign
	    (if (eq? next '=)
	      (begin 
          (ask line-obj 'next) 
          #t
        )
	      #f
      )
    )
  )
)

(define (variable? token) (symbol? token))
; This is for functions
(define (application? token line-obj)
  ;;  handle -> fib(10)
  (and 
    ; If the current token is a variable
    (variable? token)
    (not (ask line-obj 'empty?))
    ; If the next token is an open parenthesis
    (open-paren? (ask line-obj 'peek))
  )
)

;;  dict["hello"] and list[0]
(define (bracket-dereference? token line-obj) 
  (and 
    ; If the current token is a variable
    (variable? token)
    (not (ask line-obj 'empty?))
    ; If the next token is [
    (open-bracket? (ask line-obj 'peek))
  )
)
; These are trivial
(define (return? token) (eq? token 'return))
(define (break? token) (eq? token 'break))
(define (continue? token) (eq? token 'continue))
(define (block? token) (and (pair? token) (eq? (car token) '*BLOCK*)))
(define (lambda? token) (eq? token 'lambda))
(define (import? token) (eq? token 'import))
(define (raise? token) (eq? token 'raise))
(define (not? token) (eq? token 'not))
(define (and? token) (eq? token 'and))
(define (or? token) (eq? token 'or))
(define (in? token) (eq? token 'in))
(define (is? token) (eq? token 'is))
; Check if the first character of token is a dot
(define (dotted? token) (equal? (first token) '.))
(define (open-bracket? token) (eq? token open-bracket-symbol))
(define (close-bracket? token) (eq? token close-bracket-symbol))
(define (open-brace? token) (eq? token open-brace-symbol))
(define (close-brace? token) (eq? token close-brace-symbol))
(define (open-paren? token) (eq? token open-paren-symbol))
(define (close-paren? token) (eq? token close-paren-symbol))
(define (py-primitive? proc) (ask proc 'primitive?))
(define (py-procedure? proc) (ask proc 'procedure?))
(define (unary-op? token) (memq token unary-operators))
;; List Comprehensions
(define (list-comp? seq) (memq 'for seq))
; Define helper procedure to determina if object is a list or dictionary
(define (py-list? val) (equal? (ask val 'type) 'py-list))
(define (py-dict? val) (equal? (ask val 'type) 'py-dictionary))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Infix Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define infix terms
(define infix-alist
  '(
    ; Operator, arguments, infix name
    (+ . __add__) 
    (- . __sub__) 
    (*  . __mul__)
    (/ . __div__) 
    (% . __mod__) 
    (** . __pow__)
    (> . __gt__)  
    (>= . __ge__) 
    (== . __eq__)
    (< . __lt__)  
    (<= . __le__) 
    (!= . __ne__)
  )
)

;; Infix selectors
; Obtain operators: +, -, etc
(define infix-operators (map car infix-alist))
; Check if the token is an infix operator
(define (infix-op? token) (memq token infix-operators))
; Search element in infix-alist whose key equals op (+, -, etc)
(define (lookup-op op) (cdr (assq op infix-alist)))

; Remove leading ., in dotted tokens (e.g. self.test -> test) 
(define (remove-dot token)
  (let 
    ((tail (butfirst token)))
    (if (string? tail)
	    (string->symbol tail)
	    tail
    )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROCEDURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constructors
(define (make-def-block line-obj env)
  (let 
    (
      ; Obtain procedure's name
      (name (ask line-obj 'next))
      ; Obtain parameters of procedure
	    (params 
        (begin 
          (ask line-obj 'next) 
          (collect-params line-obj env)
        )
      )
    )
    (if 
      (or 
        (ask line-obj 'empty?)
	      (not (eq? (ask line-obj 'next) ':))
      )
      ; Check if the procedure is not syntactically correct
	    (py-error "SyntaxError: Expected \":\"")
      ; Else create procedure
	    (let ((body (read-block (ask line-obj 'indentation) env)))
        ; Signal this is a block procedure (a define)
	      (list '*BLOCK* '*DEF-BLOCK* (cons name params) body)
      )
    )
  )
)

;; Apply a procedure
(define (py-apply proc args)
  (cond 
    ; Check if this is a primitive procedure (we call the __call__ method inside the primitive class)
    ((py-primitive? proc) (ask proc '__call__ args))
    ; Check if this is a defined procedure (same as before but for the procedure class)
	  ((py-procedure? proc) (ask proc '__call__ args))
    ; If not error
	  (else 
      (py-error "TypeError: cannot call objects of type: "
			(ask proc 'type))
    )
  )
)

;; Selectors
(define (def-block-name block) (caaddr block))
(define (def-block-params block) (cdaddr block))
(define (def-block-body block) (cadddr block))

; Obtain parameters of procedure
(define (collect-params line-obj env)
  ; If there are not closing parenthesis: error
  (if (ask line-obj 'empty?)
    (py-error "SyntaxError: Expected \")\"")
    ; Obtain next token
    (let ((token (ask line-obj 'next)))
	    (cond 
        ; If ) -> return emtpy list
        ((eq? token close-paren-symbol) '())
        ; If , -> call recursively to obtain next parameter
	      ((comma? token) (collect-params line-obj env))
        ; If =, error
	      ((eq? (ask line-obj 'peek) '=)
	        (py-error "ExpertError: Default Parameters")
        )
        ; It none of the above, keep going recursively
	      (else 
          (cons token (collect-params line-obj env))
        )
      )
    )
  )
)

; Handle expressions with only one operand: !x, -x, etc
(define (apply-unary op val)
  (cond 
    ; negate -> create numeric python object whose value is -value
    ((eq? op '-) 
     (make-py-num (- (ask val 'val)))
    )
    ; opposite -> create boolean python object whose value is !value
	  ((eq? op 'not) 
     (make-py-bool (not (ask val 'val))) ;; handles "not x"
    ) 
    ; If the opearator is none of the above
	  (else (py-error "SyntaxError: Unrecognized operator: " op))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BLOCK 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A block is of the form:
; (BLOCK BLOCK-TYPE BLOCK-CONTENT)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructor
(define (make-block line-obj env)
  ; Obtain the type
  (let ((type (ask line-obj 'next)))
    ; Create the list with the format above presented
    (cond 
      ((eq? type 'if) (list '*DUMMY-INDENT* (make-if-block line-obj env)))
	    ((eq? type 'for) (list '*DUMMY-INDENT* (make-for-block line-obj env)))
	    ((eq? type 'def) (list '*DUMMY-INDENT* (make-def-block line-obj env)))
	    ((eq? type 'elif) (list '*DUMMY-INDENT* (make-elif-block line-obj env)))
	    ((eq? type 'else) (list '*DUMMY-INDENT* (make-else-block line-obj env)))
	    ((eq? type 'while) (list '*DUMMY-INDENT* (make-while-block line-obj env)))
      ; If the type is none of the above: error
	    (else (py-error "SyntaxError: unknown keyword: " type))
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper constructors

;; Conditionals
(define (make-if-block line-obj env)
  (py-error "TodoError: Person B, Question 7"))
(define (if-block-pred block)
  (py-error "TodoError: Person B, Question 7"))
(define (if-block-body block)
  (py-error "TodoError: Person B, Question 7"))
(define (if-block-else block)
  (py-error "TodoError: Person B, Question 7"))


;; Elif blocks
(define (make-elif-block line-obj env)
  (py-error "TodoError: Person B, Question 7"))
(define (elif-block-pred block) (py-error "TodoError: Person B, Question 7"))
(define (elif-block-body block) (py-error "TodoError: Person B, Question 7"))
(define (elif-block-else block) (py-error "TodoError: Person B, Question 7"))

;; Else blocks
(define (make-else-block line-obj env)
  (if 
    (not 
      (and 
        (not (ask line-obj 'empty?))
		    (eq? (ask line-obj 'next) ':)
		    (ask line-obj 'empty?)
      )
    )
    (py-error "SyntaxError: invalid syntax")
    (let ((body (read-block (ask line-obj 'indentation) env)))
      ; Create else-block
	    (list '*BLOCK* '*ELSE-BLOCK* (split-block body))
    )
  )
)
(define (else-block-body block) (caaddr block))
(define (else-block-else block) (py-error "SyntaxError: too many else clauses"))


;; While loops
;; While block is of the form (*BLOCK* *WHILE-BLOCK* (pred body) else)
(define (make-while-block line-obj env)
	(let 
    (
      ; Obtain predicate
	    (predicate (collect-predicate line-obj env))
    ) 
    ; Check for : symbol
    (if (not (ask line-obj 'empty?))
      (py-error "SyntaxError: invalid syntax")
      (let
        ; Obtain body
        ((body (read-block (ask line-obj 'indentation) env)))
        (list '*BLOCK* '*WHILE-BLOCK* (cons predicate body))
        ; Check for else
        ; (if (else?)
        ;   (let
        ;     ((else-block (read-block (ask line-obj 'indentation) env))))
        ;     ; If there is else block: create block with all parts
	      ;     (list '*BLOCK* '*WHILE-BLOCK* (cons pred body) else-block)
        ;   )
        ;   ; If not else, return block without it
	      ;   (list '*BLOCK* '*WHILE-BLOCK* (cons pred body))
        ; )
      )
    )
  )
)

; Obtain parameters of procedure
(define (collect-predicate line-obj env)
  ; If there are not finish :
  (if (ask line-obj 'empty?)
    (py-error "SyntaxError: Expected \":\"")
    ; Obtain next token
    (let ((token (ask line-obj 'next)))
      ; Is it equal to :
      (show token)
      (if (colon? token)
        ; Finish
        nil
        ; Continue reading line recursively (save token to list)
        (cons token (collect-predicate line-obj env))
      )
    )
  )
)

(trace collect-predicate)

(define (while-block-pred block)
  ;; Attending to the shape of a while block
  ;; Return first element inside third element in block
  (caaddr block)
)

(define (while-block-body block)
  ;; Attending to the shape of a while block
  ;; Return second element inside third element in block
  (cdaddr block)
)

(define (while-block-else block)
  ;; Attending to the shape of a while block
  ;; Return fourth element
  (cadddr block)
)

;; For loops
(define (make-for-block line-obj env)
  (py-error "TodoError: Person A, Question 7"))
(define (for-block-var block)
  (py-error "TodoError: Person A, Question 7"))
(define (for-block-collection block)
  (py-error "TodoError: Person A, Question 7"))
(define (for-block-body block)
  (py-error "TodoError: Person A, Question 7"))
(define (for-block-else block)
  (py-error "TodoError: Person A, Question 7"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Selectors
(define (block-body block) (cdr block))
(define (block-type block) (cadr block))

; Check block-type
(define (if-block? block) (eq? (block-type block) '*IF-BLOCK*))
(define (def-block? block) (eq? (block-type block) '*DEF-BLOCK*))
(define (for-block? block) (eq? (block-type block) '*FOR-BLOCK*))
(define (elif-block? block) (eq? (block-type block) '*ELIF-BLOCK*))
(define (else-block? block) (eq? (block-type block) '*ELSE-BLOCK*))
(define (while-block? block) (eq? (block-type block) '*WHILE-BLOCK*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Block evaluation
(define (eval-block block env)
  ; Evaluate each different type of block differently
  (cond 
    ((if-block? block) (eval-if-block block env))
	  ((def-block? block) (eval-def-block block env))
	  ((for-block? block) (eval-for-block block env))
	  ((elif-block? block) (eval-elif-block block env))
	  ((else-block? block) (eval-else-block block env))
	  ((while-block? block) (eval-while-block block env))
    ; If the type is none of the listed throw error
	  (else (py-error "SyntaxError: bad block type: " (block-type block)))
  )
)

; Split if-elif-else block into list of if block, elif blocks, else block
(define (split-block block)
  ;; Split block takes a list of lines and checks if the last line is a else
  ;; or elif block.  If so, it returns a pair whose car is a list of all but
  ;; that last line and whose cdr is the elif or else block.  If there is no
  ;; such trailing elif or else block, it returns (cons block #f).
  (define (iter so-far to-go)
    (let 
      (
        (current-line (tokens (car to-go)))
	      (rest (cdr to-go))
      )
      ; If there are not more block after this one
      (if (null? rest)
	      (if 
          ; Is current-line a block (concretely a elif or else block)
          (and 
            (block? (car current-line))
		        (or 
              (elif-block? (car current-line))
		          (else-block? (car current-line))
            )
          )
	        (cons 
            ; reverse the list of blocks
            (reverse so-far) 
            ; Append the current-line
            (car to-go)
          )
          ; if not
	        (cons 
            ; reverse all because we append the last block to the start of the list
            (reverse 
              (cons 
                (car to-go) 
                so-far
              )
            ) 
            #f
          )
        )
        ; If there are more blocks after this one, keep going
	      (iter 
          ; Update list of blocks: note we are appending to the start
          ; so when we finish we have to reverse
          (cons (car to-go) so-far) 
          rest
        )
      )
    )
  )
  ; Start splitting
  (iter '() block)
)


;; List Access
(define (get-slice line-obj env)
  ;; only handles [i], [i:j], and slices, not [:j], [i:], or [::k]
  (let ((val (py-eval line-obj env)))
    (cond 
      ; If the next token is a close bracket ]
      ((eq? (ask line-obj 'peek) close-bracket-symbol)
	      (ask line-obj 'next) ;; get rid of ] token
	      (list val)
      )
      ; If the next token is a :
	    ((eq? (ask line-obj 'peek) ':)
	      (ask line-obj 'next) ;; get rid of : token
	      (cons val (get-slice line-obj env))
      )
	    (else 
        (py-error 
          "SyntaxError: Expected \"]\", encountered "
			    (ask line-obj 'next)
        )
      )
    )
  )
)

;; List comprehensions should work as follows:
;;   >>> myList = [1,2,3]
;;   >>> [3*x for x in myList]
;;   [3,6,9]
;;   >>> [i + j for i in "abc" for j in "def"]
;;   ["ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"]
;;   >>> [i*j for j in range(10) if j % 2 == 0 for i in "SICP"]
;;   ["SS", "II", "CC", "PP", "SSSS", "IIII", "CCCC", "PPPP"]
