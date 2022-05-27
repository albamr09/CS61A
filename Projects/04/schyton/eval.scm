;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that the line has zero indentation then check nothing is left over
;; after sending the line to py-eval.

(define (eval-line line-obj env)
	; If it is empty object
  (if (ask line-obj 'empty?)
		; Return nothing
    *NONE*
		; If the line has no indentation
    (if (zero? (ask line-obj 'indentation))
			(let 
				; Obtain value after evaluation
				((val (py-eval line-obj env)))
				; If there are elements left on the line-obj after evaluating the tokens
	  	  (if (not (ask line-obj 'empty?))
					; Error
					(py-error "SyntaxError: multiple statements on one line")
					; Else return the value result of evaluation
					val
				)
			)
			; If there is indentation error
	  	(py-error "IndentationError: unexpected indent")
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Starts the infix/item evaluator loop

(define (py-eval line-obj env)
	; Wrapper to handle infix
  (handle-infix 
		; Evaluate item
		(eval-item line-obj env) 
		line-obj 
		env
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculates the first python object on the line-obj.

(define (eval-item line-obj env)
  (if (ask line-obj 'empty?)
    *NONE*
		(let 
			((token (ask line-obj 'next)))
			(cond 
				; String expression
				((string? token) (make-py-string token))
				; Number expression
			  ((number? token) (make-py-num token))
				; Boolean expression
			  ((bool? token) (make-py-bool token))
				; None expression
			  ((none? token) *NONE*)
				; Unary operator
			  ((unary-op? token) (apply-unary token (eval-item line-obj env)))
				; Block expression
			  ((block? token) (eval-block token env))
				; If expression
			  ((if? token)
			    (let 
						((block (make-if-block line-obj env)))
						(ask line-obj 'push block)
						(py-eval line-obj env)
					)
				)
				; For loop expression
			  ((for? token)
			    (let 
						((block (make-for-block line-obj env)))
						(ask line-obj 'push block)
				 		(py-eval line-obj env)
					)
				)
				; While loop expression
			  ((while? token)
					(let 
						((block (make-while-block line-obj env)))
  		      (ask line-obj 'push block)
  		      (py-eval line-obj env)
					)
				)
				; Define expression
			  ((def? token)
					(let 
					 ((block (make-def-block line-obj env)))
  		      (ask line-obj 'push block)
  		      (py-eval line-obj env)
					)
				)
				; Open parenthesis
			  ((open-paren? token)
			    (let 
						((sub (collect-sequence line-obj env close-paren-symbol)))
						(if (null? (cdr sub))
							(car sub)
							(py-error "SyntaxError: tuples not implemented")
						)
					 )
				)
				; Print expression
			  ((print? token) (py-print (py-eval line-obj env)))
				; Return expression
			  ((return? token)
					(cons 
						'*RETURN*
						(py-eval line-obj env)
					)
				)
				; Break expression
			  ((break? token) '*BREAK*)
				; Continue expression
			  ((continue? token) '*CONTINUE*)
				; Lambda expression
			  ((lambda? token) (eval-lambda line-obj env))
				; Import expression
			  ((import? token) (eval-import line-obj))
				; Raise expression
			  ((raise? token) (eval-raise line-obj env))
				; Bracket
			  ((open-bracket? token)
					; If the first element is a for
					(if (memq 'for (ask line-obj 'tokens))
						; Evaluate comprehensive list
						(eval-list-comp line-obj env)
						; Else create normal list
						(make-py-list
							(collect-sequence line-obj env close-bracket-symbol)
						)
					)
				)
				; Brace: create dictionary
				((open-brace? token)
  		    (make-py-dictionary
						(collect-key-value line-obj env close-brace-symbol)
					)
				)
  		  ;;handle both value dereferences and value assignments of lists and dictionaries. 
				; This breaks the handle-infix model for assignments but is the cleanest way to solve the lookahead problem
  		  ((bracket-dereference? token line-obj) ;;(dict['hello']) or (list[0]) or (list[x] = y)
					; Obtain the object associated with the token(=variable)
  		    (let ((val (lookup-variable-value token env)))
						;; remove '[' token
  		      (ask line-obj 'next) 
  		      (define key #f)
  		      (cond
							; Is it a list
  		        ((py-list? val)
								;;get the list slice
  		          (set! 
									key 
									(get-slice line-obj env)
								)
							) 
							; Is it a dictionary
  		        ((py-dict? val)
								;;get the dictionary key
								(set! 
									key 
									(eval-inside-delimiters line-obj env open-bracket-symbol close-bracket-symbol)
								)
							)
							; Else, throw error
  		        (else 
								(print (ask val 'type))
  		          (print val)
  		          (print (py-list? val))
  		          (print (py-dict? val))
  		          (py-error "token not subscriptable")
							)
						)
  		      (if 
							; If it is not the end of line and the next token is =
							(and 
								(not (ask line-obj 'empty?))
  		          (eq? (ask line-obj 'peek) '=)
							)
  		        (begin 
								;; remove '=' token
								(ask line-obj 'next) 
								;;set item in dict or list
  		          (ask val '__setitem__ key (py-eval line-obj env))
							) 
							; Else simply return the value associated with the key
  		        (ask val '__getitem__ key)
						)
					)
				)
				; Is it an assignment expression
			  ((assignment? token line-obj)
					; Define variable on environment
			    (define-variable! token (py-eval line-obj env) env)
			    *NONE*
				)
				;;application? must come before variable? because both applications and variables start with strings: i.e: foo and foo()
  		  ((application? token line-obj) 
					;variable name, i.e, fib in fib()
  		    (let ((func (lookup-variable-value token env))) 
  		      (eval-func func line-obj env)
					)
				)
  		  ((variable? token)
					;variable lookup
					(let ((val (lookup-variable-value token env)))
						; If we find variable
						(if val 
							; Return
							val 
							; Else throw error
							(py-error "NameError: Unbound variable: " token)
						)
					)
				) 
				; If none of the above we cannot evaluate
			  (else 
					; Throw error
					(py-error "SyntaxError: Unrecognized token: " token)
				)
			)
		)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accepts a line-obj with opening delimiter removed
;; returns evaluating inside something, removes the closing delimiter from line-obj

(define (eval-inside-delimiters line-obj env open-delim close-delim)
  ;; hanlde both [ x y x ] and dict[dict1[dict2[x]]]
  ;; - count keeps track of balance of braces
  ;; collect the tokens inside two delimiters
  (define (collect line-obj count)
		; No open braces
    (if (= count 0)
			; Return nothing
      '()
			; Else keep looking for closing
      (let ((t (ask line-obj 'next)))
        (cond
					; Close delimiter
          ((eq? t close-delim)
						; Add token to list and update open delimiter counter
            (cons t (collect line-obj (- count 1)))
					)
					; Open delimiter
          ((eq? t open-delim)
						; Add token to list and update open delimiter counter
            (cons t (collect line-obj (+ count 1)))
					)
					; Else add token to list and do not update open delimiter counter
          (else (cons t (collect line-obj count)))
				)
			)
		)
	)

  (let* 
		(
			; Obtain tokens inside delimiters
			(inner-tokens (collect line-obj 1))
			; Create dummy line with tokens inside delimiters
			(inside-line (make-line-obj (cons '*DUMMY-INDENT* inner-tokens)))
		)
		; Evalute this dummy line
    (py-eval inside-line env)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;evaluate function calls

(define (eval-func func line-obj env)
	; eats the open paren
  (ask line-obj 'next) 
	; Apply function over arguments
  (py-apply 
		func 
		; Get arguments
		(collect-sequence line-obj env close-paren-symbol)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluates a block, line-by-line

(define (eval-sequence block env)
	; If block is empty
  (if (null? block)
		; Return nothing
    *NONE*
		; Obtain next line
    (let ((line-obj (make-line-obj (car block))))
			; Evalue line
			(let ((val (py-eval line-obj env)))
				; If it is not empty after evaluation
			  (if (not (ask line-obj 'empty?))
					; Error
			    (py-error "SyntaxError: Too many tokens on one line")
					; Else
			    (cond 
						; If it is a return, return the value
						((and (pair? val) (eq? (car val) '*RETURN*)) val)
						; If it is a break or continue, return the value
						((memq val '(*BREAK* *CONTINUE*)) val)
						; Else keep evaluating next line
						(else (eval-sequence (cdr block) env))
					)
				)
			)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluates an if block

(define (eval-if-block block env)
  (py-error "TodoError: Person B, Question 7")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluates an else block

(define (eval-else-block block env)
	; Evaluate the block formed from the body of the else
  (eval-sequence (else-block-body block) env)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluates a define block

(define (eval-def-block block env)
  (let 
		(
			; Create procedure
			(proc 
				(make-py-proc 
					(def-block-name block)
					(def-block-params block)
					(def-block-body block)
					env
				)
			)
		)
		; Save the procedure as variable in environment
    (define-variable! (def-block-name block) proc env)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluates an elif block

(define (eval-elif-block block env)
  (py-error "TodoError: Person B, Question 7")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluates a while block

(define (eval-while-block block env)
  (let 
		(
			; Obtain conditional predicate
			(pred (while-block-pred block))
			; Obtain while body
			(body (while-block-body block))
			; Obtain else clause in while
			(else-clause (while-block-else block))
		)
  	(let ((should-eval-if else-clause))
  	  (define (loop)
				; Evaluate predicate
				(let ((bool-value (py-eval (make-line-obj pred) env)))
					; Is it true?
				  (if (ask bool-value 'true?)
						; 
				    (let ((result (eval-sequence body env)))
							(cond 
								((eq? result '*BREAK*) (set! should-eval-if #f) *NONE*)
								((and (pair? result) (eq? (car result) '*RETURN*)) result)
					      (else (loop))
							)
						)
						; If the predicate is not true, and there an else clause at 
						; the end of the while
				    (if should-eval-if
							; Evaluate else clause
							(eval-item (make-line-obj else-clause) env)
							; If not return nothing
							*NONE*
						)
					)
				)
			)
			; Start loop
  		(loop)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluates a for block

(define (eval-for-block block env)
  (py-error "TodoError: Person A, Question 7")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lambda

(define (eval-lambda line-obj env)

  (define (collect-lambda-params)
		; If the line is empty, throw errors
    (if (ask line-obj 'empty?)
			(py-error "SyntaxError: Expected \":\", encountered newline")
			; Else read next token
			(let ((token (ask line-obj 'next)))
			  (cond 
					; If the token is :, there are no more parameters
					((eq? token ':) '())
					; If the token is a comma keep reading
					((comma? token) (collect-lambda-params))
					; Else add the token as paramter to the list and keep reading
					(else (cons token (collect-lambda-params)))
				)
			)
		)
	)

  (define (get-lambda-body braces)
		; Define control variables
    (define stop-tokens '(: |,| |]| |)| |}|))
    (define brace-alist '((|{| . |}|) (|[| . |]|) (|(| . |)|)))
    (define open-braces (map car brace-alist))
    (define close-braces (map cdr brace-alist))
    (define (reverse-brace token)
      (cdr (assq token brace-alist))
		)
		; If the line is emtpy: finish
    (if (ask line-obj 'empty?)
			'()
			; Else obtain next token
			(let ((token (ask line-obj 'next)))
			  (cond 
					((and 
							; If there are no ), ] or }
							(null? braces) 
							; and this token is an stop token
							(memq token stop-tokens))
						;; so the caller can see the brace
						(ask line-obj 'push token) 
						; Finish
						'()
					)
					; If the token is opened ( [ {
					((memq token open-braces)
						; Add token to list
						(cons 
							token
							; Continue analyzing body
					    (get-lambda-body
								; Obain the closing equivalent
								(cons (reverse-brace token) braces)
							)
						)
					)
					; If the token is closed: ), ], }
					((memq token close-braces)
						(if 
							(and 
								; If there are closed braces pending
								(not (null? braces)) ;; null case handled above
								; This closed brace equals the first in the pending list
								(eq? token (car braces))
							)
							; Continue analyzing and remove this brace from pending closed braces list
					    (cons token (get-lambda-body (cdr braces)))
							; Else throw error
					    (py-error "SyntaxError: unexpected token " token))
					)
					(else (cons token (get-lambda-body braces)))
				)
			)
		)
	)

  (let 
		(
			; Create procedures name
			(name (string->symbol "<lambda>"))
			; Obtain lambda procedure parameters
			(params (collect-lambda-params))
			; Create body (note we are going to create a procedure from this lambda expession so:
			;		- we add an indentation
			;		- we add a return before the body
			;	)
			(body 
				(list 
					(cons 
						'*DUMMY-INDENT*
						(cons 
							'return (get-lambda-body '())
						)
					)
				)
			)
		)
  	(make-py-proc name params body env)
	)
)

;; File Importation
(define (eval-import line-obj)
  (define (gather-tokens)
    (cond 
			; If the line is empty, finish
			((ask line-obj 'empty?) '())
			; If the current token is a comma, simply read it and continues 
      ((comma? (ask line-obj 'peek)) (ask line-obj 'next) (gather-tokens))
			; Else, obtain the next token and save it
      (else
        (let ((n (ask line-obj 'next)))
					; Save the token and keep analyzing
          (cons n (gather-tokens))
				)
			)
		)
	)

	; Obtain all modules to import and load them
  (let ((fnames (gather-tokens)))
    (for-each meta-load fnames)
	)
  *NONE*
)

;; Errors: bump to Scheme
(define (eval-raise line-obj env)
	; Obtain error
  (let ((err (py-eval line-obj env)))
		; Create and print error
    (py-error "Error: " (ask err 'val))
	)
)

(define (py-error . args)
	; Print arguments (error message)
  (for-each display args)
  (newline)
	; Throw error
  (error "PythonError")
)

(define (eval-list-comp line-obj env)
  (py-error "ExpertError: List Comprehensions")
)

;; Blocks, Loops, Procedures

(define unindented-line #f)

;; Read-block is a procedure of two arguments.  Old-indent is the indentation
;; (as a Scheme number) to check against for dedents (mostly for else and elif
;; blocks).  Env is the current environment, used for evaluating define
;; blocks.  It returns a list of lines (Scheme list of lists, NOT line-objs!).

(define read-block
  (let ((unindented-line #f))
    (lambda (old-indent env)
      (let ((new-indent #f))
				(define (read-loop)
				  (prompt "... ")
					; Read line of block
				  (let ((line (py-read)))
				    (define (helper)
							; If there is no indentation level set
				      (if (not new-indent) 
								; Obtain the indendation of the line
								(set! new-indent (indentation line))
							)
							(cond 
								; If there are no more tokens in the line
								((null? (tokens line)) 
									; Reset control variable
									(set! unindented-line #f) 
									'()
								)
								; If the indentation in the current line is bigger than the current
								; indent, throw error
								((> (indentation line) new-indent)
									(py-error "SyntaxError: Unexpected indent")
								)
								; If indentation is less than the new indent (less anidated)
								((< (indentation line) new-indent)
									(if 
										(and 
											; If the indentation level is the same as the old indent
											(= (indentation line) old-indent)
											; If there are tokens in the line
											(not (null? (tokens line)))
											; the first token are: elif or else
											(memq (car (tokens line)) '(elif else))
										)
										; Create a block from the current read line
										(let ((trailing-block (make-block (make-line-obj line) env)))
											; If there is not an unindented-line saved
											(if (not unindented-line)
												; Simply return block as list
						    			  (list trailing-block)
						    			  (begin 
													; If not retrieve line
													(set! line unindented-line)
													; Reset
							  			   	(set! unindented-line #f)
													; Continue reading
							  			   	(cons trailing-block (helper))
												)
											)
										)
										; Else
										(begin 
											; Set this line as unindented-line
											(set! unindented-line line)
											'()
										)
									)
								)
								; If the first token is def, if, for or while
								((memq (car (tokens line)) '(def if for while))
									; Obtain block under expression
									(let ((nested-block (make-block (make-line-obj line) env)))
										; If there is not an unindented-line saved
										(if (not unindented-line)
											(list nested-block)
											(begin 
												; If not retrieve line
												(set! line unindented-line)
												; Reset
												(set! unindented-line #f)
												; Continue reading
								 			 	(cons nested-block (helper))
											)
										)
									)
								)
								; Else simply return line
					    	(else 
									(cons line (read-loop))
								)
							)
						)
						(helper)
					)
				)
				(read-loop)
			)
		)
	)
)

;; Prints a python object.
(define (py-print obj)
  (if (not (none? obj))
    (if (ask obj 'string?)
			(print (ask obj 'val))
	  	(begin 
				(display (ask (ask obj '__str__) 'val)) 
				(newline)
			)
		)
	)
  *NONE*
)

;; Takes the last value returned from py-eval and applies the next infix
;; operator, if there is one.  Also checks for list slices and procedure calls

(define (handle-infix val line-obj env)
	; If the line is empty
  (if (ask line-obj 'empty?)
			; Return the value
      val
			; Obtain next token
      (let ((token (ask line-obj 'next)))
				(cond 
					;; arithmetic infix operators
					((infix-op? token) 
						; Obtain first element of line
						(let ((rhs (eval-item line-obj env)))
							(handle-infix 
								; Apply operation to obtain value
								(py-apply 
									(ask val (lookup-op token))
									(list rhs)
								)
								line-obj
								env
							)
						)
					)
	      ;; logical infix operators
	      ((and? token)
					(py-error "TodoError: Person A, Question 5")
				)
	      ((or? token)
					(py-error "TodoError: Person A, Question 5")
				)
	      ;; test for membership
	      ((in? token)
					(py-error "TodoError: Person B, Question 5")
				)
	      ((not? token)
					(py-error "TodoError: Person B, Question 5")
				)
	      ;; dot syntax message: val.msg
        ((dotted? token)
					; Obtain function after dot
          (let ((func (ask val (remove-dot token))))      ;gets the py-function
            (if (and 
									; If the line object is not emtpy
									(not (ask line-obj 'empty?))
									;IF IT IS ACTUALLY A FUNCTION CALL, EVALUATE IT
                  (open-paren? (ask line-obj 'peek))
								) 
								; Call function
								; make sure to continue handling infix: i.e -> if list.length() > 10: -> evaluate the `> 10` portion
                (handle-infix (eval-func func line-obj env) line-obj env) 
								;OTHERWISE RETURN THE FUNCTION ITSELF
                (handle-infix func line-obj env)
						)
					)
				)
	      (else 
					(begin 
						(ask line-obj 'push token)
						val
					)
				)
			)
		)
	)
)


;; Logical operators
(define (eat-tokens line-obj) ;; eats until a comma, newline or close-paren
  (define stop-tokens '(: |,| |]| |)|))
  (define open-braces '(|[| |(|))
  (define close-braces '(|]| |)|))
  (define (helper line-obj braces)
    (if (ask line-obj 'empty?)
      *NONE*
      (let ((token (ask line-obj 'peek)))
        (cond
          ((and (memq token stop-tokens) (null? braces))
            *NONE*
					)
          ((and (memq token close-braces) (not (null? braces)))
            (begin 
							(ask line-obj 'next) 
							(helper line-obj (cdr braces))
						)
					)
          ((memq token open-braces)
            (begin 
							(ask line-obj 'next) 
							(helper line-obj (cons token braces))
						)
					)
          (else
            (begin 
							(ask line-obj 'next) 
							(helper line-obj braces)
						)
					)
				)
			)
		)
	)
  (helper line-obj '())
)

(define (meta-load fname)
  (define (loader)
    (let ((exp (py-read)))
      (if (and (null? (cdr exp))
	       (eof-object? (peek-char)))
	  *NONE*
	  (begin (py-eval (make-line-obj exp)
			  the-global-environment)
		 (loader)))))
  (let ((file (symbol->string (word fname ".py"))))
    (set-variable-value! '__name__ (make-py-string file) the-global-environment)
    (with-input-from-file file loader)
    (set-variable-value! '__name__
			 (make-py-string "__main__")
			 the-global-environment)
    *NONE*))

(define (collect-sequence line-obj env close-token)
  (let ((token (ask line-obj 'next)))
    (cond
      ((eq? token close-token) '())
      ((comma? token) (collect-sequence line-obj env close-token))
      (else
       (ask line-obj 'push token)
       (let ((obj (py-eval line-obj env)))
			(cons obj
			   (collect-sequence line-obj env close-token)))))))

(define (collect-key-value line-obj env close-token)
  (py-error "TodoError: Both Partners. Question 8"))

