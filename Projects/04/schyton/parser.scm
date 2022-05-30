;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File: PARSER.SCM
;; Author: Hoa Long Tam (hoalong.tam@berkeley.edu)
;;
;; Adapted for use in Python from a Logo-in-Scheme interpreter written by Brian
;; Harvey (bh@cs.berkeley.edu), available at ~cs61a/lib/logo.scm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSER HELPER METHODS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (char->symbol ch) (string->symbol (make-string 1 ch)))
(define (comma? symbol) (eq? symbol '|,|))
(define (colon? symbol) (eq? symbol '|:|))
(define (char-newline? char)
  (or 
    (eq? char #\newline) ;; you're in
    (and 
      (eq? char #\return)
	    (eq? (peek-char) #\newline) 
	    (read-char) ;; chomp off newline
    )
  )
)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define operators '(#\+ #\- #\* #\/ #\% #\< #\> #\! #\=))
(define open-brace-symbol (char->symbol #\{))
(define close-brace-symbol (char->symbol #\}))
(define open-paren-symbol (char->symbol #\())
(define close-paren-symbol (char->symbol #\)))
(define open-bracket-symbol (char->symbol #\[))
(define close-bracket-symbol (char->symbol #\]))
;; Unary operators
(define unary-operators '(- not))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main tokenizer.  Reads in a line from standard input and returns a list
;; of the form (indentation token1 token2 token3 ...).  Turns the line
;; 'def foo(a,b):' into (def foo |(| a |,| b |)| :).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RETURN VALUE
;; For example, if PY-READ reads "if a == 3:" at the Schython prompt, it will return
;; the *Scheme* list "(0 if a == 3 :)",

;; NOTE
;; special Scheme characters such as parentheses and commas are distinguished using
;; pipes, so "(a + b) * 3" becomes the Scheme list "(0 |(| a + b |)| * 3)".

(define (py-read)

  (define (get-indent-and-tokens)
    ;; TODO: Both Partners, Question 2
    (cons 0 (get-tokens '()))
  )

  (define (reverse-brace char)
    (let 
      (
        (result 
          (assq 
            char 
            '((#\{ . #\}) (#\} . #\{)
			       (#\( . #\)) (#\) . #\()
			       (#\[ . #\]) (#\] . #\[))
          )
        )
      )
      (if result
	      (cdr result)
	      (read-error "SyntaxError: bad closing brace: " char)
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; MAIN IDEA: reads characters and groups them into tokens
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Reads in until the end of the line and breaks the stream of input into a
  ;; list of tokens.  Braces is a list of characters representing open brace
  ;; ([, (, and {) tokens, so it can throw an error if braces are mismatched.
  ;; If it reaches the endof a line while inside braces, it keeps reading
  ;; until the braces are closed.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; NOTE
  ;; It uses the PEEK-CHAR procedure to look at the next character typed at
  ;; the Schython prompt *without processing (or "eating") it*, so that the
  ;; character can be used again; this is different from the READ-CHAR
  ;; procedure, which reads the next character and processes (or "eats")
  ;; it.

  (define (get-tokens braces)
    (let 
      ; Read next char
      ((char (peek-char)))
      (cond
        ; If it is a new line
        ((char-newline? char)
          ; If this is not the end of the line
	        (if (not (null? braces))
	          (begin 
              ; Read the current char = new line
              (read-char) 
              ; Keep reading
              (get-tokens braces)
            )
            ; Else only read the char
	          (begin 
              (read-char) 
              '()
            )
          )
        )
        ; If it is end of file
        ((eof-object? char)
          ; But there are more tokens/elements on this line
	        (if (not (null? braces))
            ; Throw error
            (read-error "SyntaxError: End of file inside expression")
            ; Else finish
            '()
          )
        )
        ; If it is a space
        ((eq? char #\space)
          ; Read the current char = space
          (read-char)
          ; Keep reading
          (get-tokens braces)
        )
        ; If it is a comment
        ((eq? char #\#)
         (ignore-comment)
         '()
        )
        ; If it is [, (, {
        ((memq char (list #\[ #\( #\{))
          ; Create symbol from the character
          (let ((s (char->symbol (read-char))))
            ; Add the token to the list and continue analysing
            (cons 
              s 
              (get-tokens (cons char braces))
            )
          )
        )
        ; If it is closing
        ((memq char (list #\] #\) #\}))
	        (if 
            ; Find the corresponding opening brace
            (and 
              (not (null? braces)) 
              (eq? char (reverse-brace (car braces)))
            )
            (let ((t (char->symbol (read-char))))
              ; Add the token to the list and continue analysing
              (cons 
                t 
                (get-tokens (cdr braces))
              )
            )
            ; If there are no opening brace for this closing brace: error
            (read-error "SyntaxError: mismatched brace: " char)
          )
        )
        ; Commar or :
        ((memq char (list #\, #\:))
          ; Convert char to symbol
          (let ((t (char->symbol (read-char))))
            ; Add the token to the list and continue analysing
            (cons 
              t 
              (get-tokens braces)
            )
          )
        )
        ; Single or double quotes
        ((memq char (list #\" #\'))
          ; Obtain the whole string
          (let ((t (list->string (get-string (read-char)))))
            ; Add the token to the list and continue analysing
            (cons 
              t 
              (get-tokens braces)
            )
          )
        )
        ; Operator
        ((memq char operators)
          ; Obtain whole operator (and operator could have two or more characters, i.e. ==)
          (let ((t (get-operator)))
            (cons t (get-tokens braces))
          )
        )
        ; Number
        ((char-numeric? char)
          ; Obtain number
        	(let ((num (get-num "")))
            ; If it is and string
        	  (if (string? num)
              ; Convert to number add to list of tokens and keep analysing
              (cons 
                (string->number num) 
                (get-tokens braces)
              )
              ; Add directly to list of tokens and keep analysing
              (cons 
                num 
                (get-tokens braces)
              )
            )
          )
        )
        ; If none of the above
        (else
          ; Obtain next token
	        (let ((token (get-token (char->symbol (read-char)))))
	          (cond
              ; If next token is a string that starts with . and the second character is a number
              ; that is: .01231...
              ((and 
                (string? token)
                (eq? (string-ref token 0) #\.)
                (char-numeric? (string-ref token 1))
              )
                ; Create number with readable format
	              (cons 
                  ; append 0 to start
                  (word (string->symbol (string-append "0" token)))
                  ; Keep analysing
	        	      (get-tokens braces)
                )
              )
              ; If it is an string
              ((string? token)
                ; Add the token to the list of tokens
	              (cons 
                  (string->symbol token) 
                  ; Keep analysing
                  (get-tokens braces)
                )
              )
              ; Else keep analysing
              (else 
                (cons 
                  token 
                  (get-tokens braces)
                )
              )
            )
          )
        )
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; HELPER METHODS
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Read any token

  (define (get-token so-far)
    ; "Read" next char
    (let ((char (peek-char)))
      ; If it is the end of file
      (if (eof-object? char)
        ; Read it and return the expression
	      (begin
	        (read-char)
	        so-far
        )
        ; Else
	      (if 
          ; If char is not: alphabetic, numberic or _
          (not 
            (or 
              (char-alphabetic? char)
		          (char-numeric? char)
		          (eq? char #\_)
            )
          )
          ; Return the expression obtained
	        so-far
          ; Else keep reading chars
	        (get-token 
            (word 
              ; Expression so far
              so-far 
              ; We now read the character
              (char->symbol (read-char))
            )
          )
        )
      )
    )
  )

  ;; Reads in a number.  Num-so-far a Scheme word (we will convert back into
  ;; a Scheme number in the get-tokens procedure).
  ;; TODO: Person B, Question 3

  (define (get-num num-so-far)
    ; "Read" the next character but not really
    (let ((char (peek-char)))
      ; If it is end of file
      (if (eof-object? char)
	      (begin
          ; Truly read the character
	        (read-char)
          ; Return the number read
	        num-so-far
        )
        ; Else, is it a numberic character
	      (if (char-numeric? char)
          ; If so, keep analysing the expression
	        (get-num 
            ; Add current character to the overall number
            (word 
              num-so-far 
              (char->symbol (read-char))
            )
          )
          ; If not, return the number obtained
	        num-so-far
        )
      )
    )
  )
  
  ;; Determine type of operator made up from double characters

  (define (get-operator)
    (let 
      (
        (char (read-char))
	      (next (peek-char))
      )
      (cond 
        ; Current char -> +, next char -> =
        ((eq? char #\+) (if (eq? next #\=) (begin (read-char) '+=) '+))
        ; Current char -> -, next char -> =
	      ((eq? char #\-) (if (eq? next #\=) (begin (read-char) '-=) '-))
        ; Current char -> %, next char -> =
	      ((eq? char #\%) (if (eq? next #\=) (begin (read-char) '%=) '%))
        ; Current char -> <, next char -> =
	      ((eq? char #\<) (if (eq? next #\=) (begin (read-char) '<=) '<))
        ; Current char -> >, next char -> =
	      ((eq? char #\>) (if (eq? next #\=) (begin (read-char) '>=) '>))
        ; Current char -> =, next char -> =
	      ((eq? char #\=) (if (eq? next #\=) (begin (read-char) '==) '=))
        ; Current char -> /, next char -> =
	      ((eq? char #\/) (if (eq? next #\=) (begin (read-char) '/=) '/))
        ; Current char -> !, next char -> =
	      ((eq? char #\!)
	        (if (eq? next #\=)
		      (begin (read-char) '!=)
          ; If not followed by =, standalone ! is not an operator
		      (read-error "Unknown operator: !"))
        )
        ; Current char -> *, next char -> 
	      ((eq? char #\*)
	        (cond 
            ; Next char -> *
            ((eq? next #\*)
		         (read-char)
		         (if (eq? (peek-char) #\=)
			         (begin 
                 (read-char) '**=) '**)
            )
            ; Next char -> =
		        ((eq? next #\=) (read-char) '*=)
            ; Else return normal *
		        (else '*)
          )
        )
      )
    )
  )

  ;; Reads in a string and returns a list of Scheme characters, up to, but not
  ;; including the closing quote.  Type is the Scheme character that opened
  ;; the string.  The first character returned by (read-char) when this
  ;; function is executed will be the first character of the desired string.

  (define (get-string type)
    (read-error "TodoError: Person A, Question 3")
  )

  (define (ignore-comment)
    (read-error "TodoError: Both Partners, Question 1")
  )
  
  ; Start reading tokens
  (get-indent-and-tokens)
)

;; Selectors for the list returned by py-read.
(define indentation car)
(define tokens cdr)

;; Error handler for py-read.  Needs to eat remaining tokens on the line from
;; user input before throwing the error.

(define (read-error . args)
  (define (loop)
    (let ((char (read-char)))
      (if 
        (or 
          (char-newline? char) 
          (eof-object? char)
        )
	      (apply py-error args)
	      (loop)
      )
    )
  )
  (loop)
)
