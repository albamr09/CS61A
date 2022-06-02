;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File: utils.scm
;; Author: Hoa Long Tam (hoalong.tam@berkeley.edu)
;; Large parts adapted for use in as a Python-in-Scheme interpreter for use in
;; UC Berkeley's intro to computer science course, CS 61A from a Logo-in-Scheme
;; interpreter written by Brian Harvey (bh@cs.berkeley.edu), available at
;; ~cs61a/lib/logo-meta.scm, and the Metacircular Evaluator, a Scheme-in-Scheme
;; interpreter written by Harold Abelson and Gerald Jay Sussman, published in
;; the Structure and Interpretation of Computer Programs (1996, Cambridge, MA:
;; MIT Press).  Particular thanks go to Michael C Chang for ideas on how to
;; handle nested indented blocks.  Thanks also to Jon Kotker, for suggesting the
;; project and writing the specification, Christopher Cartland, for testing
;; and debugging, and George Wang, for testing and administrative support.
;;
;; REVISION HISTORY
;; 2.  July 30th, 2010.  Code added for 'else'-blocks to ensure equitable
;;                       distribution of work.
;; 1.  July 27th, 2010.  Project released.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spring 2014 CS61AS revisions
;;
;; edits made by Saam Barati  <saamyjoon@berkeley.edu>
;;           and Marion Halim <marionhalim@berkeley.edu>
;; from tests conducted while solving the project
;;
;; 1. We altered many places where `parser.scm` relied on a `cons` statement evaluating its arguments
;;    from the left to the right, but cons statements in STk evaluate the right argument first.
;;    i.e (cons (display 1) (display 2)) prints `2` then `1`
;;
;; 2. Move function application detection into eval-item instead of handle-infix because, logically speaking,
;;    the application of `foo()` is one item to be evaluated.
;;    (This also fixes the following bug: `>>> 2 * foo()`)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define **DEBUGGING** #t)

;; Read-Eval-Print Loop
(define (driver-loop)
	;; We invoke the MAKE-LINE-OBJ procedure, which takes in the list of
	;; tokens returned by PY-READ, and creates an object of the LINE-OBJ
	;; class
  (define (repl)
    (prompt ">>> ")
    (if (eof-object? (peek-char))
			(begin (newline) 'bye)
			(let ((line-obj (make-line-obj (py-read))))
				; If it is an exit statement
			  (if (ask line-obj 'exit?)
			    'bye
					; Else evaluate
			    (begin 
						(py-print (eval-line line-obj the-global-environment))
						; Recursive
				    (repl)
					)
				)
			)
		)
	)
  (read-char)
  (if **DEBUGGING**
    (repl)
    (while 
			(catch (repl)) 
			'*IGNORE-ME*
		)
	)
  'bye
)

;; Only display prompt if reading user input from standard input.
(define tty-port (current-input-port))

(define (prompt str)
  (if (eq? (current-input-port) tty-port)
    (begin 
			(display str) 
			(flush) 
			*NONE*
		)
    *NONE*
	)
)
