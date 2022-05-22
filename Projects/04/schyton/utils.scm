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
  (define (repl)
    (prompt ">>> ")
    (if (eof-object? (peek-char))
			(begin (newline) 'bye)
			(let ((line-obj (make-line-obj (py-read))))
			  (if (ask line-obj 'exit?)
			    'bye
			    (begin 
						(py-print (eval-line line-obj the-global-environment))
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


;;;;;;;;;;;;;;;;;;;;;;;;
;; MATH
;;;;;;;;;;;;;;;;;;;;;;;;

(define (square x) (* x x)) ;; math helper

(define-class (math)
  ; Parent: python object
  (parent (py-obj))
  (method (__str__) (make-py-string "<built-in module 'math'>"))
  ;; Mathematical constants
  (class-vars 
    (pi (make-py-num (* 4 (atan 1))))
	  (e (make-py-num (exp 1)))
	  (phi (make-py-num (/ (+ 1 (sqrt 5)) 2)))
  )
  ;; Number-theoretic functions
  (method (ceil)
    (make-py-primitive 
      'ceil
		  (lambda 
        (num) 
        (make-py-num (ceiling (ask num 'val)))
      )
    )
  )
  (method (fabs)
    (make-py-primitive 
      'fabs
		  (lambda (num)
			 (let ((val (ask num 'val)))
			    (if (< val 0)
			      (make-py-num (- val))
			      num
          )
        )
      )
    )
  )
  (method (factorial)
    (make-py-primitive
      'factorial
      (lambda (num)
        (define (fact-iter n p)
	        (if (= n 0)
	          p
	          (fact-iter (- n 1) (* p n)))
        )
        (if (or 
              (not (ask num 'int?))
	            (< (ask num 'val) 0)
            )
	          (py-error "ValueError: factorial() not defined for negative values")
	          (fact-iter (ask num 'val) 1)
        )
      )
    )
  )
  (method (floor)
    (make-py-primitive 
      'floor
		  (lambda 
        (num) 
        (make-py-num (floor (ask num 'val)))
      )
    )
  )
  (method (trunc)
    (make-py-primitive 
      'trunc
		  (lambda 
        (num) 
        (ask num '__trunc__)
      )
    )
  )
  ;; Power and logarithmic functions
  (method (exp)
    (make-py-primitive 
      'exp
		  (lambda 
        (num) 
        (make-py-num (exp (ask num 'val)))
      )
    )
  )
  (method (log)
    (make-py-primitive
      'log
      (lambda 
        (num . base)
        (if (null? base)
	        (make-py-num (log (ask num 'val)))
	        (make-py-num 
            (/ 
              (log (ask num 'val))
		  	      (log (ask (car base) 'val))
            )
          )
        )
      )
    )
  )
  (method (log10)
    (make-py-primitive 
      'log10
		  (lambda 
        (num)
			  (make-py-num (/ (log (ask num 'val)) (log 10)))
      )
    )
  )
  (method (pow)
    (make-py-primitive 
      'pow
		  (lambda 
        (x y)
			  (make-py-num (expt (ask x 'val) (ask y 'val)))
      )
    )
  )
  (method (sqrt)
    (make-py-primitive 
      'sqrt
		  (lambda 
        (num) 
        (make-py-num (sqrt (ask num 'val)))
      )
    )
  )
  ;; Trigonometric functions
  (method (acos)
    (make-py-primitive 
      'acos
		  (lambda 
        (num) 
        (make-py-num (acos (ask num 'val)))
      )
    )
  )
  (method (asin)
    (make-py-primitive 
      'asin
		  (lambda 
        (num) 
        (make-py-num (asin (ask num 'val)))
      )
    )
  )
  (method (atan)
    (make-py-primitive 
      'atan
		  (lambda 
        (num) 
        (make-py-num (atan (ask num 'val)))
      )
    )
  )
  (method (atan2)
    (make-py-primitive
     'atan2
     (lambda 
       (x y) 
       (make-py-num (atan (ask x 'val) (ask y 'val)))
      )
    )
  )
  (method (cos)
    (make-py-primitive
      'cos
      (lambda 
        (num) 
        (make-py-num (cos (ask num 'val)))
      )
    )
  )
  (method (hypot)
    (make-py-primitive
     'hypot
     (lambda 
       (x y) 
       (make-py-num 
          (sqrt 
            (+ 
              (square (ask x 'val))
					    (square (ask y 'val))
            )
          )
        )
      )
    )
  )
  (method (sin)
    (make-py-primitive
     'sin
     (lambda 
        (num) 
        (make-py-num (sin (ask num 'val)))
      )
    )
  )
  (method (tan)
    (make-py-primitive
      'tan
      (lambda 
        (num) 
        (make-py-num (tan (ask num 'val)))
      )
    )
  )
  ;; Angular conversion functions
  (method (degrees)
    (make-py-primitive
      'degrees
      (lambda 
        (num) 
        (make-py-num (* 180 (/ (ask num 'val) pi)))
      )
    )
  )
  (method (radians)
    (make-py-primitive
      'radians
      (lambda 
        (num) 
        (make-py-num (* pi (/ (ask num 'val) 180)))
      )
    )
  )
  ;; Hyperbolic functions:
  (method (asinh)
    (make-py-primitive
      'asinh
      (lambda 
        (num)
        (make-py-num 
          (log 
            (+ 
              (ask num 'val)
		  	      (sqrt (1+ (square (ask num 'val))))
            )
          )
        )
      )
    )
  )
  (method (acosh)
    (make-py-primitive
      'acosh
      (lambda 
        (num)
        (make-py-num 
          (log 
            (+ 
              (ask num 'val)
			        (sqrt (- (square (ask num 'val)) 1))
            )
          )
        )
      )
    )
  )
  (method (atanh)
    (make-py-primitive
      'atanh
      (lambda 
        (num)
        (make-py-num 
          (* 
            .5 
            (log 
              (/ 
                (+ 1 (ask num 'val))
				        (- 1 (ask num 'val))
              )
            )
          )
        )
      )
    )
  )
  (method (sinh)
    (make-py-primitive 
      'sinh
		  (lambda (num)
			  (make-py-num
			    (* 
            .5 
            (- 
              (exp (ask num 'val))
				      (exp (- (ask num 'val)))
            )
          )
        )
      )
    )
  )
  (method (cosh)
    (make-py-primitive 
      'cosh
		  (lambda 
        (num)
			  (make-py-num 
          (* 
            .5 
            (+ 
              (exp (ask num 'val))
					    (exp (- (ask num 'val)))
            )
          )
        )
      )
    )
  )
  (method (tanh)
    (make-py-primitive 
      'tanh
		  (lambda 
        (num)
			  (make-py-num
			    (/ 
            (- (exp (* 2 (ask num 'val))) 1)
			      (+ (exp (* 2 (ask num 'val))) 1)
          )
        )
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; RANDOM
;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (py-random)
  ; Parent: python object
  (parent (py-obj))
  ; To string method
  (method (__str__) (make-py-string "<built-in module 'random'>"))
  ; Draw random number from range 
  (method (randrange)
    (make-py-primitive
      'randrange
      (lambda 
        args
        (cond 
          ((null? args)
	          (py-error "TypeError: Too few arguments to randrange")
          )
          ; One argument (start)
	        ((null? (cdr args)) 
            (make-py-num (random (ask (car args) 'val)))
          )
          ; Two arguments (start, end)
	        ((null? (cddr args))
	          (let 
              (
                ; Obtain start of range
                (start (ask (car args) 'val))
                ; Create end of range
		            (end (-1+ (ask (cdr args 'val))))
              )
              ; Create python number object from the random number
		          (make-py-num (+ start (random (- end start))))
            )
          )
          ; Three arguments (start, end, step)
	        ((null? (cdddr args))
	          (let 
              (
                (start (ask (car args) 'val))
		            (end (ask (cadr args) 'val))
		            (step (ask (caddr args) 'val))
              )
		          (set! end (- end (quotient (- end start) step)))
              ; Create python number object from the random number
		          (make-py-num (+ start (* step (random (- end start)))))
            )
          )
          ; More than four arguments throw error
	        (else (py-error "TypeError: Too many arguments to randrange")))
      )
    )
  )
  ; Create random integer between numbers a and b
  (method (randint)
    (make-py-primitive
      'randint
      (lambda 
        (a b)
        (make-py-num 
          (+ 
            (ask a 'val) 
            (random (1+ (ask b 'val)))
          )
        )
      )
    )
  )
  ; Method to return a random item from a list
  (method (choice)
    (make-py-primitive
      'choice
      (lambda 
        ; List
        (seq)
        (let 
          ; Obtain length of list
          ((len (ask (ask seq '__len__) 'val)))
          ; Obtain the item in the random idex generated
	        (ask seq '__getitem__ (make-py-num (random len)))
        )
      )
    )
  )
  ; Return random number between 0 and 1
  (method (random)
    (make-py-primitive 
      'random
		  (lambda 
        () 
        (make-py-num (/ (random 4000000000)))
      )
    )
  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; ADD LIST OF PRIMITIVES
;;;;;;;;;;;;;;;;;;;;;;;;

(define (define-primitives!)
  (define (add-prim name proc)
    (define-variable! 
      ; Name of primitive
      name 
      ; Create primitive python object
      (make-py-primitive name proc) 
      ; Create in global environment
      the-global-environment
    )
  )
  ; Define math and random library
  (define-variable! 'math (instantiate math) the-global-environment)
  (define-variable! 'random (instantiate py-random) the-global-environment)
  ; Define primitive procedures
  (add-prim 'abs
	  (lambda 
      (num)
	    (if (< (ask num 'val) 0) 
        (make-py-num (- (ask num 'val))) 
        val
      )
    )
  )
  (add-prim 'bin
	  (lambda 
      (int)
	    (let 
        ((n (ask int 'val)))
		    (let 
          ((str (number->string n 2)))
		      (make-py-string
		        (if (< n 0)
		          (string-append 
                "-0b"
				        (substring str 1 (string-length str))
              )
		          (string-append "0b" str)
            )
          )
        )
      )
    )
  )
  (add-prim 
    'bool 
    (lambda 
      arg 
      (if (null? arg)
				(make-py-bool #f)
				(make-py-bool (ask (car arg) 'true?))
      )
    )
  )
  (add-prim 
    'chr 
    (lambda 
      (num)
		  (make-py-string 
        (string (integer->char (ask num 'val)))
      )
    )
  )
  (add-prim 
    'cmp 
    (lambda 
      (x y)
		  (cond 
        ((py-apply (ask x '__lt__) (list y)) -1)
			  ((py-apply (ask x '__gt__) (list y)) 1)
			  (else 0)
      )
    )
  )
  (add-prim 
    'divmod
	  (lambda 
      (a b)
	    (make-py-list 
        (list 
          (py-apply (ask a '__div__) (list b))
				  (py-apply (ask b '__mod__) (list b))
        )
      )
    )
  )
  (add-prim 
    'float 
    (lambda 
      (num) 
      (ask num '__float__)
    )
  )
  (add-prim 
    'hex
    (lambda (int)
      (let 
        ((n (ask int 'val)))
        (let 
          ((str (number->string n 16)))
          (make-py-string
            (if (< n 0)
              (string-append 
                "-0x"
                (substring str 1 (string-length str))
              )
              (string-append "0x" str)
            )
          )
        )
      )
    )
  )
  (add-prim 'int (lambda (num) (ask num '__int__)))
  (add-prim 'len (lambda (seq) (ask seq '__len__)))
  (add-prim 
    'oct
	  (lambda 
      (int)
      (let 
        ((n (ask int 'val)))
        (let 
          ((str (number->string n 8)))
          (make-py-string
            (if (< n 0)
              (string-append 
                "-0"
                (substring str 1 (string-length str))
              )
              (string-append "0" str)
            )
          )
        )
      )
    )
  )
  (add-prim
    'ord
    (lambda 
      (char)
      (if (not (= (ask (ask char '__len__) 'val) 1))
	      (py-error "TypeError: Expected string of length 1")
	      (make-py-num 
          (char->integer 
            (string-ref (ask char 'val) 0)
          )
        )
      )
    )
  )
  (add-prim
    'pow
    (lambda 
      (base pow . mod)
      (define (mexpt b n m)
        (cond 
          ((= n 0) 1)
          ((even? n) (modulo (mexpt (modulo (* b b) m) (/ n 2) m) m))
          (else 
            (modulo 
              (* 
                b 
                (modulo 
                  (mexpt 
                    (modulo (* b b) m)
                    (quotient n 2) 
                    m
                  )
                  m
                )
              )
              m
            )
          )
        )
      )
      (if (null? mod)
        (py-apply (ask base '__pow__) (list pow))
        (make-py-num 
          (mexpt 
            (ask base 'val)
            (ask pow 'val)
            (ask (car mod) 'val)
          )
        )
      )
    )
  )
  (add-prim
    'range
    (lambda 
      (num . other-args)
      (define (make-range low cur step so-far)
        (if (< cur low)
          (make-py-list so-far)
          (make-range low (- cur step) step (cons (make-py-num cur) so-far))
        )
      )
      (cond 
        ((null? other-args) (make-range 0 (-1+ (ask num 'val)) 1 '()))
        ((null? (cdr other-args))
          (make-range (ask num 'val) (-1+ (ask (car other-args) 'val)) 1 '())
        )
        (else
          (let 
            ((start (ask num 'val))
            (end (ask (car other-args) 'val))
            (step (ask (cadr other-args) 'val)))
            (cond 
              ((= step 0)
                (py-error "ValueError: range() step argument cannot be zero")
              )
              ((> step 0)
	              (let 
                  ((last-value (- end (modulo (- end start) step))))
	                (make-range start last-value step '()))
              )
              (else
                (let 
                  ((result (make-range (1+ end) start (- step) '())))
	                (py-apply (ask result 'reverse) '())
                  result
                )
              )
            )
          )
        )
      )
    )
  )
  (add-prim 
    'raw_input
	  (lambda 
      arg
	    (define (read-line so-far)
		    (let ((char (read-char)))
		      (if (or 
                (eof-object? char)
		    	      (eq? char #\newline)
              )
		          so-far
		          (read-line (string-append so-far (string char)))
          )
        )
      )
	    (if (not (null? arg))
		    (begin 
          (display (ask (ask (car arg) '__str__) 'val)) 
          (flush)
        )
      )
	    (make-py-string (read-line ""))
    )
  )
  (add-prim 
    'reversed
	  (lambda 
      (obj) 
      (ask (ask obj '__reversed__) '__call__ '())
    )
  )
  (add-prim 
    'sorted
	  (lambda 
      (obj) 
      (ask (ask obj '__sorted__) '__call__ '())
    )
  )
  (add-prim 'str (lambda (obj) (ask obj '__str__)))
  (add-prim 'type (lambda (obj) (make-py-type (objtype obj))))
)
