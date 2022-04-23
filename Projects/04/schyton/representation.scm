;; Selectors for py-eval
(define (bool? token) (memq token '(|True| |False|)))
(define (none? token)
  (or (eq? token *NONE*)
      (eq? token '|None|)))
(define (if? token) (eq? token 'if))
(define (for? token) (eq? token 'for))
(define (while? token) (eq? token 'while))
(define (def? token) (eq? token 'def))
(define (print? token) (eq? token 'print))
(define (assignment? token line-obj)
  (and (variable? token)
       (not (ask line-obj 'empty?))
       (let ((next (ask line-obj 'peek)))
	 (if (eq? next '=)
	     (begin (ask line-obj 'next) #t)
	     #f))))
(define (variable? token) (symbol? token))
(define (application? token line-obj)
  ;;  handle -> fib(10)
  (and (variable? token)
       (not (ask line-obj 'empty?))
       (open-paren? (ask line-obj 'peek))))
(define (bracket-dereference? token line-obj) ;;  dict["hello"] and list[0]
  (and (variable? token)
       (not (ask line-obj 'empty?))
       (open-bracket? (ask line-obj 'peek))))
(define (return? token) (eq? token 'return))
(define (break? token) (eq? token 'break))
(define (continue? token) (eq? token 'continue))
(define (block? token) (and (pair? token) (eq? (car token) '*BLOCK*)))
(define (lambda? token) (eq? token 'lambda))
(define (import? token) (eq? token 'import))
(define (raise? token) (eq? token 'raise))
(define (not? token) (eq? token 'not))
;; Infix Handling
(define infix-alist
  '((+ . __add__) (- . __sub__) (*  . __mul__)
    (/ . __div__) (% . __mod__) (** . __pow__)
    (> . __gt__)  (>= . __ge__) (== . __eq__)
    (< . __lt__)  (<= . __le__) (!= . __ne__)))

;; Infix selectors
(define infix-operators (map car infix-alist))
(define (infix-op? token) (memq token infix-operators))
(define (lookup-op op) (cdr (assq op infix-alist)))

;; Infix selectors
(define (and? token) (eq? token 'and))
(define (or? token) (eq? token 'or))
(define (in? token) (eq? token 'in))
(define (is? token) (eq? token 'is))
(define (dotted? token) (equal? (first token) '.))
(define (remove-dot token)
  (let ((tail (butfirst token)))
    (if (string? tail)
	(string->symbol tail)
	tail)))

; Strings object
(define (make-py-string str)
  (instantiate py-string str)
)

; Numeric object
(define (make-py-num num)
  (if (exact? num)
      (instantiate py-int num)
      (instantiate py-float num)))

;; Lists
(define (open-bracket? token) (eq? token open-bracket-symbol))
(define (close-bracket? token) (eq? token close-bracket-symbol))

;; Dictionaries
(define (open-brace? token) (eq? token open-brace-symbol))
(define (close-brace? token) (eq? token close-brace-symbol))

;; Procedures
(define (open-paren? token) (eq? token open-paren-symbol))
(define (close-paren? token) (eq? token close-paren-symbol))
(define (py-primitive? proc) (ask proc 'primitive?))
(define (py-procedure? proc) (ask proc 'procedure?))
(define (py-apply proc args)
  (cond ((py-primitive? proc) (ask proc '__call__ args))
	((py-procedure? proc) (ask proc '__call__ args))
	(else (py-error "TypeError: cannot call objects of type: "
			(ask proc 'type)))))

;; Procedure definitions
(define (make-def-block line-obj env)
  (let ((name (ask line-obj 'next))
	(params (begin (ask line-obj 'next) (collect-params line-obj env))))
    (if (or (ask line-obj 'empty?)
	    (not (eq? (ask line-obj 'next) ':)))
	(py-error "SyntaxError: Expected \":\"")
	(let ((body (read-block (ask line-obj 'indentation) env)))
	  (list '*BLOCK* '*DEF-BLOCK* (cons name params) body)))))
(define (def-block-name block) (caaddr block))
(define (def-block-params block) (cdaddr block))
(define (def-block-body block) (cadddr block))
(define (collect-params line-obj env)
  (if (ask line-obj 'empty?)
      (py-error "SyntaxError: Expected \")\"")
      (let ((token (ask line-obj 'next)))
	(cond ((eq? token close-paren-symbol) '())
	      ((comma? token) (collect-params line-obj env))
	      ((eq? (ask line-obj 'peek) '=)
	       (py-error "ExpertError: Default Parameters"))
	      (else (cons token (collect-params line-obj env)))))))
(define (apply-unary op val)
  (cond ((eq? op '-) (make-py-num (- (ask val 'val))))
	((eq? op 'not) (make-py-bool (not (ask val 'val)))) ;; handles "not x"
	(else (py-error "SyntaxError: Unrecognized operator: " op))))

;; Block selectors
(define (block-body block) (cdr block))
(define (block-type block) (cadr block))
(define (if-block? block) (eq? (block-type block) '*IF-BLOCK*))
(define (def-block? block) (eq? (block-type block) '*DEF-BLOCK*))
(define (for-block? block) (eq? (block-type block) '*FOR-BLOCK*))
(define (elif-block? block) (eq? (block-type block) '*ELIF-BLOCK*))
(define (else-block? block) (eq? (block-type block) '*ELSE-BLOCK*))
(define (while-block? block) (eq? (block-type block) '*WHILE-BLOCK*))
(define (eval-block block env)
  (cond ((if-block? block) (eval-if-block block env))
	((def-block? block) (eval-def-block block env))
	((for-block? block) (eval-for-block block env))
	((elif-block? block) (eval-elif-block block env))
	((else-block? block) (eval-else-block block env))
	((while-block? block) (eval-while-block block env))
	(else (py-error "SyntaxError: bad block type: " (block-type block)))))
(define (split-block block)
  ;; Split block takes a list of lines and checks if the last line is a else
  ;; or elif block.  If so, it returns a pair whose car is a list of all but
  ;; that last line and whose cdr is the elif or else block.  If there is no
  ;; such trailing elif or else block, it returns (cons block #f).
  (define (iter so-far to-go)
    (let ((current-line (tokens (car to-go)))
	  (rest (cdr to-go)))
      (if (null? rest)
	  (if (and (block? (car current-line))
		   (or (elif-block? (car current-line))
		       (else-block? (car current-line))))
	      (cons (reverse so-far) (car to-go))
	      (cons (reverse (cons (car to-go) so-far)) #f))
	  (iter (cons (car to-go) so-far) rest))))
  (iter '() block))

;; Block constructor
(define (make-block line-obj env)
  (let ((type (ask line-obj 'next)))
    (cond ((eq? type 'if) (list '*DUMMY-INDENT* (make-if-block line-obj env)))
	  ((eq? type 'for) (list '*DUMMY-INDENT* (make-for-block line-obj env)))
	  ((eq? type 'def) (list '*DUMMY-INDENT* (make-def-block line-obj env)))
	  ((eq? type 'elif) (list '*DUMMY-INDENT* (make-elif-block line-obj env)))
	  ((eq? type 'else) (list '*DUMMY-INDENT* (make-else-block line-obj env)))
	  ((eq? type 'while) (list '*DUMMY-INDENT* (make-while-block line-obj env)))
	  (else (py-error "SyntaxError: unknown keyword: " type)))))

;; Conditionals
(define (make-if-block line-obj env)
  (py-error "TodoError: Person B, Question 7"))
(define (if-block-pred block)
  (py-error "TodoError: Person B, Question 7"))
(define (if-block-body block)
  (py-error "TodoError: Person B, Question 7"))
(define (if-block-else block)
  (py-error "TodoError: Person B, Question 7"))


;; Elif/Else blocks
(define (make-else-block line-obj env)
  (if (not (and (not (ask line-obj 'empty?))
		(eq? (ask line-obj 'next) ':)
		(ask line-obj 'empty?)))
      (py-error "SyntaxError: invalid syntax")
      (let ((body (read-block (ask line-obj 'indentation) env)))
	(list '*BLOCK* '*ELSE-BLOCK* (split-block body)))))
(define (else-block-body block) (caaddr block))
(define (else-block-else block) (py-error "SyntaxError: too many else clauses"))


(define (make-elif-block line-obj env)
  (py-error "TodoError: Person B, Question 7"))
(define (elif-block-pred block) (py-error "TodoError: Person B, Question 7"))
(define (elif-block-body block) (py-error "TodoError: Person B, Question 7"))
(define (elif-block-else block) (py-error "TodoError: Person B, Question 7"))


;; While loops
(define (make-while-block line-obj env)
  (py-error "TodoError: Both Partners, Question 6"))
(define (while-block-pred block)
  (py-error "TodoError: Both Partners, Question 6"))
(define (while-block-body block)
  (py-error "TodoError: Both Partners, Question 6"))
(define (while-block-else block)
  (py-error "TodoError: Both Partners, Question 6"))

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

;; List Access
(define (get-slice line-obj env)
  ;; only handles [i], [i:j], and slices, not [:j], [i:], or [::k]
  (let ((val (py-eval line-obj env)))
    (cond ((eq? (ask line-obj 'peek) close-bracket-symbol)
	   (ask line-obj 'next) ;; get rid of ] token
	   (list val))
	  ((eq? (ask line-obj 'peek) ':)
	   (ask line-obj 'next) ;; get rid of : token
	   (cons val (get-slice line-obj env)))
	  (else (py-error "SyntaxError: Expected \"]\", encountered "
			  (ask line-obj 'next))))))

;; Unary operators
(define unary-operators '(- not))
(define (unary-op? token) (memq token unary-operators))

;; List Comprehensions
(define (list-comp? seq) (memq 'for seq))

;; List comprehensions should work as follows:
;;   >>> myList = [1,2,3]
;;   >>> [3*x for x in myList]
;;   [3,6,9]
;;   >>> [i + j for i in "abc" for j in "def"]
;;   ["ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"]
;;   >>> [i*j for j in range(10) if j % 2 == 0 for i in "SICP"]
;;   ["SS", "II", "CC", "PP", "SSSS", "IIII", "CCCC", "PPPP"]
