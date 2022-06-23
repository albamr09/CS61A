;;; LISTS AS LAZY STREAMS


; Input this in the lazy evaluator stored in ../P02_01


; Redefine cons for lazy evaluation
(define (cons x y)
  ; Returns an procedure instead of the list
  (lambda 
    (m) 
    (m x y)
  )
)

; Redefine car for lazy evaluation
; - z: if you refer to cons, z is now a procedure not a list
;       this procedure is a lambda procedure
; >>>   (lambda
; >>>     (m)
; >>>     (m x y)
; >>>   )
; So:
; >>> (define lst (cons 1 2))
; Then:
; >>>   lst = 
; >>>   (lambda
; >>>     (m)
; >>>     (m 1 2)
; >>>   )
; So if we call car with lst as argument, what we are doing is:
; >>>   (lambda
; >>>     -- Arguments
; >>>     (
; >>>        m=(lambda
; >>>         (p q)
; >>>         p
; >>>        )
; >>>      )
; >>>     -- Body
; >>>     (lambda
; >>>       (1 2)
; >>>       1
; >>>     )
; >>>   )
(lambda
  (1 2)
  1
)
(define (car z)
  (z 
    ; View this as a selector procedure, used by the "z" procedure
    ; which calls the "m" argument (=this anonymous function) over the 
    ; elements of the list (x y)
    ; Thus we are selecting the first element
    (lambda 
      (p q) 
      p
    )
  )
)

; Redefine cdr for lazy evaluation
; - z: if you refer to cons, z is now a procedure not a list
;       this procedure is a lambda procedure
; >>>   (lambda
; >>>     (m)
; >>>     (m x y)
; >>>   )
; So:
; >>> (define lst (cons 1 2))
; Then:
; >>>   lst = 
; >>>   (lambda
; >>>     (m)
; >>>     (m 1 2)
; >>>   )
; So if we call cdr with lst as argument, what we are doing is:
; >>>   (lambda
; >>>     -- Arguments
; >>>     (
; >>>        m=(lambda
; >>>         (p q)
; >>>         q
; >>>        )
; >>>      )
; >>>     -- Body: call "selector" anonymous function
; >>>     (
; >>>       (lambda
; >>>         (p q)
; >>>         q
; >>>       )
; >>>       (1 2)
; >>>     )
; >>>   )
(define (cdr z)
  (z 
    ; View this as a selector procedure, used by the "z" procedure
    ; which calls the "m" argument (=this anonymous function) over the 
    ; elements of the list (x y)
    ; Thus we are selecting the first element
    (lambda 
      (p q) 
      q
    )
  )
)

; Redefine some list procedures
(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))
  )
)

(define (map proc items)
  (if (null? items)
    '()
    (cons 
      (proc (car items))
      (map proc (cdr items))
    )
  )
)

; Test

(define (scale-list items factor)
  (map 
    (lambda (x) (* x factor))
    items
  )
)
(define (add-lists list1 list2)
  (cond 
    ((null? list1) list2)
    ((null? list2) list1)
    (else 
      (cons 
        (+ (car list1) (car list2))
        (add-lists (cdr list1) (cdr list2))
      )
    )
  )
)

(define ones (cons 1 ones))

(define integers (cons 1 (add-lists ones integers)))

;;; L-Eval input:

(list-ref integers 17)

;;; L-Eval value:
18
