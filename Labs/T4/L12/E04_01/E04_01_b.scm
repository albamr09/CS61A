
; Expressions
(load "P01_02.scm")
; Environment
(load "P01_03.scm")
; Load eval
(load "P01_01.scm")
; Load interpreter
(load "P01_04.scm")

(driver-loop)

; Cy agrees that Ben is right about the for-each example, but says that that’s not the kind of program he
; was thinking about when he proposed his change to eval-sequence. He defines the following two procedures 
; in the lazy evaluator:

;;; TEST IN INTERACTIVE MODE
;; $ stk -l ../../../../lib/simply.scm 

;; STk> (load "E04_01.scm")

;;; L-Eval input:

(define (p1 x)
  (set! 
    x 
    (cons x '(2))
  )
  x
)

(define (p2 x)
  (define (p e)
    e 
    x
  )
  (p 
    (set! 
      x 
      (cons x '(2))
    )
  )
)

; What are the values of (p1 1) and (p2 1) with the original eval-sequence? 

; >> (p1 1)
; >>>> (
; >>>>   (set!
; >>>>     1
; >>>>     (cons 1 '(2))
; >>>>   )
; >>>>   x
; >>>> )
; << (1 2)
; >> (p2 1)
; >>>> (p
; >>>>   (set!
; >>>>     x
; >>>>     (cons 1 '(2))
; >>>>   )
; >>>> )
; >>>>>> (set! 
; >>>>>>   x
; >>>>>>   (cons 1 '(2))
; >>>>>> )
; <<<< (1 2)
; >>>> (p (1 2))
; << (1 2)


; What would the values be with Cy’s proposed change to eval-sequence?

(define (eval-sequence exps env)
  (cond 
    ((last-exp? exps) (eval (first-exp exps) env))
    (else 
      (actual-value 
        (first-exp exps) 
        env
      )
      (eval-sequence 
        (rest-exps exps) 
        env
      )
    )
  )
)
