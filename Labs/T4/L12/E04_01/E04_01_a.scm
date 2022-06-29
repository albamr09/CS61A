
; Expressions
(load "P01_02.scm")
; Environment
(load "P01_03.scm")
; Load eval
(load "P01_01.scm")
; Load interpreter
(load "P01_04.scm")

(driver-loop)

;; Cy D. Fect, a reformed C programmer, is worried that some side effects may never take place, 
;; because the lazy evaluator doesn’t force the expressions in a sequence. Since the value of an 
;; expression in a sequence other than the last one is not used (the expression is there

;; only for its effect, such as assigning to a variable or printing), there can be no subsequent 
;; use of this value (e.g., as an argument to a primitive procedure) that will cause it to be
;; forced. Cy thus thinks that when evaluating sequences, we must force all expressions in the 
;; sequence except the final one. 

;; Ben Bitdiddle thinks Cy is wrong. He shows Cy the for-each procedure described in Exercise 2.23, which
;; gives an important example of a sequence with side effects:

(define (for-each proc items)
  (if (null? items)
    'done
    (begin 
      (proc (car items))
      (for-each 
        proc 
        (cdr items)
      )
    )
  )
)

; He claims that the evaluator in the text (with the original eval-sequence) handles this correctly:

;;; TEST IN INTERACTIVE MODE
;; $ stk -l ../../../../lib/simply.scm 

;; STk> (load "E04_01_a.scm")

;;; L-Eval input:

; (for-each 
;   (lambda 
;     (x) 
;     (newline) 
;     (display x)
;   )
;   (list 57 321 88)
; )
; 
; 57
; 321
; 88

;;; L-Eval value:

; done

; In this case it works because we are forcing the evaluation of x by printing it.
