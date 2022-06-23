; Suppose we type in the following definitions to the lazy evaluator:

; (define count 0)
; (define (id x) 
;   (set! 
;     count 
;     (+ count 1)
;   ) 
;   x
; )

; Give the missing values in the following sequence of interactions, and explain your answers.

; (define w (id (id 10)))

;;; L-Eval input:

; count

;;; L-Eval value:

; ⟨response⟩ = 1 
; When we defined w we set it to the procedure id, with (id 10) and an argument, which was evaluated. Thus we
; updated count to count + 1

;;; L-Eval input:

; w

;;; L-Eval value:

; ⟨response⟩ = 10
; w points to the procedure id, which simply returns the same value it was passed as argument, is this case 10

;;; L-Eval input:

; count

;;; L-Eval value:

; ⟨response⟩ = 2
; The definition of w is calling id, which updates count to count + 1
