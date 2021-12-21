#lang racket
(require berkeley)

; Write a procedure substitute that takes three arguments: a sentence, an old word
;, and a new word. It should return a copy of the sentence, but 
; with every occurrence of the old word replaced by the new word.

(define (substitute sent old new)
  ; Evaluate expression
  (
    ; Define anonymous function
    (lambda
      ; Formal arguments
      (sent old new)
      ; Recursive definition
      (cond
        ; Empty return nothing
        ((empty? sent) '())
        ; If the first element in sent equal old replace it with new
        ((equal? (first sent) old) (se new (substitute (bf sent) old new)))
        ; Else do not replace the element
        (else (se (first sent) (substitute (bf sent) old new)))
      )
    )
    ; Arguments for the function
    sent old new
  )
)
(substitute '(she loves you yeah yeah yeah) 'yeah 'maybe)
;; Outputs: (she loves you maybe maybe maybe)
