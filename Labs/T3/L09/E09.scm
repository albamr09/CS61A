;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; E09
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Write the procedure cxr-name. Its argument will be a function made by composing cars and cdrs. 
; It should return the appropriate name for that function:

(define (cxr-name f)
  (print f)
)

(cxr-name (lambda (x) (cadr (cddar (cadar x)))))
; CADDDAADAR
