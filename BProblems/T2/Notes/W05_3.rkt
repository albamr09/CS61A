#lang racket
(require berkeley)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEEP LIST ADT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We’ll use the name deep lists for lists that contain lists. For example, the list

;[[john lennon] [paul mccartney] [george harrison] [ringo starr]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Other procedures

; (define (deep-map fn lol)
;   (if (list? lol)
;     ; If it is a list
;     (map 
;       (lambda 
;         ; For each sublist
;         (element) 
;         ; Apply function to elements
;         ; of sublist
;         (deep-map fn element)
;       )
;       lol
;     )
;     ; If it is an element apply function
;     (fn lol)
;   )
; )

(define (deep-map fn xmas)
  (cond 
    ((null? xmas) '())
    ; If it is a pair (list of two)
    ((pair? xmas)
      ; Create pair of the results
      (cons 
        ; Apply function to first element
        (deep-map fn (car xmas))
        ; Apply function to rest of elements
        (deep-map fn (cdr xmas))
      )
    )
    ; If it is an element, apply function to 
    ; element
    (else (fn xmas))
  )
)

