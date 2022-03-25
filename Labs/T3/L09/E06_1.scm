;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; E06
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Exercise 6. Compare the version of vector-filter from
; E06 to this one. Time it.

(define (vector-filter pred vec)
  (list->vector (filter pred (vector->list vec)))
)

;; TIMED TEST
(vector-filter (lambda (x) (> x 1)) (vector 1 2 8 9 0 1))

;;; This version
;;; 0,05s user 0,00s system 124% cpu 0,037 total
;;; E06 version
;;; 0,05s user 0,01s system 122% cpu 0,044 total 
