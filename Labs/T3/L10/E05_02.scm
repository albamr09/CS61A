;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Exercise 5
;;3.51
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Load stream-enumerate-interval
(load "E03.scm")

(define x 
  (stream-map 
    show 
    ; Sequence (stream) of integers from 0 to 10
    (stream-enumerate-interval 0 10)
  )
)

(stream-ref x 5)

#| Returns:

0 1 2 3 4 5

|#


(stream-ref x 7)
#| Returns:

6 7

|#
