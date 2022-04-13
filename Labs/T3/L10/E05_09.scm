;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E05_09
;; SICP: 3.66
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Examine the stream (pairs integers integers). Can you make any general comments 
; about the order in which the pairs are placed into the stream? For example,
; approximately how many pairs precede the pair (1, 100)? the pair (99, 100)? 
; the pair (100, 100)? (If you can make precise mathematical statements 
; here, all the better. But feel free to give more qualitative answers if you find yourself
; getting bogged down.)

; Let's analyze the definition of pairs

(define (pairs s t)
  (cons-stream
    ; First element
    (list (stream-car s) (stream-car t))
    ; Intercalate
    (interleave
      ; X
      (stream-map 
        (lambda (x) (list (stream-car s) x))
        (stream-cdr t)
      )
      ; Y
      (pairs (stream-cdr s) (stream-cdr t))
    )
  )
)

; Note we call this function with the integers' streams

; The interleave function intercales elements from each stream. So here we have two streams
; to intercalate, let's call the first one X and the second one Y.

; Suppose i is the first element in s, then
; X = {(i, i + 1), (i, i + 2) , (i, i + 3), ...}

; And by the definition of the function, we call pairs again on the streams s and t deleting
; the first element i, which means the first element for the recursive call is (i + 1). 
; Y = {(i + 1, i + 1), ...}

; Which means all of the pairs generated from now on will contain i + 1 or greater integers. So as you can see, given a pair (i, j), i <= j.

; Ok, now a picture. So for each integer, we will draw a column that contains all pairs that 
; have said integer as the first element. We will list the order the pairs are generated in:

;  1.               2.            3.          4.
;-------------------------------------------------------------
;  (1,1)          |             |           | 
;-------------------------------------------------------------
;  (1,2)          |             |           | 
;-------------------------------------------------------------
;                 | (2,2)       |           |
;-------------------------------------------------------------
;  (1,3)          |             |           |
;-------------------------------------------------------------
;                 | (2,3)       |           |
;-------------------------------------------------------------
;  (1,4)          |             |           | 
;-------------------------------------------------------------
;                 |             | (3,3)     | 
;-------------------------------------------------------------
;  (1.5)          |             |           | 
;-------------------------------------------------------------
;                 | (2,4)       |           | 
;-------------------------------------------------------------
;  (1,6)          |             |           | 
;-------------------------------------------------------------
;                 |             | (3,4)     | 
;-------------------------------------------------------------
;  (1,7)          |             |           | 
;-------------------------------------------------------------
;                 | (2,5)       |           | 
;-------------------------------------------------------------
;  (1,8)          |             |           | 
;-------------------------------------------------------------
;                 |             |           | (4,4) 
;-------------------------------------------------------------
; (1,9)           |             |           | 
;-------------------------------------------------------------
;                 | (2,6)       |           | 
;-------------------------------------------------------------
; (1,10)          |             |           | 
;-------------------------------------------------------------
;                 |             | (3,5)    | 
;-------------------------------------------------------------
; (1,11)          |             |           | 
;-------------------------------------------------------------
;                 | (2,7)       |           | 
;-------------------------------------------------------------
; (1,12)          |             |           | 
;-------------------------------------------------------------
;                 |             | (3,6)     | 
;-------------------------------------------------------------
; (1,13)          |             |           | 
;-------------------------------------------------------------
;                 | (2, 8)      |           |  
;-------------------------------------------------------------
; (1,14)          |             |           | 
;-------------------------------------------------------------
;                 |             |           | (4,5) 
