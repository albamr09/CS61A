; Suppose that we implement test-and-set! using an ordinary procedure as shown in the text, without
; attempting to make the operation atomic. Draw a timing diagram like the one in Figure 3.29 to demonstrate how the
; mutex implementation can fail by allowing two processes to acquire the mutex at the same time.

(define (test-and-set! cell)
  (if (car cell) 
    true 
    (begin (set-car! cell true) false)
  )
)

; Suppose there two processes P1 and P2 that attempt to lock the mutex:

; 1. P1 accesses the cell and applies (car cell), P1 obtains that the cell is empty, therefore we go to the line:
;  - (begin (set-car! cell true) false)
; 2. P2 accesses the cell and applies (car cell), P2 obtains that the cell is empty, therefore we go to the line:
;  - (begin (set-car! cell true) false)
; 3. P1 sets the cell to true and returns false
; 4. P4 sets the cell to true and returns false
; 5. Now both have adquired and locked the mutex
