
;SICP 3.17 Write a correct version of count-pairs.
(define (count-pairs x)
	(error "Not implemented yet!"))


;SICP 3.21 Explain what Eva Lu Ator is talking about, and what happened with
;Ben's examples.  Then define print-queue.
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
	  
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 
#| What happened with Ben's examples?




|#
; Implement the definition of print-queue
;Make sure you use display to print the queue.
(define (print-queue queue)
	(error "Not implemented yet!"))


;SICP 3.25 Write lookup and insert!

(define (lookup keys table)
	(error "Not implemented yet!")
)

(define (insert! keys value table)
	(error "Not implemented yet!")
)

#|
SICP 3.27

Explain why the number of steps is proportional to n (you may want to
include a trace to explain).

Would it still work (efficiently) if we define memo-fib as (memoize
fib)?  Why or why not?

|#

;Exercise 5. Write vector-append.
(define (vector-append v1 v2)
	(error "Not implemented yet!") 
)

;Exercise 6. Write vector-filter.
(define (vector-filter pred vec)
	(error "Not implemented yet!")
)

;Exercise 7. Write bubble-sort!
(define (bubble-sort! vec)
	(error "Not implemented yet!")
)

; The order of growth of the running time of bubble sort is Theta(??)
