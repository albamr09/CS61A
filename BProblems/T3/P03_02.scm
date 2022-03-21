;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUEUE TAD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTRUCTOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A queue is described by a front pointer and a 
; rear pointer. On creation both are inialitez to
; null
(define (make-queue) (cons '() '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SELECTORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (front-queue queue)
  ; Check if the queue is empty
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    ; Return the front pointer (first element of list
    ; that describes the queue)
    (car (front-ptr queue))
  )
)

(define (front-ptr queue) 
  ; Rerturn first element of list, points to the 
  ; first element on the queue
  (car queue)
)

(define (rear-ptr queue) 
  ; Rerturn second element of list, points to the 
  ; last element on the queue
  (cdr queue)
)

(define (empty-queue? queue)
  ; If the front pointer is null,
  ; the queue is empty
  (null? 
    (front-ptr queue)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MUTATORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-front-ptr! queue item)
  ; Update front pointer of queue
  (set-car! queue item)
)

(define (set-rear-ptr! queue item)
  ; Update rear pointer of queue
  (set-cdr! queue item)
)

(define (insert-queue! queue item)
  (let 
    ; Create new pair, whose car is the item to be
    ; inserted
    ((new-pair (cons item '())))
    (cond 
      ; If the queue is empty, 
      ((empty-queue? queue)
        ; The new pair represents the queue
        ; The front pointer of the pair is the front 
        ; pointer of the queue
        (set-front-ptr! queue new-pair)
        ; The rear pointer of the pair is the rear 
        ; pointer of the queue
        (set-rear-ptr! queue new-pair)
        queue
      )
      ; If it is not empty
      (else
        ; modify final pair, so its cdr points to
        ; new-pair
        (set-cdr! (rear-ptr queue) new-pair)
        ; Set rear pointer to new pair
        (set-rear-ptr! queue new-pair)
        queue
      )
    )
  )
)

(define (delete-queue! queue)
  (cond 
    ; Cannot delete from an empty queue
    ((empty-queue? queue)
      (error "DELETE! called with an empty queue" queue)
    )
    (else 
      ; Update front pointer
      (set-front-ptr! 
        queue 
        ; New value of the front pointer, get it from list
        ; after removing first element 
        (cdr (front-ptr queue))
      )
      queue
    )
  )
)

;; TEST
(define q (make-queue))
; q
(insert-queue! q 'a)
; a
(insert-queue! q 'b)
; a b