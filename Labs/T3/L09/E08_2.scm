;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E08_1: SICP 3.19
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A deque (“double-ended queue”) is a sequence in which items can be inserted and deleted at either the
; front or the rear. Operations on deques are the constructor make-deque, the predicate empty-deque?, 
; selectors frontdeque and rear-deque, mutators front-insert-deque!, rear-insert-deque!, front-delete-deque!,
; and rear-deletedeque!. Show how to represent deques using pairs, and give implementations of the operations
; All operations should be accomplished in Θ(1) steps

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEQUEUE TAD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A deque is described by a front pointer and a 
; rear pointer.  

; Example: 
; Given a queue q of the form (a, b, c)
; q -> (. , .)--------------┑
;       | #front-pointer    | #rear-pointer
;       |                   |
;      (a, .) -> (b, .) -> (c, .)

;;;;;;;;;;;;;;;;;;;;;;;;;
; CONSTRUCTOR
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-deque) (cons '() '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SELECTORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (front-deque queue)
  ; Check if the queue is empty
  (if (empty-deque? queue)
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

(define (empty-deque? queue)
  ; If the front pointer is null,
  ; the queue is empty
  (null? 
    (front-ptr queue)
  )
)

(define (print-deque queue) 
  ; Go through the queue
  (define (loop ptr)
    ; If the current element equals the one pointed
    ; by the rear pointer we have reached the end of
    ; the queue
    (if (eq? ptr (rear-ptr queue))
      ptr
      ; Else keep appeding the elements in the queue
      ; through the front-ptr to the rear-ptr
      (cons
        (car ptr)
        (loop (cdr ptr))
      )
    )
  )
  ; Check is the queue is empty
  (if (empty-deque? queue)
    (print "The queue is empty")
    (print (loop (front-ptr queue)))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;
; MUTATORS
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-front-ptr! queue item)
  ; Update front pointer of queue
  (set-car! queue item)
)

(define (set-rear-ptr! queue item)
  ; Update rear pointer of queue
  (set-cdr! queue item)
)


;;;;;;;;;;;;;;;;;;;;
; INSERT
;;;;;;;;;;;;;;;;;;;;

; Given a queue of the form
; #front-pointer      #rear-pointer
;  |                   |
; (a, .) -> (b, .) -> (c, \)
; There are two ways to insert

;;;;;;;;;;
; Insert to the back

; So to insert a new element "d":
; 1. Create a new pair (d, \)
; 2. Make c point to d, so:
;   #front-pointer      #rear-pointer
;    |                   |
;   (a, .) -> (b, .) -> (c, .) -> (d, \)
; 3. Update rear-pointer so it points to d
;   #front-pointer                #rear-pointer
;    |                             |
;   (a, .) -> (b, .) -> (c, .) -> (d, \)

;;;;;;;;;;
; Insert to the front

; So to insert a new element "d":
; 1. Create a new pair (d, \)
; 2. Make d point to a, so:
;             #front-pointer      #rear-pointer
;              |                   |
;   (d, .) -> (a, .) -> (b, .) -> (c, \)
; 3. Update front-pointer so it points to d
;   #front-pointer                #rear-pointer
;    |                             |
;   (d, .) -> (a, .) -> (b, .) -> (c, \)

(define (insert-deque! queue item update-pointer)
  (let 
    ; Create new pair, whose car is the item to be
    ; inserted (this opeartion is the same for front or rear insert)
    ((new-pair (cons item '())))
    (cond 
      ; If the queue is empty, 
      ((empty-deque? queue)
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
        ; Check if the update-pointer is equal to update the
        ; rear of front pointer
        (if (equal? update-pointer set-rear-ptr!)
          ; If it is the rear, make the last element of the queue point to the new
          ; item
          (set-cdr! (rear-ptr queue) new-pair)
          ; Else, make the new item point to the first element of the queue
          (set-cdr! new-pair (front-ptr queue))
        )
        ; Update the front/rear pointer accoding to the operation specified
        (update-pointer queue new-pair)
      )
    )
  )
)

; Rear insert: specifies set-rear-ptr! as the update-pointer function
(define (rear-insert-deque! queue item)
  (insert-deque! queue item set-rear-ptr!)
)

; Front insert: specifies set-front-ptr! as the update-pointer function
(define (front-insert-deque! queue item)
  (insert-deque! queue item set-front-ptr!)
)


;;;;;;;;;;;;;;;;;;;;
; DELETE
;;;;;;;;;;;;;;;;;;;;

; Given a queue of the form
; #front-pointer      #rear-pointer
;  |                   |
; (a, .) -> (b, .) -> (c, \)
; There are two ways to delete

;;;;;;;;;;
; Delete from the back

; So to delete the element c from the back of the deque:
; 1. Update rear-pointer so it points to b
;   #f-ptr    #r-ptr
;    |         |
;   (a, .) -> (b, .) -> (c, \)

;;;;;;;;;;
; Delete from the front

; So to delete the element a from the front of the deque:
; 1. Update front-pointer so it points to b
;             #f-ptr    #r-ptr
;              |         |
;   (a, .) -> (b, .) -> (c, \)

(define (delete-deque! queue update-pointer obtain-element)
  (cond 
    ; Cannot delete from an empty queue
    ((empty-deque? queue)
      (error "DELETE! called with an empty queue" queue)
    )
    (else 
      ; Update front or rear pointer
      (update-pointer 
        queue 
        (obtain-element queue)  
      )
      ; (print (obtain-element queue))
      queue
    )
  )
)

(define (front-delete-deque! queue)
  (delete-deque! 
    queue set-front-ptr! 
    (lambda (q) (cdr (front-ptr queue)))
  )
)

(define (rear-delete-deque! queue)
  ; Obtain element before last, and remove the pointer between that element
  ; and the last one
  (define (get-last ptr last)
    ; If the current pointer is not equal to
    ; the rear pointer, we have not reached
    ; the last element
    (if (not (eq? ptr (rear-ptr queue)))
      ; Keep on going through the list
      (get-last (cdr ptr) ptr)
      (begin
        ; If the last element before the last is not null
        (if (not (null? last))
          ; Empty the pointer to this "last" element
          (set-cdr! last '())
          ; If the queue only has one element, so when we delete we want to delete
          ; the element pointed by the front-pointer
          (set-front-ptr! queue '())
        )
        ; Return last, that is the element before the last
        ; or if the queue only has one element the empty list: '()
        last
      )
    )
  )
  ; Call the generic delete queue
  (delete-deque! 
    queue
    ; We specify that we will update the rear pointer
    set-rear-ptr! 
    (lambda (q) 
      ; Function to return the element before the last
      (get-last (front-ptr q) '())
    )
  )
)

;; TEST
(define q (make-deque))
(front-insert-deque! q 'a)
(print-deque q)
; (a)
(front-insert-deque! q 'b)
(print-deque q)
; (b a)
(rear-insert-deque! q 'c)
(print-deque q)
; (b a c)
(front-delete-deque! q)
(print-deque q)
; (a c)
(rear-delete-deque! q)
(print-deque q)
; (a)
(rear-delete-deque! q)
(print-deque q)
; Empty
(rear-insert-deque! q 'c)
(print-deque q)
; (c)
(front-delete-deque! q)
(print-deque q)
; Empty
