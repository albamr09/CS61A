;;;;;;;;;;;;;;;;;;;;;
;; SERIALIZERS
;;;;;;;;;;;;;;;;;;;;;

;; This is a procedure that takes as its argument another procedure (call it proc). 
;; The serializer returns a new procedure (call it protected-proc). When invoked, protected-proc invokes proc, 
;; but only if the same serializer is not already in use by another protected procedure. proc can have any number of arguments, 
;; and protected-proc will take the same arguments and return the same value.

(define (make-serializer)
  (let 
    ; Control variable
    ((mutex (make-mutex)))
    (lambda 
      ; Given a procedure p
      (p)
      ; Obtain arguments
      (define (serialized-p . args)
        ; When calling procedure, lock mutex 
        (mutex 'acquire)
        ; Execute procedure
        (let ((val (apply p args)))
          ; Unlock mutex
          (mutex 'release)
          ; Return result
          val
        )
      )
      ; Return serialized procedure
      serialized-p
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;
;; MUTEX
;;;;;;;;;;;;;;;;;;;;;


;; A mutex is an object that supports two operations -- the mutex can be acquired, and the mutex can be released. 
;; Once a mutex has been acquired, no other acquire operations on that mutex may proceed until the mutex is released.

(define (make-mutex)
  (let ((cell (list false)))
    ; Handle mutex-state with message passing
    (define (the-mutex m)
      (cond 
        ; If we want to lock the mutex
        ((eq? m 'acquire)
          ; Check if it has not been locked, the check includes
          ; 1. Is the cell true=locked?
          ; 1.1. If so, return true
          ; 1.2. If not, set the cell to be true and return false
          (if (test-and-set! cell)) 
            ; If the test returned true, tray acquiring again
            (the-mutex 'acquire)
        )
        ; If we want to unlock
        ((eq? m 'release) 
          (clear! cell))
        )
    )
    the-mutex
  )
)

(define (clear! cell) (set-car! cell false))

; Manage mutex state
(define (test-and-set! cell)
  (if (car cell) 
    true 
    (begin (set-car! cell true) false)
  )
)
