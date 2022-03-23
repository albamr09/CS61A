;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 4.1.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SICP 3.16 Draw the 4 box-and-pointer diagrams.

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ 
      (count-pairs (car x))
      (count-pairs (cdr x))
      1
    )
  )
)


; What the procedure does:

(print (count-pairs (list (list 1 2) (list 3 4) (list 5 6))))

; > (count-pairs ((1 2) (3 4) (5 6)))
; >> (count-pairs (car ((1 2) (3 4) (5 6)))) = (count-pairs (1 2))
; -------------------------------
; CAR OF ((1 2) (3 4) (5 6))
; -------------------------------
; >>> (count-pairs (car (1 2))) = (count-pairs 1)
; <<< 0 = car of (1 2)
; >>> (count-pairs (cdr (1 2))) = (count-pairs (2))
; >>>> (count-pairs (car (2))) = (count-pairs 2)
; <<<< 0 = (car (2))
; >>>> (count-pairs (cdr (2))) = (count-pairs ())
; <<<< 0 = (cdr (2))
; <<<< (car (2)) + (cdr (2)) + 1 = 1 = cdr of (1 2)
; <<< (car (1 2) + cdr (1 2)) + 1 = 1 + 1 = 2 = car of ((1 2) (3 4) (5 6))
; -------------------------------
; CDR OF ((1 2) (3 4) (5 6))
; -------------------------------
; >> (count-pairs (cdr ((1 2) (3 4) (5 6)))) = (count-pairs ((3 4) (5 6)))
; >>> (count-pairs (car ((3 4) (5 6)))) = (count-pairs (3 4))
; >>>> (count-pairs (car (3 4))) = (count-pairs 3)
; <<<< 0 = car of (3 4)
; >>>> (count-pairs (cdr (3 4))) = (count-pairs (4))
; >>>>> (count-pairs (car (4))) = (count-pairs 4)
; <<<<< 0 = (car (4))
; >>>>> (count-pairs (cdr (4))) = (count-pairs ())
; <<<<< 0 = (cdr (4))
; <<<<< (car (4)) + (cdr (4)) + 1 = 0 + 0 + 1 = cdr of (3 4)
; <<<< (car (3 4) + cdr (3 4)) + 1 = 1 + 1 = 2 = (car ((3 4) (5 6)))
; -------
; >>> (count-pairs (cdr ((3 4) (5 6)))) = (count-pairs ((5 6)))
; >>>> (count-pairs (car ((5 6)) = (count-pairs (5 6))
; >>>>> (count-pairs (car (5 6))) = (count-pairs 5)
; <<<<< 0 = (car (5 6))
; >>>>> (count-pairs (cdr (5 6))) = (count-pairs (6))
; >>>>>> (count-pairs (car (6))) = (count-pairs 6)
; <<<<<< 0 = (car (6))
; >>>>>> (count-pairs (cdr (6))) = (count-pairs ())
; <<<<<< 0 = (cdr (6))
; <<<<<< (car (6)) + (cdr (6)) + 1 = 0 + 0 + 1 = (cdr (5 6))
; <<<< (car (5 6)) + (cdr (5 6)) + 1 = 0 + 1 + 1 = 2 = (car ((5 6)))
; >>>> (count-pairs (cdr ((5 6)) = (count-pairs ())
; <<<< 0 =  (cdr ((5 6)))
; <<< (car ((5 6))) + (cdr ((5 6))) + 1 = 2 + 0 + 1 = 3 = (cdr ((3 4) (5 6)))
; -------
; << (car ((3 4) (5 6))) + (cdr ((3 4) (5 6))) + 1 = 2 + 3 + 1 = 6 = (cdr ((1 2) (3 4) (5 6)))
; -------------------------------
; RESULT
; -------------------------------
; < (car ((1 2) (3 4) (5 6)))+ (cdr ((1 2) (3 4) (5 6))) + 1 = 2 + 6 + 1 = 9
		 
; a. Returns 3:

(print (count-pairs (list (list 1 2))))

; [count] = [count (car ((1 2)))] + [count (cdr ((1 2)))] + 1 = 2 + 0 + 1 = 3
; [count (car ((1 2)))] = [count (1 2)] = [count (car (1 2))] + [count (cdr (1 2))] + 1 = 0 + 1 + 1 = 2
; [count (cdr ((1 2)))] = [count ()] = 0
; [count (car (1 2))] = [count 1] = 0
; [count (cdr (1 2))] = [count (2)] = [count (car 2)] + [count (cdr 2)] + 1 = 0 + 0 + 1 = 1
; [count (car 2)] = [count 2] = 0
; [count (cdr 2)] = [count ()] = 0

; b. Returns 4:

(print (count-pairs (list (list 1 (list 2)))))

; [count] = [count (car ((1 (2))))] + [count (cdr ((1 (2))))] + 1 = 3 + 0 + 1 = 4
; [count (cdr ((1 (2))))] = [count ()] = 0
; [count (car ((1 (2))))] = [count (1 (2))] = [count (car (1 (2)))] + [count (cdr (1 (2)))] + 1 = 0 + 2 + 1 = 3
; [count (car (1 (2)))] = [count (1)] = 0
; [count (cdr (1 (2)))] = [count (car ((2)))] + [count (cdr ((2)))] + 1 = 1 + 0 + 1 = 2
; [count (car ((2)))] = [count (car (2))] + [count (cdr (2))] + 1 = 0 + 0 + 1 = 1
; [count (car (2))] = [count 2] = 0
; [count (cdr (2))] = [count 2] = 0
; [count (cdr ((2)))] = [count ()] = 0

; c. Returns 7: Using the results from the two examples above

(print (count-pairs 
         (list 
          ; by a
          (list 1 2) 
          ; by b
          (list 1 (list 2)))
        )
)

; d. Never returns:

(define x (list 1 2))

; Infinite list: it concatenates with itself forever
(set-cdr! x x)

; Segmentation fault
(count-pairs x)
