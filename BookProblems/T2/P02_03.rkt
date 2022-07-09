#lang racket
(require berkeley)

; Import fibonacci function
(require "../T1/Racket/P02_01.rkt")

; Filtering a sequence to select only those elements that satisfy a given
; predicate

(define (filter predicate sequence)
  (cond 
    ; Finish when there are no more
    ; elements in the sequence to filter
    ((null? sequence) nil)
    ; If the current element satisfies the 
    ; predicate
    ((predicate (car sequence))
      ; Do not filter (add it to the resulting 
      ; filtered list)
      (cons 
        (car sequence)
        (filter predicate (cdr sequence))
      )
    )
    (else 
      ; If it does not satisfy the predicate
      ; remove it from the list, and keep
      ; going through the sequence
      (filter predicate (cdr sequence))
    )
  )
)

; (filter odd? (list 1 2 3 4 5))
; (1 3 5)

(define (accumulate op initial sequence)
  (if (null? sequence)
    ; When the list is empty, return the initial
    ; value
    initial
    ; Apply the operation to the first
    ; element of the sequence and the 
    ; result of the accumulation 
    ; on the rest of the sequence
    (op 
      (car sequence)
      (accumulate 
        op 
        initial 
        (cdr sequence)
      )
    )
  )
)

;(accumulate + 0 (list 1 2 3 4 5))
;; 15
;(accumulate * 1 (list 1 2 3 4 5))
;; 120
;(accumulate cons nil (list 1 2 3 4 5))
;; (1 2 3 4 5)

; Enumerate the sequence of elements to be processed

(define (enumerate-interval low high)
  (if (> low high)
    nil
    ; Append all values from low to high
    (cons 
      low 
      ; Update low and keep appending
      (enumerate-interval (+ low 1) high)
    )
  )
)

; (enumerate-interval 2 7)
; (2 3 4 5 6 7)


; Enumerate the leaves of the tree

(define (enumerate-tree tree)
  (cond 
    ; If the tree is empty
    ((null? tree) nil)
    ; If it is a leave
    ((not (pair? tree)) (list tree))
    (else 
      ; Concatenate
      (append 
        ; The leftmost branch
        (enumerate-tree (car tree))
        ; The rest of the tree
        (enumerate-tree (cdr tree))
      )
    )
  )
)

; (enumerate-tree (list 1 (list 2 (list 3 4)) 5))
; (1 2 3 4 5)


; Takes a tree as argument and computes the sum of the
; squares of the leaves that are odd

(define (sum-odd-squares tree)
  (accumulate
    ; op
    + 
    ; Initial value
    0 
    ; sequence
    (map 
      ; Function to apply to every element of 
      ; the sequence
      square 
      ; Sequence
      (filter 
        ; Predicate that filters the 
        ; sequence
        odd? 
        ; Sequence: leaves of the tree as a list
        (enumerate-tree tree)
      )
    )
  )
)


; Constructs a list of all the even Fibonacci numbers Fib(k), where
; k is less than or equal to a given integer n

(define (even-fibs n)
  (accumulate
    ; operation that accumulates
    ; elements of the sequence
    cons
    ; Initial value
    nil
    ; Sequence
    (filter 
      ; Predicate that filters the
      ; elements of the sequence
      even? 
      ; Sequece
      (map 
        ; Function that is applied to each
        ; element of the sequence
        fib 
        ; Sequence: all intergers from 0 to n
        (enumerate-interval 0 n)
      )
    )
  )
)

; Generate the sequence of all ordered pairs of positive integers less than 
; or equal to n, where for each i (1 <= i <= n) we generate a sequence where each
; j satisfies 1 <= j < i

;(accumulate
;  ; Function that accumulates each element
;  ; of the sequence
;  append 
;  ; Initial value
;  nil 
;  ; Sequence
;  (map 
;    ; Function to apply to each value
;    ; of the sequence
;    (lambda 
;      (i)
;      (map 
;        ; Function to apply to each value
;        ; of the sequence
;        (lambda 
;          (j) 
;          ; Make a pair of each i and j
;          (list i j)
;        )
;        ; Sequence: all integer from 1 to i-1 (j)
;        (enumerate-interval 1 (- i 1))
;      )
;    )
;    ; Sequence: all integers from 1 to n (i)
;    (enumerate-interval 1 n)
;  )
;)

; Combination of accumulate with append, will be used later
; instead of the definition of accumulate we did just now

(define (flatmap proc seq)
  (accumulate 
    ; operation to accumulate the elements of 
    ; the sequence
    append 
    ; Initial value
    nil 
    ; Sequence
    (map 
      ; Function to apply to each element of
      ; the sequence
      proc 
      ; Sequence
      seq
    )
  )
)

; Find those elements of the sequence whose sum is prime. The
; filter predicate is called for each element of the sequence; its argument is
; a pair and it must extract the integers from the pair. 

(define (prime-sum? pair)
  (prime? 
    ; Sum of the elements of the pair
    (+ (car pair) (cadr pair))
  )
)

; Generate the sequence of results by mapping over the filtered
; pairs using the following procedure, which constructs a triple consisting
; of the two elements of the pair along with their sum

(define (make-pair-sum pair)
  (list 
    ; First element of the pair
    (car pair) 
    ; Second element of the pair
    (cadr pair) 
    ; Sum of the elements of the pair
    (+ (car pair) (cadr pair))
  )
)

; Given a positive integer n, find all ordered pairs of distinct positive
; integers i and j, where 1 ≤ j < i ≤ n, such that i + j is prime
(define (prime-sum-pairs n)
  (map 
    ; Function to apply to each element of
    ; the sequence: make 3-tuple
    make-pair-sum
    ; Sequence
    (filter 
      ; Predicate to filter the elements of the sequence: 
      ; is the sum of the pair prime?
      prime-sum? 
      ; Accumulate with the accumulation operation: append
      (flatmap
        ; Function to apply to each element
        ; of the sequence
        (lambda 
          (i)
          (map 
            ; Function to apply to every element
            ; of the sequence
            (lambda 
              (j) 
              ; Make pair
              (list i j)
            )
            ; Sequence: all integers from 1 to i-1 (j)
            (enumerate-interval 1 (- i 1))
          )
        )
        ; Sequence: all integers from 1 to n (i)
        (enumerate-interval 1 n)
      )
    )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PERMUTATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Here is a plan for generating the permutations of S:
; For each item x in S, recursively generate the sequence of permutations
; of S − x, and and join x to the front of each one. This yields, for each x
; in S, the sequence of permutations of S that begin with x. Combining
; these sequences for all x gives all the permutations of S:

(define (permutations s)
  (if (null? s) ; empty set?
    (list nil) ; sequence containing empty set
    ; Accumulate with the append function as the 
    ; accumulator
    (flatmap 
      ; Function to apply to each element
      ; of the sequence
      (lambda 
        (x)
        (map 
          ; Function to apply to each element
          ; of the sequence
          (lambda 
            (p) 
            ; join x and p
            ; where p are all of the permutations
            ; of S - x
            (cons x p)
          )
          ; Sequence: permutations of S - x
          (permutations (remove x s)))
      )
      ; Sequence: set of elements to 
      ; get the permutations of
      s
    )
  )
)

; The remove procedure used in permutations returns
; all the items in a given sequence except for a given item. 

(define (remove item sequence)
  (filter 
    ; Function to filter each element
    ; of the sequence
    (lambda 
      (x) 
      ; Remove all elements 
      ; that are equal to x
      (not (= x item))
    )
    sequence
  )
)


; (permutations (list 1 2 3))
