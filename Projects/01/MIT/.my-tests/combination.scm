(define (combination-n-from-k lst n k i comb)
  (cond
    ; Prevent overflow of the index
    ; if n + offset is > the size of the list return all the combinations
    ; up until this point
    ((< (count lst) (+ n (- i 1))) 
     comb
    )
    (else 
      (combination-n-from-k
        ; Complete list
        lst
        ; Size of combination
        n
        ; Where to start in the list
        k
        ; Offset to append, 1 refers to second element in the list
        (+ i 1)
        ; Append combinations
        (se 
          ; Current combinations
          comb
          ; First item in the list
          (item k lst) 
          ; Sublist from offset i, to n + (i - 1)
          ; where n is the size of the subset
          ; and i is the offset
          (sublist 
            lst 
            i 
            (+ (- i 1) n)
          )
        )
      )
    )
  )
)

(define (combination-n-helper lst n k comb)
  (cond 
    ; Prevent overflow of the offset
    ; if n + offset > size of the list return all the combinations
    ; up until this point
    ((< (count lst) (+ n (- k 1))) comb)
    (else
      (combination-n-helper
        lst
        n
        ; Update where to start the combinatios
        (+ k 1)
        ; Append the combinations
        (se
          ; Old combinations
          comb
          ; New combinatinons
          (combination-n-from-k lst n k k '())
        )
      )
    )
  )
)

(define (combination-n lst n)
  (cond
    ((= n 1) lst)
    (else
      (combination-n-helper lst n 1 '())
    )
  )
)

(define (combination-helper lst n comb)
  (if (= (count lst) n)
    comb
    (combination-helper
      lst
      (+ 1 n)
      (se
        comb
        (combination-n lst n)
      )
    )
  )
)

(define (combination lst)
  (combination-helper lst 1 '())
)

(combination-n '(1 2 3) 2)
(combination-n '(1 2 3) 1)
(combination-n '(1 2 3) 3)
(combination-n '(1 2 3 4) 3)
(combination-n '(1 2 3 4) 4)
(combination-n '(1 2 3 4) 2)
(combination '(1 2 3))
