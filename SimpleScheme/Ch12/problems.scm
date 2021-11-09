; Procedure to reverse the letters of a word: 

(define (reverse wd)
  (if (= (count wd) 0)
    ; Empty letter
    ""
    ; Remove the last letter and reverse the rest of the word
    (word (last wd) (reverse (bl wd)))
  )
)

(reverse 'beatles)
; seltaeb
(reverse '"")

; Write the factorial of a number using recursion
(define (factorial n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))
  )
)

(factorial 1)
; 1
(factorial 2)
; 2
(factorial 3)
; 6
(factorial 4)
; 24

(define (downup wd)
  (if (= (count wd) 1)
    wd
    (se wd (downup (bl wd)) wd)
  )
)

(downup 'paul)
; (PAUL PAU PA P PA PAU PAUL)

(define (down wd)
  (if (= (count wd) 1)
    wd
    (se wd (down (bl wd)))
  )
)

(down 'town)
; (TOWN TOW TO T)

; Procedure that takes a sentence as its argument and returns a
; sentence of the even-numbered words of the original sentence

(define (evens sent)
  (if (<= (count sent) 1)
    '()
    (se 
      (item 2 sent) 
      (evens (bf (bf sent)))
    )
  )
)

(evens '(i want to hold your hand))
; (WANT HOLD HAND)
(evens '(want to hold your hand))
; (TO YOUR)
(evens '(i want to hold your))
; (WANT HOLD)
(evens '(i want to hold))
; (WANT HOLD)

; Procedure that squares all of the numbers of a sentence

(define (square-sent sent)
  (if (empty? sent)
    '()
    (se (square (first sent)) (square-sent (bf sent)))
  )
)

(square-sent '(2 3))
; (4 9)
(square-sent '(7))
