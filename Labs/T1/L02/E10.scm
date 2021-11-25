; Our Scheme library provides versions of the every function from the last exercise and 
; the keep function shown in lecture. Get familiar with these by trying examples such 
; as the following:
 
(every 
  ; Duplicates a letter
  (lambda (letter) (word letter letter)) 
  ; Argument
  'purple
)
;; (pp uu rr pp ll ee)

(every 
  (lambda 
    ; Formal parameter
    (number) 
    ; If the number is even duplicate it
    (if (even? number) 
      (word number number) 
      ; Else no change
      number
    )
  ) 
  ; Argument
  '(781 5 76 909 24)
) 
;; '(781 5 7676 909 2424)

; Removes odd numbers
(keep even? '(781 5 76 909 24)) 
;; '(76 24)

(keep 
  ; Anonymous function
  (lambda 
    ; Formal parameter
    (letter) 
    ; If the letter is a vowel keep
    (member? letter 'aeiou)
  ) 
  ; Argument
  'bookkeeper
) 
;; ooeee

(keep 
  (lambda 
    ; Formal parameter
    (letter) 
    ; If the letter is a vowel keep
    (member? letter 'aeiou)
  ) 
  ; Argument
  'syzygy
) 
;; ""

(keep 
  ; Anonymous function
  (lambda 
    ; Formal parameter
    (letter) 
    ; If letter is a vowel keep 
    (member? letter 'aeiou)
  ) 
  ; Argument
  '(purple syzygy)
) 
;; Error word cannot be member of word

(keep 
  ; Anonymous function
  (lambda 
    ; Formal parameter
    (wd) 
    ; If 'e is in the word keep
    (member? 'e wd)
  ) 
  ; Argument
  '(purple syzygy)
)
;; (purple)
