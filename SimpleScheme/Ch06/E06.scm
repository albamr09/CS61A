; Write a predicate that returns true if its argument is between 13 and 19

(define (teen? age)
  (
    and (>= age 13) (<= age 19)
  )
)

(teen? 15)
(teen? 12)
(teen? 19)
(teen? 13)
(teen? 8)
(teen? 50)
