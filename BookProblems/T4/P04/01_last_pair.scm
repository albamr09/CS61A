; Find last pair of list

; This initializes the interpreter
(load "../../../lib/query.scm")
(initialize-data-base microshaft-data-base)
(query-driver-loop)

; Copy the following rules to the terminal

; Base case, the list only has one element
(assert!
  (rule
    ; The only element of the list is the last element
    (last-pair (?x . ()) ?x)
  )
)

; Recursive case
(assert!
  (rule
    ; Split list into x=first element and y=rest of list
    ; z stores last pair
    (last-pair (?x . ?y) ?z)
    ; Continue searching through rest of list, y
    (last-pair ?y ?z)
  )
)

;;;;;;;;;;;;;;;;;;;;,
; TEST
; (last-pair (3) ?x) 
; (last-pair (1 2 3) ?x)
; (last-pair (2 ?x) (3))

; It does not work on (last-pair ?x (3))
