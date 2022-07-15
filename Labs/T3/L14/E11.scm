; Exercise 4: Do You Want to be the Very Best?

; You have access to a stream of all 744 pokemon data here and "~cs61as/lib/mapreduce/pokemon_data". streammapreduce.scm should load it automatically and 
; define the variable "data" as your input. 

(load "../../../lib/streammapreduce.scm")

; The key is the pokemon national number, the value is a list of regional number, name, name (yes it appears twice), and the rest are types that they have. 

; For example the first element is (1 1 bulbasaur bulbasaur grass poison) so it has the national number 1, regional number of 1, names bulbasaur, and has 
; the types 'grass' and 'poison'. A Pokemon can either have 1 or 2 types. Here is an example of one that only has one type: (4 4 charmander charmander fire). 
; You can take a look at the input by typing (ss data) in the interpreter after loading the files.

; Define the mapper, reducer and base-case such that calling mapreduce with (mapreduce mapper reducer base-case data) would return a list of key-value 
; pairs where the keys are different types, and the values represent how many times a pokemon of that type appears in the dataset. The final result should 
; yield the following (in any order):

; (
;    (grass . 86) 
;    (dragon . 39) 
;    (normal . 99) 
;    (flying . 93) 
;    (poison . 59) 
;    (ice . 35) 
;    (fire . 58) 
;    (ghost . 37) 
;    (psychic . 77) 
;    (electric . 47) 
;    (water . 124) 
;    (fairy . 35) 
;    (bug . 70) 
;    (steel . 42) 
;    (ground . 62) 
;    (rock . 54) 
;    (fighting . 45) 
;    (dark . 44)
; )

;loads data
(load "pokemon_data")


(define (type-amount)

  (define (mapper input-kv-pair)
    (let
      ((types (cdddr (kv-value input-kv-pair))))
      ; Create pair of (type number of times it appears -> 1)
      (map
        (lambda
          (type)
          (make-kv-pair
            type
            1
          )
        )
        types
      )
    )
  )

  ; Accumulate all values of each pokemon type
  (define (reducer num other-num)
  	(+ num other-num)
  )

  (mapreduce mapper reducer 0 data)
)

(define pokemon-types (type-amount))

(print (stream-ref pokemon-types 0))
(print (stream-ref pokemon-types 1))
(print (stream-ref pokemon-types 2))
(print (stream-ref pokemon-types 3))
(print (stream-ref pokemon-types 4))
(print (stream-ref pokemon-types 5))
(print (stream-ref pokemon-types 6))
(print (stream-ref pokemon-types 7))
(print (stream-ref pokemon-types 8))
(print (stream-ref pokemon-types 9))
(print (stream-ref pokemon-types 10))
(print (stream-ref pokemon-types 11))
(print (stream-ref pokemon-types 12))
(print (stream-ref pokemon-types 13))
(print (stream-ref pokemon-types 14))
(print (stream-ref pokemon-types 15))
(print (stream-ref pokemon-types 16))
(print (stream-ref pokemon-types 17))
