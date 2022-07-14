; Exercise 4: Do You Want to be the Very Best?

; You have access to a stream of all 744 pokemon data here and "~cs61as/lib/mapreduce/pokemon_data". streammapreduce.scm should load it automatically and 
; define the variable "data" as your input. 

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
