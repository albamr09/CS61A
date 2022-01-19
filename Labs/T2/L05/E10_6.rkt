#lang racket
(require berkeley)

; Consider the encoding procedure that you designed in Exercise 2.68 (E10_2). What is the order of growth in
; the number of steps needed to encode a symbol? 

; Consider the special case where the relative frequencies of then symbols are as described in 
; Exercise 2.71 (E10_5), and give the order of growth (as a function of n) of the number of steps needed 
; to encode the most frequent and least frequent symbols in the alphabet.

; -------------------------------------
; Given a string s of lenght t, and an alphabet of n symbols.
; where the frecuency of the ith symbol is 2^{i-1}

; If s = the most frequent symbol then, O(n) = 1
; If s = the least frequent symbol then, O(n) = 2^{n-1}

; So in general, O(n) = t*(2^{n-1}) = 2^{n-1}, for sufficiently large n
; that is to say O(n) = the number os steps needed to encode the least frequent element
; --------------------------------------
