#lang racket

(require berkeley)

; Write a predicate that takes three numbers as arguments, representing a month, a day of the month, and a year. Your procedure should return if
; the numbers represent a valid date (e.g., it isn’t the 31st of September). February has 29
; days if the year is divisible by 4, except that if the year is divisible by 100 it must also be
; divisible by 400.

(define (valid-date? month day year)
  (cond
    ; Year not negative or in the future
    ((or (< year 0) (> year 2021)) #f)
    ; Month between 1 and 12
    ((or (< month 1) (> month 12)) #f)
    ; Day not negative
    ((<= day 0) #f)
    ; 31 day on 30 day months
    ((and (= day 31) (member? month '(2 4 6 9 11))) #f)
    ; Año bisiesto si es divisible entre 400
    ((and (= month 2) (not (equal? (remainder year 400) 0)) (> day 28)) #f)
    (else #t)
  )
)

(valid-date? 9 31 1949)
; #F
(valid-date? 10 4 1949)
; #T
(valid-date? 20 4 1776)
; #F
(valid-date? 5 0 1992)
; #F
(valid-date? 2 29 1900)
; #F
(valid-date? 2 29 2000)
; #T
