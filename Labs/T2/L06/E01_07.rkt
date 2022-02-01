#lang racket
(require berkeley)

; Louis Reasoner has noticed that apply-generic
; may try to coerce the arguments to each other’s type even
; if they already have the same type. Therefore, he reasons,
; we need to put procedures in the coercion table to coerce
; arguments of each type to their own type. For example, in
; addition to the scheme-number->complex coercion shown
; above, he would do:
