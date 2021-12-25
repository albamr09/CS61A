#lang racket
(require berkeley)

; Sentences are a special case of lists, which are built out of pairs. Explore how that's done with experiments such as these:

(define x '(a (b c) d))
(car x)
(cdr x)
(car (cdr x))

