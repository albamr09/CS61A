#lang racket
(require berkeley)


; Import packages: each of them have their own equ? procedure
(require "./complex-pkg.rkt")
(require "./rational-pkg.rkt")
(require "./scheme-number-pkg.rkt")

; Insert the operation in the operation-type table
(define (install-arithmetic-pkg)
  (install-complex-package)
  (install-scheme-number-package)
  (install-rational-package)
  'done
)

; Exports
(provide install-arithmetic-pkg)
