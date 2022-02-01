#lang racket
(require berkeley)

; Import packages
(require "./complex-pkg.rkt")
(require "./rational-pkg.rkt")
(require "./scheme-number-pkg.rkt")

(define (install-arithmetic-pkg)
  (install-complex-package)
  (install-scheme-number-package)
  (install-rational-package)
  'done
)

; Exports 
(provide install-arithmetic-pkg)
