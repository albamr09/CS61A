#lang racket
(require berkeley)

; Import packages
(require "../../../../BProblems/T2/P05_01/complex-pkg.rkt")
(require "../../../../BProblems/T2/P05_01/rational-pkg.rkt")
(require "./scheme-number-pkg.rkt")

(define (install-arithmetic-pkg)
  (install-complex-package)
  (install-scheme-number-package)
  (install-rational-package)
  'done
)

; Exports 
(provide install-arithmetic-pkg)
