;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E01_07
; SICP 4.14
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Eva Lu Ator and Louis Reasoner are each experimenting with the metacircular 
; evaluator. Eva types in the definition of map, and runs some test programs 
; that use it. ey work fine. Louis, in contrast, has installed the system version 
; of map as a primitive for the metacircular evaluator. When he tries it, things 
; go terribly wrong. Explain why Louis’s map fails even though Eva’s works.

; map is not a primitive procedure, it has its own evaluation rules that we have not
; implemented, so when we:

; (1) Evaluate: we treat it as an application so we apply it with my-apply, where
;				- operator = map						-> map procedure
;				- operands = (fn arguments) -> evaluate recurively fn and arguments
;					- fn: yields a procedure
;					- arguments: yiels a list
; (2) In apply we treat it as a primitive procedure and so we call apply-primitive-procedure with:
;				- procedure = map procedure
;				- arguments = ((fn procedure) (list of arguments))
; (3) In apply-primitive-procedure we call apply-in-underlying-scheme(=apply) with
;				- (primitive-implementation proc) = map procedure
;				- args = ((fn procedure) (list of arguments))
; (4) So apply tries to apply the map with the (fn procedure) to the list of arguemnts, but the 
;			(fn procedure) has our syntax, which may not relate with scheme's syntax. For example for a defined
;			procedure it would be:
;			(procedure <procedure name>)
;			For a primitive procedure it would have the form:
;			(primitive <primitive name>)
;			And apply does not know what to do with it.
