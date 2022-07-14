; Consider the problem of transferring an amount from one account to another. 
; Ben Bitdiddle claims that this can be accomplished with the following procedure, even if
; there are multiple people concurrently transferring money among multiple accounts, using any account mechanism
; that serializes deposit and withdrawal transactions, for example, the version of make-account in the text above.

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount)
)

; Louis Reasoner claims that there is a problem here, and
; that we need to use a more sophisticated method, such as
; the one required for dealing with the exchange problem. Is
; Louis right? If not, what is the essential difference between
; the transfer problem and the exchange problem? (You should
; assume that the balance in from-account is at least amount.)

; The main difference is we are not computing the value "difference" between the accounts. Which is the variable that may change in between the procedure exchange. However in this case we simply reduce the balance of an account by "amount" and then we add it to the other account's "balance". 

; Both these procedures are serialized therefore there will be no inconsistencies in the result. That is, it does not matter if in between these procedures some other changes are made to the balance because:

; 1. We are extracting "atomically" from the first account and that's it.
; 2. Any other procedure might take place and change the balance of any of the two accounts.
; 3. Then we add the amount "atomically" to the other account and that's it.
