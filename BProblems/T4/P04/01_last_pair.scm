(assert! 
  (rule 
    (last-pair (?y . ?x) ?z)
    (last-pair ?x ?z)
  ) 
)

(assert! 
  (rule 
    (last-pair (?x) ?x)
  ) 
)
