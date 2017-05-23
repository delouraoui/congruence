(set-logic QF_UF)
(set-info :status sat)
(declare-sort U 0)
(declare-fun c_2 () U)
(assert 
 (let ((?n1 false))
 (let ((?n2 (= c_2 c_2))) 
 (let ((?n3 (not ?n1))) 
 (let ((?n4 (or ?n1 ?n1 ?n2 ?n3))) ?n4
)))))
(check-sat)
(exit)
