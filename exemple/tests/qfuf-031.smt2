(set-logic QF_UF)
(set-info :status sat)
(declare-sort U 0)
(declare-fun p4 ( U U) Bool)
(declare-fun f5 ( U U) U)
(declare-fun c16 () U)
(declare-fun c15 () U)
(assert 
 (let ((?n1 (f5 c15 c16))) 
 (let ((?n2 (p4 c15 ?n1))) ?n2
)))
(check-sat)
(exit)
