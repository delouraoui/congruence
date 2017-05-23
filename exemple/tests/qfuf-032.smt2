(set-logic QF_UF)
(set-info :status sat)
(declare-sort U 0)
(declare-fun c_3 () U)
(declare-fun c_0 () U)
(declare-fun c_1 () U)
(declare-fun p4 ( U U) Bool)
(assert 
 (and
  (p4 c_0 c_0)
   (not (p4 c_1 c_1))
    (or (= c_0 c_1) (= c_1 c_3))))
(check-sat)
(exit)
