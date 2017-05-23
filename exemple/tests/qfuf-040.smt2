(set-logic QF_UF)
(set-info :status sat)
(set-info :source | Benchmark generated by veriT |)
(set-info :smt-lib-version 2.0)
(declare-sort U 0)
(declare-fun f5 (U U) U)
(declare-fun c_3 () U)
(declare-fun c_2 () U)
(declare-fun c_1 () U)
(declare-fun p9 (U U) Bool)

(assert (= (f5 c_3 c_2) c_2))
(assert (p9 c_1 c_2))
(assert (= (f5 c_2 c_3) c_1))
(assert (= c_2 c_3))
(assert (p9 c_1 c_3))

(check-sat)
(exit)
