(set-logic QF_UF)
(set-info :status sat)
(set-info :source | Benchmark generated by veriT |)
(set-info :smt-lib-version 2.0)
(declare-sort U 0)
(declare-fun d () U)
(declare-fun c () U)
(declare-fun a () U)
(declare-fun b () U)
(assert (and (and (= b a) (= b c) (= c d) (= a d)))
)
(check-sat)
(exit)