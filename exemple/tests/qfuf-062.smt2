(set-logic QF_UF)
(set-info :status sat)
(declare-sort U 0)
(declare-fun a () U)
(declare-fun b () U)
(declare-fun c () U)
(declare-fun d () U)
(declare-fun p (U U) Bool)
(declare-fun f (U U) U)
(assert (not (p b b)))
(assert (= d (f (f b c) a)))
(assert (= d b))
(check-sat)
(exit)