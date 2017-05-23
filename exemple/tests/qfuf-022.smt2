(set-logic QF_UF)
(set-info :status unsat)
(set-info :smt-lib-version 2.0)
(set-info :category "crafted")

(declare-sort U 0)

(declare-fun f (U) U)
(declare-fun a () U)
(assert (not (= (f (f (f (f (f a))))) (f (f a)))))
(assert (= (f a) a))
(check-sat)
(exit)

