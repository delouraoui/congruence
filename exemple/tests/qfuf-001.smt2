(set-logic QF_UF)
(set-info :status unsat)
(set-info :smt-lib-version 2.0)
(set-info :category "crafted")
(declare-sort U 0)
(declare-fun p (U) Bool)
(declare-fun a () U)
(declare-fun b () U)
(declare-fun c () U)
(assert
  (and
    (= a c)
    (= c b)
    (p a)
    (not (p b))))
(check-sat)
(exit)

