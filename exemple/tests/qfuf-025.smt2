(set-logic QF_UF)
(set-info :status sat)
(set-info :smt-lib-version 2.0)
(set-info :category "crafted")
(set-info :source |
A small parser test
Pascal Fontaine
|)
(declare-fun p (Bool) Bool)
(assert true)
(assert (p true))
(check-sat)
(exit)
