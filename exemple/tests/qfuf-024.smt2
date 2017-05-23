(set-logic QF_UF)
(set-info :status unsat)
(set-info :smt-lib-version 2.0)
(set-info :category "crafted")
(set-info :source |
A small soundness check for Booleans as arguments of functions (SMT-LIB 2.0 feature)
Pascal Fontaine
|)
(declare-sort U 0)
(declare-fun p (Bool) Bool)
(declare-fun a () Bool)
(declare-fun b () Bool)
(declare-fun c () Bool)
(assert (and (p a)
	  (p b)
	  (not (p c))
	  a
	  (not b)))
(check-sat)
(exit)
