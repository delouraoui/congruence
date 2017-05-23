(set-logic QF_UF)
(set-info :status unsat)
(set-info :source | Benchmark generated by veriT |)
(set-info :smt-lib-version 2.0)
(declare-sort U 0)
(declare-sort I 0)
(declare-fun op (I I) I)
(declare-fun a () I)
(assert (= (op a (op a a)) (op a a)))
(assert (= (op (op a a) a) a))
(assert (or
	  (= (op a (op a a)) a)
	  (= a (op a a))))
(assert
  (or
    (not (= a (op a a)))
    (not (= a (op (op a a) (op a a))))
    (not (= (op a a) (op (op a a) a)))
    (not  (= (op a (op (op a a) a)) a))
    ))
(check-sat)
(exit)
