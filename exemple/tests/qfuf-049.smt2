(set-logic QF_UF)
(set-info :status sat)
(set-info :smt-lib-version 2.0)
(set-info :category "unknown")
(declare-sort U 0)
(declare-sort I 0)
(declare-fun e4 () I)
(declare-fun e0 () I)
(declare-fun e3 () I)
(declare-fun op (I) I)
(declare-fun e1 () I)
(assert
  (and
    (not (= e1 e0))
    (and
      true
      (not (= e0 e1))
      )))
(check-sat)
(exit)
