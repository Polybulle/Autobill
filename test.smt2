(declare-const T_563 Int) (assert (<= 0 T_563))
(declare-const T_562 Int) (assert (<= 0 T_562))

(assert
 (and
  (= 0 T_562)
  (= 0 (+ T_562 T_563 (- T_562) (- 1)))
  (= 0 (+ T_563 (- T_563)))))

(minimize T_563)
(minimize T_562)

(check-sat)
(get-objectives)
