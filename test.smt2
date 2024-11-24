(declare-const T_545 Int) (assert (>= T_545 0))
(declare-const T_544 Int) (assert (>= T_544 0))
(declare-const T_543 Int) (assert (>= T_543 0))
(declare-const T_542 Int) (assert (>= T_542 0))
(declare-const T_541 Int) (assert (>= T_541 0))
(declare-const T_540 Int) (assert (>= T_540 0))

(assert (= 0 T_540))
(assert (= 0 (+ T_540 T_541 T_542 (- 1) (- T_543))))
(assert (= 0 (+ T_541 (* 2 T_542) (- T_544))))
(assert (= 0 (+ T_542 (- T_545))))

(minimize T_542)
(minimize T_541)
(minimize T_540)

(check-sat)

(echo "P(X) = T_542 * X^2 + T_541 * X + T_540")
(get-objectives)
