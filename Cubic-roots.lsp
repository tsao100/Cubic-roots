;(setq a 1.0 b 3.0 c 5.0 d 7.0)

(defun complex-add (c1 c2)
  ;"Adds two complex numbers represented as (real imaginary)."
  (list (+ (car c1) (car c2)) (+ (cadr c1) (cadr c2))))

(defun complex-sub (c1 c2)
  ;"Subtracts two complex numbers represented as (real imaginary)."
  (list (- (car c1) (car c2)) (- (cadr c1) (cadr c2))))

(defun complex-mul (c1 c2)
  ;"Multiplies two complex numbers."
  (setq real-part (- (* (car c1) (car c2)) (* (cadr c1) (cadr c2))))
  (setq imag-part (+ (* (car c1) (cadr c2)) (* (cadr c1) (car c2))))
  (list real-part imag-part))

(defun complex-div (c1 c2)
  ;"Divides two complex numbers."
  (setq denom (+ (* (car c2) (car c2)) (* (cadr c2) (cadr c2))))
  (setq num (complex-mul c1 (list (car c2) (- (cadr c2)))))
  (if (= denom 0.0) (setq denom 1e-200))
  (list (/ (car num) denom) (/ (cadr num) denom)))

(defun complex-root (c n)
  ;"Finds the nth root of a complex number using De Moivre¡¦s Theorem."
  (setq r (sqrt (+ (* (car c) (car c)) (* (cadr c) (cadr c)))))
  (setq theta (atan (cadr c) (car c)))
  (setq new-r (expt r (/ 1.0 n)))
  (setq new-theta (/ theta n))
  (list (* new-r (cos new-theta)) (* new-r (sin new-theta))))

(defun quadratic-roots (a b c)
  ;"Finds the roots of a quadratic equation ax^2 + bx + c = 0."
  (setq disc (- (* b b) (* 4 a c)))
  (setq sqrt-disc (complex-root (list disc 0) 2))
  (list (complex-div (complex-sub (list (- b) 0) sqrt-disc) (list (* 2 a) 0))
        (complex-div (complex-add (list (- b) 0) sqrt-disc) (list (* 2 a) 0))))

(defun depress-cubic (coefficients)
  ;"Depresses the cubic equation."
  (setq a (nth 3 coefficients))
  (setq b (nth 2 coefficients))
  (setq c (nth 1 coefficients))
  (setq d (nth 0 coefficients))
  (setq shift (/ b (* 3 a)))
  (setq H (/ (- c (/ (* b b) (* 3 a))) (* 3 a)))
  (setq G (/ (+ (/ (* 2 (expt b 3)) (* 27 a a)) (- (/ (* b c) (* 3 a))) d) a))
  (list (list (/ G a) (/ (* 3 H) a) 0 1) H G shift))

(defun cardano-method (H G shift)
  ;"Finds the roots of the cubic equation using Cardano¡¦s method."
  (setq roots (quadratic-roots 1 G (- (expt H 3))))
  (setq u (nth 0 roots))
  (setq v (nth 1 roots))
  (setq first (complex-root u 3))
  (setq second (complex-div (list (- H) 0) (complex-root u 3)))
  (setq omega (list -0.5 (/ (sqrt 3) 2)))
  (setq omegaSq (list -0.5 (- (/ (sqrt 3) 2))))
  (list (complex-sub (complex-add first second) (list shift 0))
        (complex-sub (complex-add (complex-mul omega first) (complex-mul omegaSq second)) (list shift 0))
        (complex-sub (complex-add (complex-mul omegaSq first) (complex-mul omega second)) (list shift 0))))

(defun cubic-roots (a b c d)
  ;"Solves the cubic equation ax^3 + bx^2 + cx + d = 0."
  (if (= a 0)
      (quadratic-roots b c d)
      (progn
        (setq depressed (depress-cubic (list d c b a)))
        (setq H (nth 1 depressed))
        (setq G (nth 2 depressed))
        (setq shift (nth 3 depressed))
        (cardano-method H G shift))))



;(cubic-roots 1 3.0 5 7) use real nnn.0 will be better.
;|
(setq roots (cubic-roots 1 -6.0 11 -6))  ;; Solves x3 - 6x2 + 11x - 6 = 0
(princ roots)  ;; Expected Output: (1 2 3)
|;
