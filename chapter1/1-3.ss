(load "fermat.scm")

; 1.3.1
;--------------------------------------------------

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc x) (+ x 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;-------------------------
;; 1.29
;; Simpson's Rule.
;-------------------------

(define (integral-simpson f a b n)
  (define h (/ (- a b) n))
  (define (y-k k)
    (f (+ a (* k h))))
  (define (s-term k)
    (cond ((or (= k 0) (= k n)) (y-k k))
          ((odd? k) (* 4 (y-k k)))
          (else (* 2 (y-k k)))))
  (* (sum s-term 0 inc n)
     (/ h 3.0)))

;-------------------------
;; 1.30
;; Iterative sum.
;-------------------------

(define (sum-iter term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (+ (term x) result))))
  (iter a 0))

;-------------------------
;; 1.31.a
;; Product.
;-------------------------

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-wallis n)
  (define adjusted-n
    (if (even? n)
        (+ n 1)
        n))
  (define (add-2 x)
    (+ x 2.0))
  (define (num-term x)
    (* x (add-2 x)))
  (define num-product
    (product num-term 2 add-2 adjusted-n))
  (define den-product
    (product square 3 add-2 adjusted-n))
  (/ num-product den-product))

;-------------------------
;; 1.31.b
;; Iterative product.
;-------------------------

(define (product-iter term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (* (term x) result))))
  (iter a 1))

;-------------------------
;; 1.32.a
;; Accumulate.
;-------------------------

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (add x y) (+ x y))
(define (mul x y) (* x y))

(define (accum-sum term a next b)
  (accumulate add 0 term a next b))

(define (accum-product term a next b)
  (accumulate mul 1 term a next b))

;-------------------------
;; 1.32.b
;; Iterative accumulate.
;-------------------------

(define (accum-iter combiner null-value term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x)
              (combiner x result))))
  (iter a null-value))

;-------------------------
;; 1.33
;; Accumulate with filter.
;-------------------------

(define (filtered-accumulate combiner filter null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                    (filtered-accumulate combiner filter null-value
                                         term (next a) next b))
          (filtered-accumulate combiner filter null-value term (next a) next b))))

;; 1.33.a
;------------

(define (prime-square-sum a b)
  (define (prime? x) (faster-prime? x 50))
  (filtered-accumulate add prime? 0 square a inc b))

;; 1.33.b
;------------

(define (relative-prime-product n)
  (define (relative-prime? x) (= 1 (gcd x n)))
  (filtered-accumulate mul relative-prime? 1 identity 2 inc n))

;-------------------------
;; 1.34
;; A perverse situation.
;;
;; Calling (f f) fails, since 2 is not a procedure.
;-------------------------

(define (f g)
  (g 2))

;; Fixed point.
;-------------------------

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (> tolerance
       (abs (- v1 v2))))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (begin
            (newline)
            (display guess)
            (try (/ (+ guess next) 2)))))) ; average damping
  (try first-guess))

;-------------------------
;; 1.35
;; Golden ratio.
;-------------------------

(define (golden-ratio x)
  (fixed-point (lambda (x) (+ 1 (/ 1.0 x)))
               1.0))

;-------------------------
;; 1.36
;; x^x = 1000
;;
;; Steps taken without average damping = 33.
;; Steps taken using average damping = 9.
;; Tested with initial guess = 2.0, x = 10.
;-------------------------

(define (x-to-the-x x)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               2.0))

;-------------------------
;; 1.37.a
;; Continued fractions.
;;
;; For k = 10, product of phi and gr-frac gives 1.000035.
;-------------------------

(define (cont-frac n d k)
  (define (level i)
    (let ((n-value (n i))
          (d-value (d i)))
      (if (= i k)
          (/ n-value d-value)
          (/ n-value (+ d-value
                        (level (inc i)))))))
  (level 1))

(define (gr-frac k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

;-------------------------
;; 1.37.b
;; Continued fractions (iterative).
;-------------------------

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k (/ (n k) (d k))))

;-------------------------
;; 1.38
;; Euler's approximation of e.
;-------------------------

(define (e-frac k)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  (lambda (i) (if (= (remainder (- i 1) 3) 1)
                                  i
                                  1))
                  k)))

;-------------------------
;; 1.39
;; Lambert's approximation of tan.
;-------------------------

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             (expt x i)
                             (* -1 (expt x i))))
             (lambda (i) (- (* i 2) 1))
             k))

;; Newton's method
;-------------------------

(define (average a b)
  (/ (+ a b) 2.0))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x))
                 dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x)
                      ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;-------------------------
;; 1.40
;; Cubic function.
;-------------------------

(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

;-------------------------
;; 1.41
;; Double composition.
;-------------------------

(define (double g)
  (lambda (x) (g (g x))))

;-------------------------
;; 1.42
;; Function composition.
;-------------------------

(define (compose f g)
  (lambda (x) (f (g x))))

;-------------------------
;; 1.43
;; Repeated unary functions.
;-------------------------

(define (repeated g n)
  (if (<= n 1)
      (lambda (x) (g x))
      (compose g (repeated g (- n 1)))))

;-------------------------
;; 1.44
;; n-fold smoothing.
;-------------------------

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3.0)))

(define (n-fold-smooth f n)
  (repeated (smooth f) n))

;-------------------------
;; 1.45
;; N-th root.
;;
;; Average-damping n/2 times.
;-------------------------

(define (cube-root x)
  (fixed-point-of-transform (lambda (y) (/ x (square y)))
                            average-damp
                            1.0))

(define (fourth-root x)
  (fixed-point-of-transform (lambda (y) (/ x (cube y)))
                            (repeated average-damp 2)
                            1.0))

(define (nth-root x n)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (repeated average-damp (/ n 2))
                            1.0))
  
;-------------------------
;; 1.46
;; Iterative improvement.
;-------------------------

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (lambda (x) (iter x)))

(define (iter-imp-sqrt x)
  (define (good-enough? guess)
    (< (abs (- x (square guess))) tolerance))
  ((iterative-improve good-enough? (average-damp (lambda (y) (/ x y)))) 1.0))
