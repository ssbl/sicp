; Baby's first Scheme program
; Chapter 1 exercises

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (sqrt x) (sqrt-iter 1.0 x))

; 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x)
                         x)))

; 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

