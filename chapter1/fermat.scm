; Fermat test

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= 0 times) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (smallest-divisor n)
  (define (divides? div n)
    (= 0 (remainder n div)))
  (define (try-divisor div n)
    (cond ((> (square div) n) n)
          ((divides? div n) div)
          (else (try-divisor (+ 1 div) n))))
  (try-divisor 2 n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (prime? n)
  (fast-prime? n 1000))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime total-time)
  (display " *** ")
  (display total-time))

;--------------------------------------------------
; Results: > 1000 = 1009, 1013, 1019
;          > 10000 = 10007, 10009, 10037
;          > 100000 = 100003, 100019, 100043
;          > 1000000 = 1000003, 1000033, 1000037
;--------------------------------------------------
(define (search-for-primes a b)
  (define (adjust-start-value x)
    (if (even? x) (+ 1 x) x))
  (define (display-prime x)
    (newline)
    (display x))
  (define (search-from-odd m n)
    (if (< m n)
        (begin
          (timed-prime-test m)
          (search-from-odd (+ 2 m) n))))
  (search-from-odd (adjust-start-value a) b))

(define (fast-smallest-divisor n)
  (define (divides? div n)
    (= 0 (remainder n div)))
  (define (next x)
    (if (= 2 x)
        3
        (+ 2 x)))
  (define (try-divisor div n)
    (cond ((> (square div) n) n)
          ((divides? div n) div)
          (else (try-divisor (next div) n))))
  (try-divisor 2 n))

(define (non-trivial-sqrt? a m)
  (and (not (= 1 a)) (not (= (- m 1) a)) (= 1 (remainder (square a) m))))

(define (expmod-with-signal a n m)
  (cond ((= 0 n) 1)
        ((even? n) (if (non-trivial-sqrt? a m)
                       0
                       (remainder (square (expmod-with-signal a (/ n 2) m)) m)))
        (else (remainder (* a (expmod-with-signal a (- n 1) m)) m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= 1 (expmod-with-signal a (- n 1) n)))
  (try-it (+ 1 (random (- n 1)))))

(define (faster-prime? n times)
  (cond ((= 0 times) true)
        ((miller-rabin-test n) (faster-prime? n (- times 1)))
        (else false)))
