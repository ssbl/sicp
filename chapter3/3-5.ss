;; Streams
;--------------------------------------------------


(define (my-cons-stream item items)     ; functions with prefix 'my' are
  (cons item (delay items)))            ; special forms in scheme.

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (my-delay exp)
  (memo-proc (lambda () exp)))
(define (my-force delayed-object)
  (delayed-object))

;-------------------------
;; 3.50
;; mapping over streams.
;-------------------------

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map
                          (cons proc (map stream-cdr argstreams))))))

;-------------------------
;; 3.51
;; streams and display.
;;
;; i. 10 .. 1 displayed.
;; ii. 5
;; iii. 7
;-------------------------

(define (show x)
  (display-line x)
  x)

;-------------------------
;; 3.52
;; some more streams.
;;
;; Running the functions more than once would be faster,
;; since the results would be memoized. Otherwise, each
;; function is evaluated once, so there is no difference.
;-------------------------

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))


(define ones
  (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (scale-stream s k)
  (stream-map (lambda (x) (* k x)) s))

;-------------------------
;; 3.53
;; example stream.
;;
;; The result is the same as that from the double stream.
;-------------------------

;-------------------------
;; 3.54
;; factorials.
;-------------------------

(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams factorials integers)))

;-------------------------
;; 3.55
;; partial sums.
;-------------------------

(define (partial-sums s)
  (add-streams s (cons-stream 0 (partial-sums s))))

;-------------------------
;; 3.56
;; Hamming's problem.
;-------------------------

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 5)
                                (merge (scale-stream S 3)
                                       (scale-stream S 2)))))

;-------------------------
;; 3.57
;; additions in fibs.
;;
;; The number of additions grows linearly in fibs.
;; No additions need to be performed if the values
;; are memoized.
;-------------------------

;-------------------------
;; 3.58
;; expand.
;;
;; (expand 1 7 10) gives 1,4,2,8,5,7 ..
;; (expand 3 8 10) gives 3,7,5,0..
;-------------------------

;-------------------------
;; 3.59
;; integrating series.
;-------------------------

(define coeffs
  (let ((denom 0))
    (define (iter)
      (set! denom (+ denom 1))
      (cons-stream (/ 1.0 denom) (iter)))
    (iter)))

(define (integrate-series s) (mul-streams coeffs s))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series
                  (scale-stream sine-series -1))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;-------------------------
;; 3.60
;; sin^2 x + cos^2 x = 1.
;-------------------------

(define (add-series s1 s2) (add-streams s1 s2))
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2)
                                          (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))
(define test-mul
  (let ((sin-squared (mul-series sine-series sine-series))
        (cos-squared (mul-series cosine-series cosine-series)))
    (add-series sin-squared cos-squared))) ; 1

;-------------------------
;; 3.61
;; inverse of a series.
;-------------------------

(define (invert-unit-series s)
  (let ((s-no-constant (stream-cdr s))
        (negate (lambda (s) (scale-stream s -1))))
    (define iter
      (cons-stream 1 (negate (mul-series s-no-constant iter))))
    iter))

;-------------------------
;; 3.62
;; tangent.
;-------------------------

(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error ("Divide by zero -- DIV-SERIES"))
      (mul-series s1 (invert-unit-series s2))))

(define tangent-series (div-series sine-series cosine-series))

;-------------------------
;; 3.63
;; slower guesses.
;;
;; With each call to (sqrt-stream x) the guesses have to be
;; computed again; they are not memoized. This is redundant. (VERIFY)
;-------------------------

;-------------------------
;; 3.64
;; sqrt using streams.
;-------------------------

(define (stream-limit s tolerance)
  (let ((s0 (stream-car s))
        (s1 (stream-car (stream-cdr s))))
  (if (<= (abs (- s0 s1)) tolerance)
      s1
      (stream-limit (stream-cdr s) tolerance))))

(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (improve guess x))
                             guesses)))
  guesses)

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;-------------------------
;; 3.65
;; ln-2 using streams.
;-------------------------

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (ln-2-summands n)
  (cons-stream (/ 1.0 n) (stream-map - (ln-2-summands (+ n 1)))))
(define (sum-stream s)
  (let ((sum (+ (stream-car s)
                (stream-car (stream-cdr s)))))
    (cons-stream sum
                 (sum-stream
                  (cons-stream sum
                               (stream-cdr (stream-cdr s)))))))
(define (ln-2 tolerance)                ; slowest
  (let ((summands (ln-2-summands 1)))
    (stream-limit (sum-stream summands)
                  tolerance)))
(define (ln-2-euler tolerance)
  (let ((summands (ln-2-summands 1)))
    (stream-limit (euler-transform (sum-stream summands))
                  tolerance)))
(define (ln-2-accelerated tolerance)    ; fastest
  (let ((summands (ln-2-summands 1)))
    (stream-limit (accelerated-sequence
                   euler-transform (sum-stream summands))
                  tolerance)))

;-------------------------
;; 3.66
;; interleaved streams.
;;
;; (car 1 (2 3 ..)) is interleaved with
;; the rest of the solution recursively. (VERIFY)
;-------------------------

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;-------------------------
;; 3.67
;; pythagorean triples.
;-------------------------

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (stream-map (lambda (y) (list (stream-car t) y))
                            (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (triple? x)
  (let ((i (car x))
        (j (cadr x))
        (k (caddr x)))
    (= (+ (square i) (square j)) (square k))))
;; (define pythagorean-triples
;;   (stream-filter triple? (triples integers integers integers)))

;-------------------------
;; 3.70
;; generalizing pairs.
;-------------------------

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        ((< (weight (stream-car s1))
            (weight (stream-car s2)))
         (cons-stream (stream-car s1)
                      (merge-weighted (stream-cdr s1) s2 weight)))
        ((> (weight (stream-car s1))
            (weight (stream-car s2)))
         (cons-stream (stream-car s2)
                      (merge-weighted s1 (stream-cdr s2) weight)))
        (else
         (cons-stream (stream-car s1)
                      (merge-weighted (stream-cdr s1) s2 weight)))))

(define (weighted-pairs s1 s2 weight)
  (cons-stream
   (list (stream-car s1) (stream-car s2))
   (merge-weighted
    (stream-map (lambda (x)
                  (list (stream-car s1) x))
                (stream-cdr s2))
    (weighted-pairs (stream-cdr s1) (stream-cdr s2) weight)
    weight)))

(define sum-ordered-pairs               ; a
  (let ((weight (lambda (x) (+ (car x) (cadr x)))))
    (weighted-pairs integers integers weight)))

(define hamming-problem-pairs           ; b
  (let ((weight
         (lambda (x)
           (+ (* 2 (car x))
              (* 3 (cadr x))
              (* 5 (car x) (cadr x)))))
        (filtered-integers
         (stream-filter (lambda (x)
                          (not (or (= (remainder x 2) 0)
                                   (= (remainder x 3) 0)
                                   (= (remainder x 5) 0))))
                        integers)))
    (weighted-pairs filtered-integers filtered-integers weight)))

;-------------------------
;; 3.71
;; ramanujan numbers.
;;
;; 4104,13832,20683,32832,39312.
;-------------------------

(define ramanujan-numbers
  (let ((weight (lambda (x)
                  (+ (expt (car x) 3)
                     (expt (cadr x) 3)))))
    (let ((cube-weighted-integers
           (weighted-pairs integers integers weight)))
      (define (weights-equal? s)
        (= (weight (stream-car s))
           (weight (stream-car (stream-cdr s)))))
      (define (iter s)
        (if (weights-equal? s)
            (cons-stream (weight (car s))
                         (iter (stream-cdr s)))
            (iter (stream-cdr s))))
      (iter cube-weighted-integers))))

;-------------------------
;; 3.72
;; other numbers.
;-------------------------

(define other-numbers
  (let ((weight (lambda (x)
                  (+ (expt (car x) 2)
                     (expt (cadr x) 2)))))
    (let ((square-weighted-integers
           (weighted-pairs integers integers weight)))
      (define (weights-equal? s)
        (= (weight (stream-car s))
           (weight (stream-car (stream-cdr s)))
           (weight (stream-car (stream-cdr (stream-cdr s))))))
      (define (iter s)
        (if (weights-equal? s)
            (cons-stream (list (weight (stream-car s))
                               (stream-car s)
                               (stream-car (stream-cdr s))
                               (stream-car (stream-cdr (stream-cdr s))))
                         (iter (stream-cdr (stream-cdr s))))
            (iter (stream-cdr s))))
      (iter square-weighted-integers))))

; streams as signals


(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

;-------------------------
;; 3.73
;; RC circuits.
;-------------------------

(define (RC R C dt)
  (lambda (i v0)
    (add-streams
     (scale-stream i R)
     (integral (scale-stream i (/ 1.0 C)) v0 dt))))

;-------------------------
;; 3.74
;; zero crossing detector.
;-------------------------

(define (sign-change-indicator x y)
  (cond ((and (negative? x) (or (= y 0) (positive? y))) -1)
        ((and (negative? y) (or (= x 0) (positive? x))) 1)
        (else 0)))
(define (zero-crossings sense-data)
  (stream-map sign-change-indicator
              sense-data
              (cons-stream 0 sense-data)))

;-------------------------
;; 3.75
;; pre-detection averaging.
;-------------------------

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))

;-------------------------
;; 3.76
;; smoothing function.
;-------------------------

(define (smooth s)
  (cons-stream (/ (+ (stream-car s) (stream-car (stream-cdr s))) 2)
               (smooth (stream-cdr s))))
(define (make-zero-crossings input-stream last-value)
  (let ((smoothed-input (smooth input-stream)))
    (cons-stream (sign-change-indicator (stream-car smoothed-input) last-value)
                 (make-zero-crossings (stream-cdr smoothed-input)
                                      (stream-car smoothed-input)))))

;-------------------------
;; 3.77
;; delayed integrals.
;-------------------------

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)


(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? delayed-integrand)
                   the-empty-stream
                   (let ((integrand (force delayed-integrand)))
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;-------------------------
;; 3.78
;; 2nd order equations.
;-------------------------

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

;-------------------------
;; 3.79
;; generalized solve-2nd.
;-------------------------

(define (general-solve-2nd f deriv1 y)
  (stream-map f deriv1 y))

;-------------------------
;; 3.80
;; RLC circuits.
;-------------------------

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ -1.0 C)))
    (define diL (add-streams (scale-stream vC (/ 1.0 L))
                             (scale-stream iL (/ (* -1.0 R) L))))
    (cons vC iL)))

;-------------------------
;; 3.81
;; rand using streams.
;-------------------------

(define rand
  (let ((init 42)
        (a 92) (b 23) (m 237)
        (update (lambda (x) (remainder (+ (* a x) b) m))))
    (cons-stream 42 (stream-map update (stream-cdr rand)))))

(define (rand-stream request-stream)
  (let ((request (stream-car request-stream)))
    (cond ((eq? request 'generate) (stream-cdr rand)) ; next value on stream
          ((eq? request 'reset)                       ; seed rand with new-value
           (lambda (new-value)
             (stream-cons new-value (stream-cdr rand))))
          (else (error "Invalid request -- RAND-STREAM" request)))))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

;-------------------------
;; 3.82
;; estimating integrals.
;-------------------------

(define (random-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (estimate-integral p x1 y1 x2 y2)
  (let ((tests 1))
    (define (experiment-stream x1 y1 x2 y2)
      (let ((random-x (random-range (stream-car x1) (stream-car x2)))
            (random-y (random-range (stream-car y1) (stream-car y2))))
        (cons-stream (p random-x random-y)
                     (experiment-stream (stream-cdr x1)
                                        (stream-cdr y1)
                                        (stream-cdr x2)
                                        (stream-cdr y2)))))
    (cons-stream (monte-carlo (experiment-stream x1 y1 x2 y1) tests 0)
                 (monte-carlo
                  (experiment-stream x1 y1 x2 y2) (+ tests 1) 0))))
