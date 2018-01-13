;; 2.1
;; Introduction to data abstraction.
;--------------------------------------------------


;-------------------------
;; 2.1
;; Normalized make-rat.
;-------------------------

(define (make-rat n d)
  (let ((sign (cond ((and (negative? n) (negative? d)) 1)
                    ((or (negative? n) (negative? d)) -1)
                    (else 1)))
        (abs-n (abs n))
        (abs-d (abs d))
        (g (gcd n d)))
    (cons (* sign (/ abs-n g))
          (/ abs-d g))))

;-------------------------
;; 2.2
;; Midpoint of a segment.
;-------------------------

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p q) (cons p q))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (define (average a b) (/ (+ a b) 2.0))
  (let ((p (start-segment s))
        (q (end-segment s)))
    (make-point (average (x-point p) (x-point q))
                (average (y-point p) (y-point q)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;-------------------------
;; 2.3
;; rectangles.
;-------------------------

(define (make-rectangle l b) (cons l b))
(define (length r) (car r))
(define (breadth r) (cdr r))

(define (perimeter r)
  (* 2 (+ (length r) (breadth r))))
(define (area r)
  (* l b))

;-------------------------
;; 2.4
;; Procedural definitions.
;-------------------------

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

;-------------------------
;; 2.5
;; Arithmetic definitions.
;-------------------------

(define (number-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (number-car x)
  (define (iter a result)
    (if (= (remainder result 2) 0)
        (iter (+ 1 a) (/ result 2))
        a))
  (iter 0 x))
(define (number-cdr x)
  (define (iter b result)
    (if (= (remainder result 3) 0)
        (iter (+ 1 b) (/ result 3))
        b))
  (iter 0 x))


;-------------------------
;; 2.6
;; Church numerals.
;;
;; zero = identity, add-1 = apply f
;; similarly, one = apply f once, two = twice ...
;; Had to look up wikipedia.
;-------------------------

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f (n x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
  

;-------------------------
;; 2.8
;; Interval constructors, selectors.
;-------------------------

(define (make-interval a b) (cons a b))
(define (upper-bound z) (cdr z))
(define (lower-bound z) (car z))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;-------------------------
;; 2.9
;; Interval width.
;;
;; Adding 2 intervals = adding their widths.
;; Not so for mul, div.
;-------------------------

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

;-------------------------
;; 2.10
;; Safer div-interval.
;-------------------------

(define (div-interval x y)
  (cond ((or (= 0 (upper-bound y)) (= 0 (lower-bound y)))
         (error "Divide by zero."))
        (else (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                             (/ 1.0 (lower-bound y)))))))

;-------------------------
;; 2.11
;; ugly mul-interval.
;-------------------------

(define (mul-interval x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (cond ((and (positive? x1) (positive? x2) (positive? y1) (positive? y2))
           (make-interval (* x1 y1) (* x2 y2)))
          ((and (positive? x1) (positive? x2) (negative? y1) (positive? y2))
           (make-interval (* x2 y1) (* x2 y2)))
          ((and (positive? x1) (positive? x2) (negative? y1) (negative? y2))
           (make-interval (* x2 y1) (* x1 y2)))
          ((and (negative? x1) (negative? x2) (negative? y1) (negative? y2))
           (make-interval (* x2 y2) (* x1 y1)))
          ((and (negative? x1) (negative? x2) (positive? y1) (positive? y2))
           (make-interval (* x1 y2) (* x2 y1)))
          ((and (negative? x1) (negative? x2) (negative? y1) (positive? y2))
           (make-interval (* x1 y2) (* x1 y1)))
          ((and (negative? x1) (positive? x2))
           (cond ((and (negative? y1) (negative? y2))
                  (make-interval (* x2 y1) (* x1 y1)))
                 ((and (positive? y1) (positive? y2))
                  (make-interval (* x1 y2) (* x2 y2)))
                 ((and (negative? y1) (positive? y2))
                  (make-interval (min (* x1 y2) (* x2 y1))
                                 (max (* x1 y1) (* x2 y2)))))))))
                 
;-------------------------
;; 2.12
;; Value +- tolerance %.
;-------------------------

(define (make-center-percent c p)
  (let ((fraction (* c (/ p 100.0))))
    (make-interval (- c fraction) (+ c fraction))))
(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))
(define (percent x)
  (* 100 (/ (- (upper-bound x) (center x)) (center x))))

;-------------------------
;; 2.13
;; (percent (mul-interval x y)) == (+ (percent x) (percent y))
;; For small percentages.
;-------------------------

;-------------------------
;; 2.14-2.16
;-------------------------

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
