; Systems with Generic Operations
;;--------------------------------------------------


;-------------------------
;; 2.77
;; (magnitude z)
;;
;; The operations real, imag, magnitude, angle are undefined.
;; Just as the rational number package needs to export appropriate selectors,
;; the complex number package exports the above selectors.
;; Trace: magnitude -> real-part / imag-part -> car / cdr
;; Why would apply-generic be called? We aren't using add, mul etc.
;-------------------------

;-------------------------
;; 2.78
;; discarding the scheme-number tag.
;;
;; attach-tag will not add a tag if the data is a number.
;; the rest of the system will see numbers as data with the scheme-number tag.
;-------------------------

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad number or tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad number or tagged datum -- CONTENTS" datum))))

;-------------------------
;; 2.79
;; generic equ?.
;-------------------------

(define (install-scheme-number-package)
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y))))
(define (install-rational-package)
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y))
                          (= (denom x) (denom y))))))
(define (install-complex-package)
  (put 'equ? '(complex complex)
       (lambda (x y) (and (= (magnitude x) (magnitude y))
                          (= (angle x) (angle y))))))

;-------------------------
;; 2.80
;; generic =zero?.
;-------------------------

(define (install-scheme-number-package)
  (put '=zero? 'scheme-number (lambda (x) (= x 0))))
(define (install-rational-package)
  (put '=zero? 'rational (lambda (x) (= (numer x) 0))))
(define (install-complex-package)
  (put '=zero? 'complex
       (lambda (x) (and (= (real-part x) 0)
                        (= (imag-part x) 0)))))

;-------------------------
;; 2.81
;; apply-generic issues.
;;
;; a.
;; Fails with "No method for these types".
;; b.
;; Yes.
;-------------------------

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (apply-generic op a1 a2)
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;-------------------------
;; 2.82
;; apply-generic for multiple arguments.
;;
;; Another strategy: find the argument with the type
;; which is the highest in the hierarchy.
;; Coerce all args to that type.
;-------------------------

;-------------------------
;; 2.83
;; raising types one level.
;-------------------------

(define (raise arg)
  (let ((type (type-tag arg)))
    (cond ((eq? type 'real)
           (make-complex arg 0))
          ((eq? type 'rational)
           (* 1.0 (/ (numer arg) (denom arg))))
          ((eq? type 'scheme-number)
           (make-rational arg 1))
          (else
           (error "Operation not supported for this type -- RAISE"
                  arg)))))

;-------------------------
;; 2.84
;; apply by comparing types.
;-------------------------

(define type-list
  '(complex real rational number))
(define (compare-types type1 type2)
  (define (compare-types-1 types)
    (cond ((null? types) false)
          ((eq? type1 type2) 'eq)
          ((eq? type1 (car types)) 'gt)
          ((eq? type2 (car types)) 'lt)
          (else (compare-types-1 (cdr types)))))
  (compare-types-1 type-list))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (apply-generic op a1 a2)
                    (let ((cmp (compare-types type1 type2)))
                      (cond ((eq? cmp 'gt)
                             (apply-generic op a1 (raise a2)))
                            ((eq? cmp 'lt)
                             (apply-generic op (raise a1) a2))
                            (else (error "Types not compatible"
                                         (list type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;-------------------------
;; 2.85
;; simplified apply-generic output.
;-------------------------

(define (project num)                   ; not data-directed dispatch!
  (let ((type (type-tag num))
        (datum (contents num)))
    (cond ((eq? type 'complex)
           (make-real (real-part num)))
          ((eq? type 'real)
           (make-rational (round datum)))
          ((eq? type 'rational)
           (/ (numer datum) (denom datum)))
          (else (error "Invalid type -- PROJECT")))))
(define (drop num)
  (let ((type (type-tag num))
        (datum (contents num))
        (projected-num (raise (project num))))
    (if (equ? num projected-num)
        (drop projected-num)
        num)))
(define (apply-generic op . args)
  (define (apply-generic-1 op . args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (if (= (length args) 2)
                (let ((type1 (car type-tags))
                      (type2 (cadr type-tags))
                      (a1 (car args))
                      (a2 (cadr args)))
                  (if (eq? type1 type2)
                      (apply-generic op a1 a2)
                      (let ((cmp (compare-types type1 type2)))
                        (cond ((eq? cmp 'gt)
                               (apply-generic op a1 (raise a2)))
                              ((eq? cmp 'lt)
                               (apply-generic op (raise a1) a2))
                              (else (error "Types not compatible"
                                           (list type-tags)))))))
                (error "No method for these types"
                       (list op type-tags)))))))
  (drop (apply-generic-1 op . args)))

;-------------------------
;; 2.86
;; generic sin, cos.
;-------------------------

(define (number-sine x) (sin x))
(define (rational-sine x)
  (sin (numer x) (denom x)))

(define (number-cos x) (cos x))
(define (rational-cos x)
  (cos (numer x) (denom x)))

(define (install-number-trig)
  (put 'sin '(scheme-number) number-sine)
  (put 'cos '(scheme-number) number-cos))
(define (install-rationl-trig)
  (put 'sin '(rational) 'rational-sine)
  (put 'cos '(rational) 'rational-cos))

;; 2.87 .. 2.97
;; put and get do not exist! leaving these exercises for another day.
