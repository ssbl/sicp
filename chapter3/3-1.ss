;; Assignment and Local State 
;--------------------------------------------------

;-------------------------
;; 3.1
;; accumulator with state.
;-------------------------

(define (make-accumulator sum)
  (lambda (value-to-add)
    (begin (set! sum (+ sum value-to-add))
           sum)))

;-------------------------
;; 3.2
;; counter as state.
;-------------------------

(define (make-monitored f)
  (let ((count 0))
    (lambda (f-arg)
      (cond ((eq? f-arg 'how-many-calls?) count)
            ((eq? f-arg 'reset-count)
             (begin (set! count 0)
                    "count reset."))
            (else (begin (set! count (+ count 1))
                         (f f-arg)))))))

;-------------------------
;; 3.3
;; internal state.
;-------------------------

(define (make-account balance password)
  (lambda (pwd operation)
    (lambda (value)
      (if (not (eq? pwd password))
          "Incorrect password."
          (cond ((eq? operation 'withdraw)
                 (if (>= balance value)
                     (begin (set! balance (- balance value))
                            balance)
                     "Insufficient funds."))
                ((eq? operation 'deposit)
                 (if (positive? value)
                     (begin (set! balance (+ balance value))
                            balance)
                     "Invalid amount.")))))))
                            
;-------------------------
;; 3.4
;; counter, internal state.
;-------------------------

(define (make-account balance password)
  (define (reset-count) (set! count 0))
  (let ((count 0)
        (max-count 7))
    (lambda (pwd)
      (lambda (operation value)
        (if (not (eq? pwd password))
            (if (= count max-count)
                "Calling the cops!"
                (begin (set! count (+ count 1))
                       "Incorrect password."))
            (begin (reset-count)
                   (cond ((eq? operation 'withdraw)
                          (if (>= balance value)
                              (begin (set! balance (- balance value))
                                     balance)
                              "Insufficient funds."))
                         ((eq? operation 'deposit)
                          (if (positive? value)
                              (begin (set! balance (+ balance value))
                                     balance)
                              "Invalid amount.")))))))))

;-------------------------
;; 3.5
;; area of a region.
;-------------------------

(define (random-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((random-x (random-range x1 x2))
          (random-y (random-range y1 y2)))
    (p random-x random-y)))
  (let ((trials-passed (monte-carlo trials experiment))
        (total-area (* (- x2 x1) (- y2 y1))))
    (* trials-passed total-area)))

;-------------------------
;; 3.6
;; resetting a generator.
;-------------------------

(define random-init 42)
(define (rand-update x)                 ; from footnote 6.
  (let ((a 71)
        (b 99)
        (m 78))
    (remainder (+ (* a x) b) m)))
(define rand
  (let ((x random-init))
    (lambda (cmd-arg)
      (cond ((eq? cmd-arg 'generate)
             (lambda (input)
               (begin (set! x (rand-update input))
                      x)))
            ((eq? cmd-arg 'reset)
             (lambda (new-value)
               (begin (set! x new-value)
                      x)))
            (else (error "Invalid command -- RAND"))))))

;-------------------------
;; 3.7
;; aliasing.
;-------------------------

(define (make-joint account password new-password)
  (lambda (entered-password)
    (if (eq? entered-password new-password)
        (account password)
        "Incorrect password")))

;-------------------------
;; 3.8
;; not referentially transparent functions.
;-------------------------

(define (f x)
  (let ((state 0))
    (if (= state x)
        (begin (set! state (- state 1))
               state)
        (begin (set! state x)
               x))))
