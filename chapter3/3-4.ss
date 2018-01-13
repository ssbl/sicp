;; Concurrency
;--------------------------------------------------


;-------------------------
;; 3.38
;; multiple sequences of events.
;;
;; a.
;; Based on different sequence of events,
;; the deposit could be 35, 45, 50 or 40.
;;
;; b.
;; If they are interleaved, the deposit
;; could be 110, 80 or 50. (AND SOME MORE)
;-------------------------

;-------------------------
;; 3.39
;; different serialization.
;;
;; x can either be 121 or 100. (VERIFY)
;-------------------------

;-------------------------
;; 3.40
;; serializing 2 3-step procedures.
;;
;; Without serializing, 20 ways of interleaving.
;; Otherwise, 2 values possible. (MENTIONED EARLIER?)
;-------------------------

;-------------------------
;; 3.41
;; bank-balance access.
;;
;; Balance access should be serialized.
;; A different order of accesses will change the values
;; that are read by the processes. Some of these sequences
;; will result in anomalous balance amounts.
;-------------------------

;-------------------------
;; 3.42
;; another serialized version.
;;
;; No change.
;-------------------------

;-------------------------
;; 3.43
;; sequential, concurrent.
;;
;; Since the operations are sequential and conurrent,
;; there is no interleaving. This makes the exchanges
;; consistent. Since they are concurrent, the difference
;; in their order makes a difference in the order of the
;; balances.
;; 
;; This version does not lock out other processes when one
;; account accesses the balance.
;-------------------------

;-------------------------
;; 3.44
;; transfer.
;;
;; The current version is enough.
;; Both actions (withdrawal and deposit) are independent of
;; the accounts involved, unlike exchange.
;-------------------------

;-------------------------
;; 3.45
;; different exchange version.
;;
;; Tries to acquire already acquired lock. (WIKI)
;-------------------------

;-------------------------
;; 3.46
;; non-atomic test-and-set!
;;
;; One process tests, meanwhile another process sets.
;-------------------------

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))
(define (clear! cell)
  (set-car! cell false))

;-------------------------
;; 3.47
;; semaphores.
;-------------------------

(define (make-semaphore n)
  (let ((processes 0)
        (mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (set! processes (+ processes 1))
             (if (>= processes n)
                 (mutex 'acquire))
             'acquired)
            ((eq? m 'release)
             (if (= processes n)
                 (mutex 'release))
             (set! processes (- processes 1))
             'released)
            (else (error "Invalid command -- SEMAPHORE" m))))
    the-semaphore))

(define (make-semaphore n)
  (let ((processes 0)
        (cell false))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (set! processes (+ processes 1))
             (if (>= processes n)
                 (if (test-and-set! cell)
                     (the-semaphore 'acquire)))
             'acquired)
            ((eq? m 'release)
             (if (= processes n)
                 (clear! cell))
             (set! processes (- processes 1))
             'released)
            (else (error "Invalid command -- SEMAPHORE" m))))
    the-semaphore))

;-------------------------
;; 3.48
;; numbered accounts.
;-------------------------

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (id1 (get-number account1))
        (id2 (get-number account2)))
    (if (< id1 id2)
        (serializer2 (serializer1 exchange))
        (serializer1 (serializer2 exchange)))
    account1
    account2))

;-------------------------
;; 3.49
;; scenario.
;;
;; P1 needs to access a table.
;; P2 has access to the table, and is waiting for P1 before
;; witholding access. P1 needs to lookup the appropriate record
;; in the table to give P1 the go-ahead. Deadlock. (VERIFY)
;-------------------------
