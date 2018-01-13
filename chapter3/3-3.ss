;; Modeling with Mutable Data
;--------------------------------------------------

;-------------------------
;; 3.12
;; mutable append.
;;
;; Both <response>s are '(b c d).
;-------------------------

;-------------------------
;; 3.13
;; another set! function.
;; 
;; (last-pair z) gives never returns.
;-------------------------

;-------------------------
;; 3.14
;; a mystery function.
;;
;; Reverses a list destructively.
;-------------------------

;-------------------------
;; 3.15
;; some diagrams.
;-------------------------

;-------------------------
;; 3.16
;; counting pairs in a list.
;;
;; 3 => (a b c)
;; 4 => ((a . b) c d)
;; 7 => (((a . b) a . b) (a . b) a . b)
;; never returns => (set-cdr! pair pair)
;-------------------------

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;-------------------------
;; 3.17
;; count-pairs with a set.
;-------------------------

(define (count-pairs x)
  (let ((set '()))
    (define (iter x)
      (cond ((null? x) 0)
            ((and (pair? x) (not (memq x set)))
             (begin (set! set (cons x set))
                    (+ 1
                       (iter (cdr x))
                       (iter (car x)))))
            (else 0)))
    (iter x)))

;-------------------------
;; 3.18
;; check if pair is a cycle.
;-------------------------

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define (check-for-cycle x)
  (define (iter x visited)
    (cond ((not (pair? x)) false)
          ((memq (cdr x) visited) true)
          (else (iter (cdr x) (cons x visited)))))
  (iter x '()))

;-------------------------
;; 3.19
;; Floyd's algorithm.
;-------------------------

(define (check-for-cycle x)
  (define (safe-cdr pair)
    (if (pair? pair)
        (cdr pair)
        '()))
  (define (safe-cddr pair)
    (safe-cdr (safe-cdr pair)))
  (define (iter slow fast)
    (cond ((or (not (pair? slow))
               (not (pair? fast)))
           false)
          ((eq? slow fast) true)
          (else (iter (safe-cdr slow) (safe-cddr fast)))))
  (iter (safe-cdr x) (safe-cddr x)))

;-------------------------
;; 3.20
;; some more diagrams.
;-------------------------


;; queues
;-------------------------

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 

;-------------------------
;; 3.21
;; a queue view.
;-------------------------

(define (print-queue queue) (car queue))

;-------------------------
;; 3.22
;; message-passing representation.
;-------------------------

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! x) (set! front-ptr x)) ; queue functions
    (define (set-rear-ptr! x) (set! rear-ptr x))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "Queue empty -- FRONT")
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (if (empty-queue?)
            (begin (set-front-ptr! new-pair)
                   (set-rear-ptr! new-pair)
                   front-ptr)
            (begin (set-cdr! rear-ptr new-pair)
                   (set-rear-ptr! new-pair)
                   front-ptr))))
    (define (delete-queue!)
      (if (empty-queue?)
          (error "Queue empty -- DELETE!")
          (begin (set-front-ptr! (cdr front-ptr))
                 front-ptr)))
    (define (dispatch m)                ; function dispatch
      (cond ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)
            ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            (else (error "Undefined queue operation" m))))
    dispatch))

;-------------------------
;; 3.23
;; deques.
;-------------------------

(define (make-deque) (cons '() '()))

(define (prev-ptr node) (cadr node))
(define (next-ptr node) (caddr node))

(define (set-prev-ptr! x item)
  (if (pair? x)
      (set-car! (cdr x) item)
      false))
(define (set-next-ptr! x item)
  (if (pair? x)
      (set-car! (cddr x) item)
      false))

(define (empty-deque? deque)
  (null? (front-ptr deque)))
(define (front-deque deque)
  (car (front-ptr deque)))
(define (rear-deque deque)
  (car (rear-ptr deque)))

(define (print-deque deque)
  (if (or (empty-deque? deque) (null? deque))
      '()
      (let ((current-node (car deque)))
        (cons (car current-node)
              (print-deque (cddr current-node))))))

(define (front-insert-deque! deque item)
  (let ((new-pair (list item '() '())))
    (if (empty-deque? deque)
        (begin (set-front-ptr! deque new-pair)
               (set-rear-ptr! deque new-pair)
               (print-deque deque))
        (begin (set-next-ptr! new-pair (front-ptr deque))
               (set-prev-ptr! (front-ptr deque) new-pair)
               (set-front-ptr! deque new-pair)
               (print-deque deque)))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (list item '() '())))
    (if (empty-deque? deque)
        (begin (set-front-ptr! deque new-pair)
               (set-rear-ptr! deque new-pair)
               (print-deque deque))
        (begin (set-prev-ptr! new-pair (rear-ptr deque))
               (set-next-ptr! (rear-ptr deque) new-pair)
               (set-rear-ptr! deque new-pair)
               (print-deque deque)))))

(define (front-delete-deque! deque)
  (if (empty-deque? deque)
      (error "Deque empty -- FRONT-DELETE!")
      (let ((next (next-ptr (front-ptr deque))))
        (begin (set-front-ptr! deque next)
               (set-prev-ptr! next '())
               (print-deque deque))
        (if (not (pair? deque))
            (begin (set-rear-ptr! deque '())
                   (print-deque deque))
            (print-deque deque)))))

(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
      (error "Deque empty -- REAR-DELETE!")
      (let ((prev (prev-ptr (rear-ptr deque))))
        (begin (set-next-ptr! prev '())
               (set-rear-ptr! deque prev))
        (if (not (pair? prev))
            (begin (set-front-ptr! deque '())
                   (print-deque deque))
            (print-deque deque)))))

(define (test-deque)
  (let ((d (make-deque)))
    (begin (front-insert-deque! d 'x)
           (rear-insert-deque! d 'y)
           (front-delete-deque! d)
           (rear-delete-deque! d)
           (print-deque d))))           ; should return an empty list

;-------------------------
;; 3.24
;; similar keys.
;-------------------------

(define (same-key? key1 key2)
  (let ((tolerance 5)
        (difference (abs (- key1 key2))))
    (<= difference tolerance)))

;-------------------------
;; 3.25
;; arbitrary number of keys.
;;
;; Since the current implementation uses equal?,
;; using a list of numbers as the key is already supported.
;-------------------------

;-------------------------
;; 3.26
;; table using a tree.
;-------------------------

(define (make-tree data left right)
  (list data left right))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-record key value) (cons key value))
(define (get-key record) (car record))
(define (get-value record) (cadr record))

(define (make-table) (list '*table*))

(define (assoc key record-tree)
  (cond ((null? record-tree) false)
        ((eq? key (caar record-tree))
         (car record-tree))
        ((< key (caar record-tree))
         (assoc key (left-branch record-tree)))
        ((> key (caar record-tree))
         (assoc key (right-branch record-tree)))))
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (insert! key value table)
  (define (insert-into-tree tree)
    (cond ((null? tree)
           (make-tree (make-record key value) '() '()))
          ((< key (caar tree))
           (make-tree (car tree)
                      (insert-into-tree (left-branch tree))
                      (right-branch tree)))
          ((> key (caar tree))
           (make-tree (car tree)
                      (left-branch tree)
                      (insert-into-tree (right-branch tree))))))
  (let ((record (assoc key (cdr table))))
    (if record
        (begin (set-cdr! record value)
               table)
        (begin (set-cdr! table (insert-into-tree (cdr table)))
               table))))

;-------------------------
;; 3.27
;; memoization.
;;
;; memo-fib stores all results which are (< n) in a table.
;; Computing fib(n) takes n steps, if n is the largest number yet.
;; Else, it takes a single step: looking up the value from the table.
;-------------------------


;; digital circuits
;-------------------------

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;-------------------------
;; 3.28
;; or gate.
;-------------------------

(define (or-gate a1 a2 output)
  (define (or-gate-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-gate-procedure)
  (add-action! a2 or-gate-procedure)
  'ok)

;-------------------------
;; 3.29
;; or gate using and, inverters.
;-------------------------

(define (or-gate a1 a2 output)
  (let ((s (make-wire))
        (inverted-a1 (make-wire))
        (inverted-a2 (make-wire)))
    (inverter a1 inverted-a1)
    (inverter a2 inverted-a2)
    (and-gate inverted-a1 inverted-a2 s)
    (inverter s output)
    'ok))

;-------------------------
;; 3.30
;; ripple carry adder.
;-------------------------

(define (ripple-carry-adder a b s c)
  (let ((n-inputs (length a)))
    (define (iter a-k b-k s-k n)
      (if (= n n-inputs)
          (let ((c-n (make-wire)))
            (full-adder (car a-k)
                        (car b-k)
                        c-n
                        (car s-k)
                        c))
          (begin (iter (cdr a-k) (cdr b-k) (cdr s-k) (+ n 1))
                 (full-adder (car a-k) (car b-k) c (car s-k) c))))
    (iter a b s 1)))

; wires

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; the agenda

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

; sample simulation

(define (probe name wire)
  (add-action! wire
               (lambda ()        
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

;; (define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

;-------------------------
;; 3.31
;; adding a procedure differently.
;;
;; By not executing the procedure after adding it,
;; the probe function will behave differently.
;; By running proc after adding it, we can see the
;; signal value of a wire even if it did not change
;; after an operation.
;-------------------------

;-------------------------
;; 3.32
;; why use a queue?
;;
;; In this case, the order of events would not change the
;; overall outcome. Since (0,1) and (1,0) produce the
;; same output (0).
;-------------------------

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints 
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)  
           (process-new-value))
          ((eq? request 'I-lost-my-value) 
           (process-forget-value))
          (else 
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

;-------------------------
;; 3.33
;; averager.
;-------------------------

(define (averager a b c)
  (let ((a+b (make-connector))
        (half (make-connector)))
    (adder a b a+b)
    (constant 0.5 half)
    (multiplier half a+b c)
    'done))

;-------------------------
;; 3.34
;; unsafe squarer.
;;
;; This constraint does not deal with negative values of b.
;-------------------------

;-------------------------
;; 3.35
;; safer squarer.
;-------------------------

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt b) me))
        (if (has-value? a)
            (set-value! b (square a) me)))
    'done)
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value) 
           (process-forget-value))
          (else 
           (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

;-------------------------
;; 3.36
;; some more diagrams.
;-------------------------

;-------------------------
;; 3.37
;; expression-oriented constraints.
;-------------------------

(define (c+ a b)
  (let ((sum (make-connector)))
    (adder a b sum)
    sum))

(define (c- a b)
  (let ((negative-one (make-connector))
        (negative-b (make-connector))
        (difference (make-connector)))
    (constant -1 negative-one)
    (multiplier negative-one b negative-b)
    (adder a negative-b difference)
    difference))

(define (c* a b)
  (let ((product (make-connector)))
    (multiplier a b product)
    product))

(define (divider a b c)
  (define (process-new-value)
    (cond ((and (has-value? b) (= (get-value b) 0))
           (error "Divide by zero -- DIVIDER"))
          ((and (has-value? a) (has-value? b))
           (set-value! c (/ (get-value a) (get-value b)) me))
          ((and (has-value? a) (has-value? c))
           (set-value! b (* (get-value a) (get-value c)) me))
          ((and (has-value? b) (has-value? c))
           (set-value! a (* (get-value a) (get-value c)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (forget-value! c me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value) 
           (process-forget-value))
          (else 
           (error "Unknown request -- DIVIDER" request))))
  (connect a me)
  (connect b me)
  (connect c me)
  me)

(define (c/ a b)
  (let ((fraction (make-connector)))
    (divider a b fraction)
    fraction))

(define (cv value)
  (let ((c (make-connector)))
    (constant value c)
    c))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
