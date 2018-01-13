;; 2.2
;; Hierarchical data and closure property.
;--------------------------------------------------


;-------------------------
;; 2.17
;; Last element as unit list.
;-------------------------

(define (last-pair xs)
  (cond ((null? xs) (error "last-pair: Empty list."))
        ((null? (cdr xs)) (list (car xs)))
        (else (last-pair (cdr xs)))))

;-------------------------
;; 2.18
;; Reverse a list.
;-------------------------

(define (reverse xs)
  (cond ((null? xs) xs)
        (else (append (reverse (cdr xs))
                      (list (car xs))))))

;-------------------------
;; 2.19
;; Redefining cc using lists.
;-------------------------

(define (no-more? coins) (null? coins))
(define (except-first-denomination coins)
  (cdr coins))
(define (first-denomination coins)
  (car coins))
(define (cc amount coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coins)) 0)
        (else (+ (cc amount (except-first-denomination coins))
                 (cc (- amount (first-denomination coins)) coins)))))

;-------------------------
;; 2.20
;; Elements from ys with same parity as x.
;;
;; Redefined filter, which was unnecessary.
;-------------------------

(define (same-parity x . ys)
  (define x-parity (remainder x 2))
  (define (filter f xs)
    (cond ((null? xs) xs)
          ((f (car xs)) (cons (car xs)
                              (filter f (cdr xs))))
          (else (filter f (cdr xs)))))
  (filter (lambda (y) (= (remainder y 2)
                         x-parity))
          ys))

;-------------------------
;; 2.21
;; Map.
;-------------------------

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))
(define (square-list items)
  (map square items))

;-------------------------
;; 2.22
;; cons adds to the start of the list.
;-------------------------

;-------------------------
;; 2.23
;; for-each: map without returning a list.
;-------------------------

(define (for-each- f items)
  (if (null? items)
      true
      (begin (f (car items))
             (for-each- f (cdr items)))))

;-------------------------
;; 2.24
;; result = (1 (2 (3 4))
;-------------------------

;-------------------------
;; 2.25
;; (1) (cadar (cddr x))
;; (2) (caar x)
;; (3) (cadadadadadadr x)
;-------------------------

;-------------------------
;; 2.26
;; (1) (1 2 3 4 5 6)
;; (2) ((1 2 3) 4 5 6)
;; (3) ((1 2 3) (4 5 6))
;-------------------------

;-------------------------
;; 2.27
;; Reverse nested lists.
;-------------------------

(define (deep-reverse items)
  (cond ((null? items) items)
        ((list? (car items))
         (append (deep-reverse (cdr items)) (list (reverse (car items)))))
        (else (append (deep-reverse (cdr items)) (list (car l))))))

;-------------------------
;; 2.28
;; List leaves.
;-------------------------

(define (fringe tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

;; binary mobile
;-------------------------

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

;-------------------------
;; 2.29.a
;; Mobile selectors.
;-------------------------

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

;-------------------------
;; 2.29.b
;; Mobile weight.
;-------------------------

(define (total-weight mobile)
  (define (follow-branch b)
    (cond ((not (pair? (branch-structure b))) (branch-structure b))
          (else (total-weight (branch-structure b)))))
  (+ (follow-branch (left-branch mobile)) (follow-branch (right-branch mobile))))

;-------------------------
;; 2.29.c
;; Check if mobile is balanced.
;; Inefficient.
;-------------------------

(define (balanced? mobile)
  (define (check-branch b)
    (cond ((not (pair? (branch-structure b))) (* (branch-length b)
                                                 (branch-structure b)))
          ((balanced? (branch-structure b))
           (* (length b) (total-weight (branch-structure b))))
          (else 0)))
  (= (check-branch (left-branch mobile)) (check-branch (right-branch mobile))))

;-------------------------
;; 2.29.d
;; Only the selectors will change.
;-------------------------

;; mapping over trees
;-------------------------

;-------------------------
;; 2.30
;; Square tree elements.
;-------------------------

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

;-------------------------
;; 2.31
;; map for trees.
;-------------------------

(define (tree-map proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))
(define (map-square-tree tree) (tree-map square tree))

;-------------------------
;; 2.32
;; Subsets. Interesting!
;-------------------------

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; sequence operations
;-------------------------

(define (accumulate proc initial sequence)
  (if (null? sequence)
      initial
      (proc (car sequence)
            (accumulate proc initial (cdr sequence)))))

;-------------------------
;; 2.33
;; Using accumulate.
;-------------------------

(define (map- p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (append- list1 list2)
  (accumulate (lambda (x y) (cons x y)) list2 list1))
(define (length- sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;-------------------------
;; 2.34
;; Horner's rule
;-------------------------

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;-------------------------
;; 2.35
;; Redefining count-leaves.
;-------------------------


(define (count-leaves tree)
  (define (recursive-mark items)
    (if (pair? items)
        (map recursive-mark items)
        1))
  (accumulate (lambda (t ts) (if (pair? t)
                                 (count-leaves t)
                                 (+ t ts)))
              0
              (map recursive-mark tree)))

;-------------------------
;; 2.36
;; accumulate on list of lists.
;-------------------------

(define (accumulate-n proc initial sequences)
  (if (null? (car sequences))
      '()
      (cons (accumulate proc initial (map car sequences))
            (accumulate-n proc initial (map cdr sequences)))))

;-------------------------
;; 2.37
;; vector, matrix operations.
;-------------------------

(define (dot-product v w)
  (accumulate + 0 (accumulate-n * 1 (list v w))))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))
(define (transpose m)
  (accumulate-n cons '() m))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (map (lambda (y) (dot-product x y)) cols)) m)))

;-------------------------
;; 2.38
;; fold-left.
;;
;; 1. 3/2
;; 2. 1/6
;; 3. (1 2 3)
;; 4. (3 2 1)
;; op must be commutative. (op a b) == (op b a)
;-------------------------

(define (fold-left op init sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init sequence))

;-------------------------
;; 2.39
;; reverse using folds.
;-------------------------

(define (fold-right op intial sequence)
  (accumulate op intial sequence))
(define (fold-right-reverse sequence)
  (fold-right (lambda (item rest) (append rest (list item))) '() sequence))
(define (fold-left-reverse sequence)
  (fold-left (lambda (rest item) (cons item rest)) '() sequence))

;; nested mappings
;-------------------------

(define (enumerate-interval x y)
  (if (< y x)
      '()
      (cons x (enumerate-interval (+ x 1) y))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (remove x seq)
  (if (= x (car seq))
      (cdr seq)
      (cons (car seq) (remove x (cdr seq)))))

(define (permutations seq)
  (if (null? seq)
      '(())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x seq))))
               seq)))

;-------------------------
;; 2.40
;; Ordered pairs.
;-------------------------

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

;-------------------------
;; 2.41
;; Triplet sum.
;-------------------------

(define (ordered-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                    (map (lambda (k) (list i j k))
                         (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 2 (- i 1))))
           (enumerate-interval 3 n)))
(define (sum-triples n s)
  (define (sum triple)
    (+ (car triple) (cadr triple) (caddr triple)))
  (filter (lambda (t) (= (sum t) s)) (ordered-triples n)))

;-------------------------
;; 2.42
;; N-Queens problem.
;-------------------------

(define empty-board '())

(define (safe? k positions)
  (define (check-cols col)
    (let ((queen-at-k (car (last-pair positions)))
          (other-queens (except-last-pair positions)))
      (cond ((= col k) true)
            ((or (= (list-ref other-queens (- col 1)) queen-at-k)
                 (= (abs (- col k))
                    (abs (- (list-ref other-queens (- col 1)) queen-at-k))))
             false)
            (else (check-cols (+ col 1))))))
  (if (= (length positions) 1)
      true
      (check-cols 1)))

(define (adjoin-position new-row k queens)
  (append queens (list new-row)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;-------------------------
;; 2.42
;; The interchange makes queens a tree-recursive procedure.
;; Expected time = T^N.
;; Ref: BTL's blog.
;-------------------------

;-------------------------
;; 2.44
;; up-split
;-------------------------

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((split (up-split painter (- n 1))))
        (below painter (beside split split)))))

;-------------------------
;; 2.45
;; Generalized split.
;-------------------------

(define (split first second)
  (define (split-painter painter n)
    (if (= n 0)
        painter
        (let ((s (split-painter painter (- n 1))))
          (first painter (second s s)))))
  (split-painter))

;-------------------------
;; 2.46
;; Vector selectors, operations.
;-------------------------

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2) (make-vect (+ (xcor-vect v1) (xcor-vect v2))
                                    (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2) (make-vect (- (xcor-vect v1) (xcor-vect v2))
                                    (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v) (make-vect (* s (xcor-vect v))
                                    (* s (ycor-vect v))))

;-------------------------
;; 2.47
;; Frame constructors, selectors. 
;-------------------------

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (cddr f))

;-------------------------
;; 2.48
;; Segments.
;-------------------------

(define (make-segment x1 y1 x2 y2)
  (cons (make-vect x1 y2) (make-vect x2 y2)))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

;-------------------------
;; 2.49
;; Drawing primitive painters.
;-------------------------

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line                       ; does not exist!
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (outline frame)
  (let ((seg1 (make-segment (origin frame) (edge1-frame)))
        (seg2 (make-segment (origin frame) (edge2-frame)))
        (seg3 (make-segment (edge1 frame) (add-vect (edge1-frame frame)
                                                    (edge2-frame frame))))
        (seg4 (make-segment (edge2 frame) (add-vect (edge1-frame frame)
                                                    (edge2-frame frame)))))
    (segments->painter (list seg1 seg2 seg3 seg4))))

(define (connect-corners frame)
  (let ((seg1 (make-segment (origin frame) (add-vect (edge1-frame frame)
                                                     (edge2-frame frame))))
        (seg2 (make-segment (edge1-frame frame) (edge2-frame frame))))
    (segments->painter (list seg1 seg2))))

(define (midpoint v1 v2)
  (make-vect (/ (+ (xcor-vect v1) (xcor-vect v2)) 2.0)
             (/ (+ (ycor-vect v1) (ycor-vect v2)) 2.0)))
(define (diamond frame)
  (let ((p (midpoint (origin frame) (edge1-frame)))
        (q (midpoint (origin frame) (edge2-frame)))
        (r (midpoint (edge1 frame) (add-vect (edge1-frame frame)
                                             (edge2-frame frame))))
        (s (midpoint (edge2 frame) (add-vect (edge1-frame frame)
                                             (edge2-frame frame)))))
    (segments->painter (list (make-segment p q)
                             (make-segment q s)
                             (make-segment s r)
                             (make-segment r p)))))

;-------------------------
;; 2.50
;; Additional transformations.
;-------------------------

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)))
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;-------------------------
;; 2.51
;; Defining below in two ways.
;-------------------------

(define (below painter1 painter2)
  (let* ((split-point (make-vect 0.0 0.5))
         (above (transform-painter painter2
                                   split-point
                                   (make-vect 0.5 1.0)
                                   (make-vect 0.0 1.0)))
         (under (transform-painter painter1
                                   (make-vect 0.0 0.0)
                                   (make-vect 0.0 1.0)
                                   split-point)))
    (lambda (frame)
      (above frame)
      (below frame))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))
  
;-------------------------
;; 2.52
;; I don't have the wave painter!
;-------------------------
