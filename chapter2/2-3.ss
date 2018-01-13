;; 2.3
;; Symbolic Data
;--------------------------------------------------


;-------------------------
;; 2.53
;; Using quote.
;;
;; => (list 'a 'b 'c)
;; (a b c)
;; => (list (list 'george))
;; ((george))
;; => (cdr '((x1 y1) (x2 y2)))
;; ((x2 y2))
;; => (cadr '((x1 y1) (x2 y2)))
;; (x2 y2)
;; => (pair? (car '(a short list)))
;; false
;; => memq 'red '((red shoes) (blue socks))
;; false
;; => memq 'red '(red shoes blue socks)
;; (red shoes blue socks)
;-------------------------

;-------------------------
;; 2.54
;; eq? on a pair of lists.
;-------------------------

(define (equal-list? list1 list2)
  (cond ((and (null? list1) (null? list2)) true)
        ((or (null? list1) (null? list2)) false)
        ((eq? (car list1) (car list2)) (equal-list? (cdr list1) (cdr list2)))
        (else false)))

;-------------------------
;; 2.55
;; Using quote II.
;;
;; (car ''abcdefg) becomes (quote (quote abcdefg)).
;; this gives the result `quote'.
;-------------------------


;; symbolic differentiation
;-------------------------

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;-------------------------
;; 2.56
;; Exponentiation rule.
;-------------------------

(define (exponentiation? exp)
  (eq? (car exp) '**))
(define (base exp) (second exp))
(define (exponent exp) (third exp))

(define (make-exponentiation u n)
  (cond ((= n 0) 1)
        ((= n 1) u)
        (else (list '** u n))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp)
                                                          (- (exponent exp) 1))
                                     (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;-------------------------
;; 2.57
;; multiple operand sum, product.
;-------------------------

(define (make-sum term1 term2 . terms)
  (if (null? terms)
      (cond ((=number? term1 0) term2)
            ((=number? term2 0) term1)
            ((and (number? term1) (number? term2)) (+ term1 term2))
            (else (list '+ term1 term2)))
      (append (list '+ term1 term2) terms)))
(define (addend exp) (cadr exp))
(define (augend exp)
    (if (null? (cdddr exp))
        (caddr exp)
        (cons '+ (cddr exp))))

(define (make-product term1 term2 . terms)
  (if (null? terms)
      (cond ((or (=number? term1 0) (=number? term2 0)) 0)
            ((=number? term1 1) term2)
            ((=number? term2 1) term1)
            ((and (number? term1) (number? term2)) (* term1 term2))
            (else (list '* term1 term2)))
      (append (list '* term1 term2) terms)))
(define (multiplier exp) (cadr exp))
(define (multiplicand exp)
  (if (null? (cdddr exp))
      (caddr exp)
      (cons '* (cddr exp))))

;-------------------------
;; 2.58.a
;; Infix sum, product.
;-------------------------

(define (sum? exp)
  (eq? (cadr exp) '+))

(define (make-sum s1 s2)
  (cond ((=number? s1 0) s2)
        ((=number? s2 0) s1)
        ((and (number? s1) (number? s2)) (+ s1 s2))
        (else (list s1 '+ s2))))
(define (addend sum) (car sum))
(define (augend sum) (caddr sum))

(define (product? exp)
  (eq? (cadr exp) '*))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
(define (multiplier prod) (car prod))
(define (multiplicand prod) (caddr prod))


;-------------------------
;; 2.58.b
;; Standard infix operators.
;-------------------------

(define (anti-memq item items)
  (define (iter result rest)
    (cond ((null? rest) false)
          ((eq? item (car rest)) result)
          (else (iter (append result (list (car rest))) (cdr rest)))))
  (iter '() items))

(define (simplify exp)                  ; credits to BTL
  (if (null? (cdr exp))
      (car exp)
      exp))

(define (sum? exp) (pair? (memq '+ exp)))
(define (addend sum) (simplify (anti-memq '+ sum)))
(define (augend sum) (simplify (cdr (memq '+ sum))))

(define (multiplicand prod) (simplify (cddr prod)))

;; sets
;-------------------------


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;-------------------------
;; 2.59
;; union in unordered sets.
;-------------------------

(define (union-set set1 set2)
  (fold-right adjoin-set '() (append set1 set2)))

;-------------------------
;; 2.60
;; unordered sets with duplicates.
;;
;; element-of-set?, intersection-set don't change.
;-------------------------

(define (adjoin-set x set) (cons x set))
(define (union-set set1 set2) (append set1 set2))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2) (cons (car set1)
                                                 (intersection-set (cdr set1)
                                                                   set2)))
        (else (intersection-set (cdr set1) set2))))

;-------------------------
;; 2.61
;; adjoin for ordered sets.
;-------------------------

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;-------------------------
;; 2.62
;; union for ordered sets.
;-------------------------

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        ((> (car set1) (car set2))
         (cons (car set2)
               (union-set set1 (cdr set2))))
        (else (cons (car set1) (union-set (cdr set1) (cdr set2))))))


(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;-------------------------
;; 2.63
;; Trees as lists.
;;
;; a.
;; Both procedures produce same results.
;; Result is (1 3 5 7 9 11) for all trees.
;;
;; b. (ref. BTL's blog)
;; tree->list-1: O(nlogn)
;; append divides the dataset into half with each call.
;; tree->list-2: O(n)
;; each node is visited with a cons, an O(1) operation.
;-------------------------

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;-------------------------
;; 2.64
;; Balanced binary trees.
;;
;; a.
;; partial-tree balances the list at `n`.
;; when n = 0, a leaf node is reached.
;; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))) 
;;
;; b.
;; O(n) since each element is visited once.
;-------------------------

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;-------------------------
;; 2.65
;; union, intersection for tree sets.
;;
;; a balanced tree always produces an ordered list using tree->list-*.
;-------------------------

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (union-set set1 set2)
  (define (merge list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list1)
          ((= (car list1) (car list2))
           (cons (car list1) (merge (cdr list1) (cdr list2))))
          ((< (car list1) (car list2))
           (cons (car list1) (merge (cdr list1) list2)))
          ((> (car list1) (car list2))
           (cons (car list2) (merge list1 (cdr list2))))))
  (list->tree
   (merge (tree->list-2 set1)
          (tree->list-2 set2))))

(define (intersection-set set1 set2)
  (define (take-common list1 list2)
    (cond ((or (null? list1) (null? list2)) '())
          ((= (car list1) (car list2))
           (cons (car list1) (take-common (cdr list1) (cdr list2))))
          ((< (car list1) (car list2))
           (take-common (cdr list1) list2))
          ((> (car list1) (car list2))
           (take-common list1 (cdr list2)))))
  (list->tree
   (take-common (tree->list-2 set1)
                (tree->list-2 set2))))

;-------------------------
;; 2.66
;; Lookup in tree-set records.
;-------------------------

(define (lookup k record-tree-set)
  (cond ((null? record-tree-set) false)
        ((= k (key (entry record-tree-set)))
         (entry record-tree-set))
        ((< k (key (entry record-tree-set)))
         (lookup k (left-branch record-tree-set)))
        ((> k (key (entry record-tree-set)))
         (lookup k (right-branch record-tree-set)))))

;; huffman trees
;-------------------------


(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

;-------------------------
;; 2.67
;; (a d a b b c a)
;-------------------------

;-------------------------
;; 2.68
;; encode.
;-------------------------

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (cond ((or (null? tree) (leaf? tree)) '())
        ((memq symbol (left-branch tree))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        (else (cons 1 (encode-symbol symbol (right-branch tree))))))

;-------------------------
;; 2.69
;; generate huffman tree.
;-------------------------

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge leaf-set)
  (cond ((null? leaf-set) '())
        ((null? (cdr leaf-set)) (car leaf-set))
        (else (let ((min1 (car leaf-set))
                    (min2 (cadr leaf-set)))
                (let ((new-node (make-code-tree min1 min2)))
                  (successive-merge
                   (adjoin-set new-node (cddr leaf-set))))))))

;-------------------------
;; 2.70
;; an example.
;; 
;; result is an 86-bit sequence.
;; with a fixed-length code: 108.
;; 20% less space used.
;-------------------------  

;-------------------------
;; 2.71
;; f(n) = 2**(n - 1)
;;
;; takes n - 1 bits for the least frequent symbol.
;; takes a single bit for the most frequent symbol.
;-------------------------

;-------------------------
;; 2.72
;; order of growth.
;;
;; O(n) for the most frequent symbol. (memq is O(n))
;; O(n^2) worst case (least frequent). (n memqs)
;; could be optimized with a faster memq. (logn)
;-------------------------
