;; 2.4
;; Multiple Representations for Abstract Data
;--------------------------------------------------


;-------------------------
;; 2.73
;; data-directed deriv.
;;
;; a.
;; This is a function which uses operation tags.
;; The correct operation is taken from the table, based on the expression tag.
;; number? and same-variable? don't need their own generic operations.
;; b and c.
;; solved below.
;; d.
;; install procedure will change.
;-------------------------

(define (differentiate-sum sum)
  ; actual operation
  )

(define (differentiate-product product)
  ; actual operation
  )

(define (install-deriv)
  (put 'deriv '+ differentiate-sum)
  (put 'deriv '* differentiate-product))

(define (new-rule exp)
  ; actual operation
  )

(define (install-new-rule)
  (put 'deriv 'op new-rule))

;-------------------------
;; 2.74
;; generic record operations.
;; 
;; a, b, c.
;; The operation table should be used,
;; This is keyed by the division name & field selector.
;; d.
;; A new row is added.
;; All operations defined by that division have the same name.
;-------------------------

;-------------------------
;; 2.75
;; message-passing style.
;-------------------------

(define (make-from-mag-ang mag angle)
  (lambda (op)
    (cond ((eq? op 'real-part) (* mag (cos angle)))
          ((eq? op 'imag-part) (* mag (sin angle)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) angle))))

;-------------------------
;; 2.76
;; a summary.
;;
;; direct dispatch:
;; add a rule for the new type.
;; the new functions go under this rule.
;; data-directed style:
;; add a new column to the dispatch table.
;; message-passing style:
;; implement a dispatch function for the new feature.
;;
;; if types are frequently added, use data-directed.
;; apply-generic is unchanged if a new type is added.
;; 
;; if operations are frequently added, use message-passing.
;; other types are unaffected if the new operation isn't supported.
;-------------------------
