; 1.12
; generating elements of the pascal triangle recursively

(define (pascal i j)
  (cond ((or (< i 0) (> j i) (< j 0)) 0) ; error inputs
        ((= i 0) 1)                      ; base condition
        ((or (= j 0) (= j i)) 1)         ; base condition
        (else (+ (pascal (- i 1) j)      ; recursive definition
                 (pascal (- i 1) (- j 1))))))
