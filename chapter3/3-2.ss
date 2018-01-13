; The Environment Model of Evaluation
;;--------------------------------------------------

; each function call results in the creation of a new environment.

;-------------------------
;; 3.9
;; environment structures.
;; 
;; Each recursive call to factorial creates 3 frames,
;; the factorial frame, the (= n 1) frame, and the (* n (f (- n 1))) frame.
;; The terminating case creates 2 frames.
;; So (factorial 6) creates n * 3 - 1 = 17 frames.
;;
;; 4 frames per call for the iterative version,
;; the fact-iter frame, one (>) frame, one (*) frame, one (+) frame.
;; The last call creates one frame.
;; So (fact-iter 1 1 6) creates (n - 1) * 4 + 1 = 21 frames.
;-------------------------

;-------------------------
;; 3.10
;; evaluating let.
;;
;; Since let evaluates to a lambda, the symbol W1 in the global env will
;; point to a lambda with the parameter intial-amount. The body of this
;; lambda will in turn point to another lambda with parameter amount.
;; This lambda will reference a local environment in which balance is bound.
;-------------------------

;-------------------------
;; 3.11
;; local state.
;;
;; the variable balance will be accessed by both accounts.
;-------------------------
