;; The Metacircular Evaluator
;--------------------------------------------------

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define apply-in-underlying-scheme apply)

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;-------------------------
;; 4.1
;; order of evaluation.
;-------------------------

(define (list-of-values-l2r exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env))
            (right (list-of-values-l2r (rest-operands exps) env)))
        (cons left right))))
(define (list-of-values-r2l exps env)
  (if (no-operands? exps)
      '()
      (let ((left (list-of-values-r2l (rest-operands exps) env))
            (right (eval (first-operand exps) env)))
        (cons left right))))


(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
(define (variable? exp) (symbol? exp))
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


;-------------------------
;; 4.2
;;
;; a. Due to the way applications are handled, the
;; evaluator will treat 'define' as an operator. The
;; test `application?` will always be true for a
;; compound expression.
;;
;; b.
;-------------------------

(define (new-application? exp)
  (tagged-list? exp 'call))
(define (operator- exp) (cadr exp))
(define (operands- exp) (cddr exp))

;; rest of the methods do not change
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;-------------------------
;; 4.3
;; data-directed eval.
;-------------------------

(define (dd-eval exp env)                  ; can be run after installing eval
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else ((get 'eval (tag exp) (operands exp) env)))))
(define (tag exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-eval)
  (define (eval-lambda exp)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env))
  (define (eval-begin exp)
    (eval-sequence (begin-actions exp) env))
  (define (eval-cond exp)
    (eval (cond->if exp) env))
  (define (eval-application exp)
    (apply (eval (operator exp) env)
           (list-of-values (operands exp) env)))
           
  (put 'eval 'quote text-of-quotation)
  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'lambda eval-lambda)
  (put 'eval 'begin eval-begin)
  (put 'eval 'cond eval-cond)
  (put 'eval 'call eval-application)
  'done)

;-------------------------
;; 4.4
;; and, or.
;-------------------------

(define (and? exp) (tagged-list? exp 'and))
(define (and-operands exp) (cdr exp))

(define (eval-and exp env)
  (define (eval-ops ops)
    (cond ((no-operands? ops) true)
          ((eval (first-operand ops) env) (eval-ops (rest-operands ops)))
          (else false)))
  (eval-ops (and-operands exp)))

(define (or? exp) (tagged-list? exp 'or))
(define (or-operands exp) (cdr exp))

(define (eval-or exp env)
  (define (eval-ops ops)
    (cond ((no-operands? ops) false)
          ((eval (first-operand ops) env) true)
          (else (eval-ops (rest-operands ops)))))
  (eval-ops (or-operands exp)))

(define (and->if exp)                   ; derived and
  (define (eval-ops ops)
    (if (null? ops)
        true
        (make-if (first-operand ops)
                 (eval-ops (rest-operands ops))
                 false)))
  (eval-ops (and-operands exp)))
(define (or->if exp)                    ; derived or
  (define (eval-ops ops)
    (if (null? ops)
        false
        (make-if (first-operand ops)
                 true
                 (eval-ops (rest-operands ops)))))
  (eval-ops (or-operands exp)))

;-------------------------
;; 4.5
;; cond - alternate clause.
;-------------------------

(define (cond-alternate-clause? clause)
  (eq? (cadr clause) '=>))
(define (alternate-clause-test clause)
  (car clause))
(define (alternate-clause-recipient clause)
  (caddr clause))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (if (cond-alternate-clause? first)
                (make-if (alternate-clause-test first)
                         (apply (alternate-clause-recipient first)
                                (list (alternate-clause-test first)))
                         (expand-clauses rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

;-------------------------
;; 4.6
;; let.
;-------------------------

(define (let? exp) (tagged-list? exp 'let))
(define (let-variables exp) (map car (cadr exp)))
(define (let-definitions exp) (map cadr (cadr exp)))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (cons (make-lambda (let-variables exp) (let-body exp))
        (let-definitions exp)))
  
;-------------------------
;; 4.7
;; let*.
;-------------------------

(define (make-let defs body) (cons 'let (cons defs body)))
(define (let*? exp) (tagged-list? 'let* exp))
(define (let*->nested-lets exp)
  (define (expand-lets lets)
    (let ((vars (let-variables lets))
          (defs (let-definitions lets)))
      (if (null? vars)
          (car (let-body exp))          ; using car to undo last list operation
          (if (null? defs)
              (error "No corresponding definition for variable " (car vars))
              (let ((first-var (car vars))
                    (first-def (car defs))
                    (body (let-body lets))
                    (rest-of-bindings (list (cdadr lets) (let-body lets))))
                (make-let
                 (list (list first-var first-def))
                 (list
                  (expand-lets (make-let rest-of-bindings)))))))))
  (expand-lets exp))


(define (named-let? exp)
  (and (let? exp) (symbol? (cadr exp))))
(define (named-let-var exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cadddr exp))
(define (let->combination exp)
  (if (named-let? exp)
      (let ((vars (map car (named-let-bindings exp))))
        (let ((proc (make-lambda vars (named-let-body exp)))
              (var (named-let-var exp)))
          (make-let
           (list (named-let-bindings exp)
                 (list var proc))
           (list var vars))))
      (cons (make-lambda (let-variables exp) (let-body exp))
            (let-definitions exp))))

;-------------------------
;; 4.9
;; iteration constructs.
;-------------------------

; for <init> <predicate> <increment> [<body>]
(define (for? exp) (tagged-list? exp 'for))
(define (for-init exp) (cadr exp))
(define (for-predicate exp) (caddr exp))
(define (for-increment exp) (cadddr exp))
(define (for-body exp) (car (cddddr exp)))
(define (for->combination exp)
  (let ((var (car (for-init exp)))
        (init-value (cadr (for-init exp))))
    (let ((proc-body
           (make-if (for-predicate exp)
                    (append (for-body exp)
                            (list (for-increment exp) '(*while-proc*)))
                    '())))
      (make-let
       (list (list '*while-proc* (make-lambda (list var) proc-body)))
       (cons '*while-proc* init-value)))))

;-------------------------
;; 4.10
;; redefining syntax.
;;
;; Without changing eval, we can just change
;; the `construct?` and `construct-part` functions.
;; Changing basic syntax, such as using [ instead
;; of (, isn't possible using this method.
;-------------------------


(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (if (list? body)
      (list 'procedure parameters (scan-out-defines body) env)
      (list 'procedure parameters body env)))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

; environment operations

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "Unassigned variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;-------------------------
;; 4.11
;; variable-value pairs.
;-------------------------

(define (make-frame- variables values)
  (map* '() cons variables values))
(define (frame-variables- frame) (map car frame))
(define (frame-values- frame) (map cdr frame))
(define (add-binding-to-frame!- var val frame)
  (set! frame (cons (cons var val) frame)))

(define (lookup-variable-value- var env)
  (define (env-loop env)
    (define (scan var-entries)
      (cond ((null? var-entries)
             (env-loop (enclosing-environment env)))
            ((eq? (car (car var-entries)) var)
             (cdr (car var-entries)))
            (else (scan (cdr var-entries)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (extend-environment- vars vals base-env)
  (let ((vars-length (length vars))
        (vals-length (length vals)))
    (if (= vars-length vals-length)
        (cons (make-frame- vars vals) base-env)
        (if (> vars-length vals-length)
            (error "More variables specified than values")
            (error "More values specified than variables")))))

(define (define-variable!- var val env)
  (let ((frame (first-frame env)))
    (define (scan var-entries)
      (cond ((null? var-entries)
             (add-binding-to-frame!- var val frame))
            ((eq? var (car (car var-entries)))
             (set-cdr! (car var-entries) val))
            (else (scan (cdr var-entries)))))
    (scan frame)))

(define (set-variable-value!- var val env)
  (define (env-loop env)
    (define (scan var-entries)
      (cond ((null? var-entries)
             (env-loop (enclosing-environment env)))
            ((eq? (car (car var-entries)) var)
             (set-cdr! (car var-entries) val))
            (else (scan (cdr var-entries)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET! " var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

;-------------------------
;; 4.12
;; refactoring env operations.
;-------------------------

(define (modify-frame-variable var env proc . alt-proc)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (if (null? alt-proc)
                 (let ((new-env (enclosing-environment env)))
                   (modify-frame-variable var new-env proc))
                 ((car alt-proc) vars vals frame)))
            ((eq? (car vars) var) (proc vars vals frame))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable")
        (else (scan (frame-variables frame) (frame-values frame))))))
(define (set-variable-value!- var val env)
  (let ((modifier (lambda (vs vls f) (set-car! vls val))))
    (modify-frame-variable var env modifier)))
(define (define-variable!- var val env)
  (let ((modifier (lambda (vs vls f) (set-car! vls val)))
        (alt-modifier (lambda (vs vls f)
                        (add-binding-to-frame! var val f))))
    (modify-frame-variable var env modifier alt-modifier)))
(define (lookup-variable-value- var env)
  (let ((modifier (lambda (vs vls f) (car vls))))
    (modify-frame-variable var env modifier)))

;-------------------------
;; 4.13
;; unbinding variables.
;;
;; Only unbinds variable in the given environment.
;; Less obstructive if multiple definitions for the
;; same identifier are present. Also faster than
;; the remove-all-bindings approach.
;-------------------------

(define (make-unbound! var env)
  (let ((modifier
         (lambda (vs vls f)
           (set! vs (cdr vs))
           (set! vls (cdr vls))
           'ok))
        (alt-modifier (lambda (vs vls f) 'ok))) ; variable undefined, do nothing
    (modify-frame-variable var env modifier alt-modifier)))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '= =)
        (list '* *)
        (list 'pp pp)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;-------------------------
;; 4.14
;; installed map vs defined map.
;;
;; Scheme-defined map is a named-lambda.
;; >> (pp map)
;; Apply has no way of handling this type of procedure yet.
;-------------------------

;-------------------------
;; 4.15
;; halting theorem.
;;
;; >> (try try) ; returns 'halted.
;; So (halts? try try) returns false.
;; Which means that (try try) will never halt.
;; This is a contradiction.
;;
;; >> (try try) ; does not return.
;; So (halts? try try) returns true.
;; Which means (try try) halted. Contradiction.
;-------------------------

(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

;-------------------------
;; 4.16
;; internal definitions.
;-------------------------

(define (scan-out-defines proc-body)
  (let ((defines (filter definition? proc-body))
        (rest-of-body (remove definition? proc-body)))
    (if (null? defines)
        proc-body                       ; no defines, nothing to do.
        (let ((vars (map definition-variable defines))
              (inits (map definition-value defines)))
          (list           ; since procedure-body uses caddr, not cddr.
           (make-let
            (map (lambda (var) (list var ''*unassigned*)) vars)
            (append
             (map* '() (lambda (var init) (list 'set! var init)) vars inits)
             rest-of-body)))))))

;-------------------------
;; 4.17
;; more diagrams.
;-------------------------

;-------------------------
;; 4.18
;; scanning out delayed defines.
;;
;; The method in the exercise will
;; work, since y and dy are available
;; and assigned when they are set.
;;
;; The earlier method will not work,
;; since the definitions must be
;; mutually exclusive.
;-------------------------

;-------------------------
;; 4.19
;; simultaneous definitions.
;;
;; Ben's version is wrong.
;; Alyssa's version is how it is.
;; Eva's version is how it should be.
;-------------------------

;-------------------------
;; 4.20
;; letrec.
;-------------------------

(define (letrec? exp) (tagged-list? 'letrec exp))
(define (letrec-vars exp) (map car (cadr exp)))
(define (letrec-exps exp) (map cdr (cadr exp)))
(define (letrec-body exp) (cddr exp))

(define (letrec->combination exp)
  (let ((unassigned-vars
         (map (lambda (x) (cons x '*unassigned*)) (letrec-vars exp)))
        (internal-vars '(a b c d e f g h i j)))
    (make-let unassigned-vars
              (let ((internal-defs
                     (map* '()
                           list
                           internal-vars
                           (letrec-exps exp)))
                    (internal-let-body
                     (map* '()
                           (lambda (x y) (list 'set! x y))
                           (letrec-vars exp)
                           internal-vars)))
                (cons
                 (make-let internal-defs internal-let-body)
                 (letrec-body exp))))))

;-------------------------
;; 4.21
;; Y operator.
;-------------------------

(define fibs
  ((lambda (n)
     ((lambda (fib)
        (fib fib n))
      (lambda (ft k)
        (cond ((<= k 0) 0)
              ((= k 1) 1)
              (else (+ (ft ft (- k 1)) (ft ft (- k 2))))))))
   10))

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

; separating analysis from execution

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((let? exp) (analyze (let->combination exp)))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))
(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;-------------------------
;; 4.22
;; analyzing let.
;-------------------------

(define (analyze-let exp)
  (let ((defprocs (map analyze (let-definitions exp)))
        (vars (let-variables exp)))
    (lambda (env)
      (let->combination
       (make-let (map* '()
                       (lambda (var dproc) (list var (dproc env)))
                       vars
                       defprocs)
                 (let-body exp))))))

;-------------------------
;; 4.23
;; alternate analyze-sequence.
;;
;; For a one-expression sequence,
;; the text-version gives the expression only,
;; the alyssa-version gives the analyzed expression.
;; For a two-expression sequence,
;; both versions give the same result.
;-------------------------

;-------------------------
;; 4.24
;; analyzed vs. non-analyzed code.
;-------------------------
