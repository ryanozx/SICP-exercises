#lang sicp
; Logic Programming Evaluator in Chapter 4

; Evaluates expression
(define (eval exp env) ((analyse exp) env))
(define (analyse exp)
  (cond ((self-evaluating? exp) (analyse-self-evaluating exp))
        ((variable? exp) (analyse-variable exp))
        (else
         (let ((op (get 'analyse (get-operator exp))))
           (cond ((not (eq? op false)) (op (get-operands exp)))
                 ((application? exp) (analyse-application exp))
                 (else (error "Unknown expression type: ANALYSE" exp)))))))

; =========================================
; Self-evaluating expressions

; Checks if expression is self-evaluating (i.e. number or symbol)
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; Analyse self-evaluating expressions
(define (analyse-self-evaluating exp)
  (lambda (env) exp))


; =========================================
; Variables

; Checks if expression is a variable
(define (variable? exp) (symbol? exp))

; Analyse variable expression
(define (analyse-variable exp)
  (lambda (env) (lookup-variable-value exp env)))


; =========================================
; Quotes

; Has the form (quote <text-of-quotation?)
(define (get-quotation-text exp) (car exp))

; Analyse quote expressions
(define (analyse-quoted exp)
  (let ((qval (get-quotation-text exp)))
    (lambda (env) qval)))


; =========================================
; Assignments
  
; Has the form (set! <var> <value>)
(define (get-assignment-var exp) (car exp))
(define (get-assignment-val exp) (cadr exp))
(define (make-assignment var val) (tag-exp 'set! (list var val)))
; Analyses assignment statement
(define (analyse-assignment exp)
  (let ((var (get-assignment-var exp))
        (vproc (analyse (get-assignment-val exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))


; =========================================
; Definitions

; Has the form (define <var> <value>) or (define (<var> <param 1> ... <param n>) <body)
(define (get-definition-var exp)
  (if (define-var? exp)
      (car exp)
      (caar exp)))
(define (get-definition-val exp)
  (if (define-var? exp)
      (cadr exp)
      ; Function body is passed as a list
      (make-lambda (get-definition-params exp)
                   (cdr exp))))
(define (get-definition-params exp)
  (if (define-var? exp)
      (error "DEFINE has no params:" exp)
      (cdar exp)))
      
(define (define? exp) (tagged-list? exp 'define))
(define (define-var? exp) (symbol? (car exp)))

; Constructors - val should be a single expression
(define (make-define-var var val) (tag-exp 'define (list var val)))
(define (make-define-proc var params val) (tag-exp 'define (list (cons var params) val)))

; Analyses definition statements
(define (analyse-definition exp)
  (let ((var (get-definition-var exp))
        (vproc (analyse (get-definition-val exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))


; =========================================
; Lambdas
(define (get-lambda-parameters exp) (car exp))
(define (get-lambda-body exp) (cdr exp))

; Constructs a lambda expression - body should be a list
(define (make-lambda parameters body)
  (tag-exp 'lambda (cons parameters body)))

; Analyse lambda expression
(define (analyse-lambda exp)
  (let ((vars (get-lambda-parameters exp))
        (bproc (analyse-sequence (get-lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))


; =========================================
; Conditionals - uses lazy evaluation
; Has the form (if <predicate> <consequent> <alternative>) (single condition)
; or (cond ((<predicate> <action>) (<test> => <recipient>) ... (else <alternative>))
(define (get-if-predicate exp) (car exp))
(define (get-if-consequent exp) (cadr exp))
(define (get-if-alternative exp)
  (if (not (null? (cddr exp)))
      (caddr exp)
      'false))
(define (cond-else-clause? clause)
  (eq? (get-cond-predicate clause) 'else))
(define (get-cond-predicate clause) (car clause))
(define (get-cond-actions clause) (cdr clause))

; Analyses if expressions
(define (analyse-if exp)
  (let ((pproc (analyse (get-if-predicate exp)))
        (cproc (analyse (get-if-consequent exp)))
        (aproc (analyse (get-if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
                      (cproc env)
                      (aproc env)))))

; Constructs an if expression
(define (make-if predicate consequent alternative)
  (tag-exp 'if (list predicate consequent alternative)))

; Converts cond expression into nested if expressions
(define (cond->if exp)
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (make-begin (get-cond-actions first))
                  (error "ELSE clause isn't last: COND->IF" clauses))
              (let ((first-pred (get-cond-predicate first))
                    (first-action (get-cond-actions first)))
                (if (and (pair? first-action) (eq? (car first-action) '=>))
                    (make-if first-pred (list (cadr first-action) first-pred) (expand-clauses rest))
                    (make-if (get-cond-predicate first)
                       (make-begin (get-cond-actions first))
                       (expand-clauses rest))))))))
  (expand-clauses exp))


; =========================================
; Sequences
(define (last-exp? seq) (and (pair? seq) (null? (cdr seq))))
(define (get-first-exp seq) (car seq))
(define (get-rest-exps seq) (cdr seq))

; Analyses a sequence - exps should be a list
(define (analyse-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((no-internal-defs (scan-out-defines exps)))
    (let ((procs (map analyse no-internal-defs)))
      (if (null? procs)
          (error "Empty sequence: ANALYSE"))
      (loop (car procs) (cdr procs)))))

; Constructs a sequence - seq should be a list
(define (make-begin seq) (tag-exp 'begin seq))


; =========================================
; Application
; Checks if expression is an application - not a pre-defined type
; but nevertheless a procedure that can be applied on a list of values
(define (application? exp) (pair? exp))

; Evaluates arguments
(define (eval-list-of-values exps env)
  (if (no-args? exps)
      '()
      (let ((left (eval (get-first-arg exps) env)))
        (let ((right (eval-list-of-values (get-rest-args exps) env)))
          (cons left right)))))

(define (get-operator exp) (car exp))
(define (get-operands exp) (cdr exp))

(define (no-args? args) (null? args))
(define (get-first-arg args) (car args))
(define (get-rest-args args) (cdr args))

; Analyse application expressions
(define (analyse-application exp)
  (let ((fproc (analyse (get-operator exp)))
        (aprocs (map analyse (get-operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env)) aprocs)))))

; Executes application expressions
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((get-procedure-body proc)
          (extend-environment
           (get-procedure-parameters proc)
           args
           (get-procedure-env proc))))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION" proc))))

; =========================================
; Binary operators
  
(define (last-bool-expr? exp) (null? (cdr exp)))
(define (get-first-bool-exp exp) (car exp))
(define (get-rest-bool-exps exp) (cdr exp))

; Converts AND expression into nested if expressions (to take advantage
; of short-circuiting
(define (and->if exp)
  (cond ((null? exp) true)
        ((last-bool-expr? exp) (car exp))
        (else
         (make-if (get-first-bool-exp exp)
                  (and->if (get-rest-bool-exps exp))
                  'false))))

; Converts OR expression into nested if expressions (to take advantage
; of short circuitingx
(define (or->if exp)
  (cond ((null? exp) false)
        ((last-bool-expr? exp) (car exp))
        (else
         (make-if (get-first-bool-exp exp)
                  'true
                  (or->if (get-rest-bool-exps exp))))))


; =========================================
; Let expressions
(define (named-let? exp) (variable? (car exp)))
(define (get-let-name exp)
  (if (named-let? exp)
      (car exp)
      (error "LET has no procedure name:" exp)))
(define (get-let-assignments exp)
  (if (named-let? exp)
      (cadr exp)
      (car exp)))
(define (get-let-body exp)
  (if (named-let? exp)
      (cddr exp)
      (cdr exp)))
(define (get-let-vars assignments) (map car assignments))
(define (get-let-vals assignments) (map cadr assignments))
(define (make-let-assignment var val) (list var val))

; Constructors
(define (make-let assignments body) (tag-exp 'let (cons assignments body)))

; Transforms let expression into a procedure
(define (let->combination exp)
  (let ((assignments (get-let-assignments exp)))
    (let ((vars (get-let-vars assignments))
          (vals (get-let-vals assignments)))
      (let ((proc (make-lambda vars (get-let-body exp))))
        (if (named-let? exp)
            ; ( (define ...) (....) ))
            (make-begin
             (list (make-define-var (get-let-name exp) proc)
                   (cons (get-let-name exp) vals)))
            ; ((proc) (vals))
            (cons proc vals))))))

; Transforms let* expression into a sequence of nested let-expressions to
; guarantee assignment order and enable utilising previous assignments in
; subsequent assignments
(define (let*->nested-lets exp)
  (define (generate-nested-lets assignments body)
    (if (null? assignments)
        body
        (list (make-let (list (car assignments))
                        (generate-nested-lets (cdr assignments) body)))))
  (car (generate-nested-lets (get-let-assignments exp)
                             (get-let-body exp))))

; Transforms a letrec expression into a let expression
(define (letrec->let exp)
  (let ((assignments (get-let-assignments exp)))
    (let ((vars (get-let-vars assignments))
          (vals (get-let-vals assignments)))
      (make-let (map (lambda (var) (make-let-assignment var unassigned-val)) vars)
                (append (map (lambda (var val) (make-assignment var val)) vars vals)
                        (get-let-body exp))))))

; =========================================
; Data structures

; Truthiness - anything that is not explicitly 'false' is true
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

; Procedure representation
(define (get-procedure-parameters proc) (cadr proc))
(define (get-procedure-body proc) (caddr proc))
(define (get-procedure-env proc) (cadddr proc))

; Creates a procedure
(define (make-procedure parameters body env)
  (tag-exp 'procedure (list parameters body env)))

; Rearranges procedure body to enable simultaneous scope
(define (scan-out-defines body)
  (define (split seq)
    (if (null? seq)
        (cons nil nil)
        (let ((first (get-first-exp seq))
              (rest (split (get-rest-exps seq))))
          (if (define? first)
              (let ((stripped-first (get-operands first)))
                (cons (cons (get-definition-var stripped-first) (car rest))
                    (cons (make-assignment (get-definition-var stripped-first) (get-definition-val stripped-first))
                          (cdr rest))))
              (cons (car rest) (cons first (cdr rest)))))))
  (let ((split-defs (split body)))
    (let ((defs (car split-defs))
          (body (cdr split-defs)))
      (if (null? defs)
          body
          (append (map (lambda (var) (make-define-var var unassigned-val)) defs)
                  body)))))

; Checks whether procedure is a user-defined procedure
(define (compound-procedure? p)
  (tagged-list? p 'procedure))

; Tagged lists
(define (tagged-list? exp tag) (and (pair? exp) (eq? tag (car exp))))
(define (tag-exp tag exp) (cons tag exp))

; Primitive procedures
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'display display)
        (list 'newline newline)
        (list '< <)
        (list '> >)
        (list '() '())
        (list 'equal? equal?)
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (get-primitive-implementation proc) (cadr proc))

; Applies primitive procedures on the arguments
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (get-primitive-implementation proc) args))

; Need to declare this first as apply is overwritten
(define apply-in-underlying-scheme apply)


; =============================================================
; Environment representations
(define (get-enclosing-environment env) (cdr env))
(define (get-first-frame env) (car env))
(define empty-environment '())

; Creates a new frame for the environment
(define (make-frame vars vals)
  (define (fold xs ys proc)
    (if (or (null? xs) (null? ys))
        nil
        (cons (proc (car xs) (car ys)) (fold (cdr xs) (cdr ys) proc))))
  (if (= (length vars) (length vals))
      (cons '*frame* (fold vars vals make-binding))
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define (get-bindings frame) (cdr frame))
(define (get-first-binding bindings) (car bindings))
(define (get-rest-bindings bindings) (cdr bindings))

; Creates a new binding
(define (make-binding var val) (cons var val))
(define (get-binding-var binding) (car binding))
(define (get-binding-val binding) (cdr binding))

; Adds a binding of a variable and a value to the frame
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (make-binding var val) (get-bindings frame))))

; Adds a new frame to the environment
(define (extend-environment vars vals base-env)
  (let ((new-frame (make-frame vars vals)))
    (cons new-frame base-env)))

; Finds binding in environment
(define (env-loop env var)
    (define (scan bindings)
      (let ((binding (assoc var bindings)))
        (if binding
            binding
            (env-loop (get-enclosing-environment env) var))))
    (if (eq? env empty-environment)
        (error "Unbound variable" var)
        (scan (get-bindings (get-first-frame env)))))

; Checks the value of a variable if it exists in the environment
(define unassigned-val '*unassigned*)
(define (lookup-variable-value var env)
  (if (eq? var unassigned-val)
      unassigned-val
      (let ((val (cdr (env-loop env var))))
        (if (eq? val unassigned-val)
            (error "Variable is unassigned: " var)
            val))))

; Sets the value of a variable in the program
(define (set-variable-value! var val env) (set-cdr! (env-loop env var) val))

; Defines a variable - changes the value if it already exists in the
; current frame, otherwise it creates a new binding in the current frame
(define (define-variable! var val env)
  (let ((binding (assoc var (get-bindings (get-first-frame env)))))
    (if binding
        (set-cdr! binding val)
        (add-binding-to-frame! var val (get-first-frame env)))))

; Unbinds a variable binding from the current frame - acts only on the current
; frame to limit potential side-effects
(define (make-unbound! var env)
  (define (remove-binding bindings)
    (cond ((null? bindings) (error "Variable not in current frame:" var))
          ((eq? var (get-binding-var (get-first-binding bindings)))
           (get-rest-bindings bindings))
          (else
           (cons (get-first-binding bindings) (remove-binding (get-rest-bindings bindings))))))
  (set-car! env (cons '*frame* (remove-binding (get-bindings (get-first-frame env))))))


; =========================================
; Data Directed Programming
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (table-op proc-if-found proc-if-no-subkey proc-if-no-key)
      (lambda (key-1 key-2)
        (let ((subtable
               (assoc key-1 (cdr local-table))))
          (if subtable
              (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (proc-if-found record)
                    (proc-if-no-subkey subtable)))
              (proc-if-no-key local-table)))))
    (define (lookup key-1 key-2)
      (let ((return-false (lambda ignore false))
            (extract-record (lambda (record) (cdr record))))
        ((table-op extract-record return-false return-false) key-1 key-2)))
    (define (insert! key-1 key-2 value)
      (let ((modify-record
             (lambda (record) (set-cdr! record value)))
            (append-to-subtable
             (lambda (subtable) (set-cdr! subtable
                                            (cons (cons key-2 value) (cdr subtable)))))
            (append-to-table
             (lambda (table) (set-cdr! table
                                       (cons (list key-1 (cons key-2 value)) (cdr table))))))
        ((table-op modify-record append-to-subtable append-to-table) key-1 key-2)
        'ok))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

; Interface for interacting with tables
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define (install-eval-syntax)
  (put 'analyse 'quote analyse-quoted)
  (put 'analyse 'set! analyse-assignment)
  (put 'analyse 'define analyse-definition)
  (put 'analyse 'if analyse-if)
  (put 'analyse 'lambda analyse-lambda)
  (put 'analyse 'begin analyse-sequence)
  (put 'analyse 'cond (lambda (exp) (analyse (cond->if exp))))
  (put 'analyse 'let (lambda (exp) (analyse (let->combination exp))))
  (put 'analyse 'let* (lambda (exp) (analyse (let*->nested-lets exp))))
  (put 'analyse 'letrec (lambda (exp) (analyse (letrec->let exp))))
  (put 'analyse 'and (lambda (exp) (analyse (and->if exp))))
  (put 'analyse 'or (lambda (exp) (analyse (or->if exp))))
  )


; =========================================
; Setup
(install-eval-syntax)
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env
    ))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input global-environment)))
      (announce-output output-prompt)
      (user-print output)
      ))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (is-cons-pair? obj) (tagged-list? obj 'cons))

(define (user-print object)
  (if (and (pair? object) (compound-procedure? object))
      (display (list 'compound-procedure
                     (get-procedure-parameters object)
                     (get-procedure-body object)
                     '<procedure-env>))
      (display object)))

(define global-environment (setup-environment))
(driver-loop)


; =========================================
; Tests
; To run the tests, disable the driver loop above and call (test)
(define (test)
  (define pass-count 0)
  (define total 0)
  (define failed nil)
  (define (run-test test)
    (set! total (+ total 1))
    (if (equal? (test) true)
        (set! pass-count (+ pass-count 1))
        (set! failed (cons test failed))))
  (define (display-failed test)
    (display test)
    (newline))
  (for-each run-test
       (list
        and-test-true
        and-test-false
        or-test-true
        or-test-false
        cond-arrow-test
        let-test
        let*-test
        named-let-test
        append-test
        factorial-test
        letrec-test
        y-op-recursion-test
        y-op-fibonacci-test
        y-op-even-test
        ))
  (if (not (null? failed))
      (begin
        (display "Failed: ")
        (newline)
        (for-each display-failed failed)
        (newline)))
  (display pass-count)
  (display "/")
  (display total)
  (display " tests passed")
  (newline))

; 4.4: Implement 'and' and 'or' as derived expressions using shortcircuiting
(define (and-test-true)
  (equal? (eval '(and true true) (setup-environment)) true))

(define (and-test-false)
  (equal? (eval '(and true false) (setup-environment)) false))

(define (or-test-true)
  (equal? (eval '(or false true) (setup-environment)) true))

(define (or-test-false)
  (equal? (eval '(or false false) (setup-environment)) false))

; 4.5: Support (<test> => <recipient>)) clauses for cond
(define (cond-arrow-test)
  (equal? (eval '(cond ((cons 1 2) => car) (else false)) (setup-environment)) 1))

; 4.6: Implement let expressions as derived expressions
(define (let-test)
  (equal? (eval '(let ((x 2) (y 10)) (+ x y)) (setup-environment)) 12))

; 4.7: Implement let* expressions as nested-let expressions
(define (let*-test)
  (let ((output (eval '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z)) (setup-environment))))
    (equal? output 39)))

; 4.8: Support named-let (i.e. (let <var> <bindings> <body>))
(define (named-let-test)
  (let ((test-env (setup-environment)))
    (eval '(define (fib n)
             (let fib-iter ((a 1)
                            (b 0)
                            (count n)) (if (= count 0)
                                           b
                                           (fib-iter (+ a b) a (- count 1))))) test-env)
    (equal? (eval '(fib 10) test-env) 55)))

(define (append-test)
  (let ((test-env (setup-environment)))
    (eval '(define (append x y)
             (if (null? x)
                 y
                 (cons (car x) (append (cdr x) y)))) test-env)
    (equal? (eval '(append '(a b c) '(d e f)) test-env) '(a b c d e f))))

(define (factorial-test)
  (let ((test-env (setup-environment)))
    (eval '(define (factorial n)
             (if (= n 1) 1 (* (factorial (- n 1)) n))) test-env)
    (equal? (eval '(factorial 5) test-env) 120)))


; 4.20 Implement letrec
(define (letrec-test)
  (let ((test-env (setup-environment)))
    (equal? (eval '(letrec ((fact (lambda (n) (if (= n 1)
                                                  1
                                                  (* n (fact (- n 1)))))))
                     (fact 10))
                  test-env)
            3628800)))

; 4.21 Y-operator recursion
(define (y-op-recursion-test)
  (let ((test-env (setup-environment)))
    (equal? (eval '((lambda (n)
                      ((lambda (fact) (fact fact n))
                       (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
                    10) test-env)
            3628800)))

; 4.21(a) Y-operator Fibonacci
(define (y-op-fibonacci-test)
  (let ((test-env (setup-environment)))
    (equal? (eval '((lambda (n)
                      ((lambda (fib) (fib fib 1 0 n))
                       (lambda (fb a b count)
                         (if (= count 0)
                             b
                             (fb fb (+ a b) a (- count 1)))))) 10)
                  test-env)
            55)))

; 4.21(b) Y-operator even
(define (y-op-even-test)
  (let ((test-env (setup-environment)))
    (eval '(define (f x)
             ((lambda (even? odd?) (even? even? odd? x))
              (lambda (ev? od? n)
                (if (= n 0) true (od? ev? od? (- n 1))))
              (lambda (ev? od? n)
                (if (= n 0) false (ev? ev? od? (- n 1))))))
          test-env)
    (and (equal? (eval '(f 10) test-env) true)
         (equal? (eval '(f 9) test-env) false)
         )))

; =========================================

; 4.55
; 1. (supervisor ?person (Bitdiddle Ben))
; 2. (job ?person (accounting . ?job))
; 3. (address ?person (Slumerville . ?address))

; 4.56
; a. (and (supervisor ?person (Bitdiddle Ben)) (address ?person ?where))
; b. (and (salary (Bitdiddle Ben) ?ben-salary)) (salary ?person ?person-salary) (lisp-value < ?person-salary ?ben-salary))
; c. (and (supervisor ?person ?supervisor) (not (job ?supervisor (computer . ?type))) (job ?supervisor ?job))

; 4.57
; (rule (can-replace ?person1 ?person2) (and (job ?person1 ?job1) (job ?person2 ?job2) (or (same ?job1 ?job2) (can-do-job ?job1 ?job2)) (not (same ?person1 ?person2))))
; a. (can-replace ?p (Fect Cy D))
; b. (and (can-replace ?person1 ?person2) (salary ?person1 ?p1-salary) (salary ?person2 ?p2-salary) (list-value > ?p2-salary ?p1-salary))

; 4.58
; (rule (big-shot ?person ?division) (and (job ?person (?division . ?type)) (not (and (supervisor ?person ?boss) (job ?boss (?division . ?rest))))))

; 4.59
; a. (meeting ?division (Friday . ?time))
; b. (rule (meeting-time ?person ?day-and-time) (or (meeting whole-company ?day-and-time) (and (job ?person (?department . ?jobs)) (meeting ?department ?day-and-time))))
; c. (meeting-time (Hacker Alyssa P) (Wednesday . ?time))

