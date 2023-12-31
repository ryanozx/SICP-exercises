#lang sicp

; Metacircular Evaluator in Chapter 4

; Evaluates expression
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else
         (let ((op (get 'eval (get-operator exp))))
           (cond ((not (eq? op false)) (op (get-operands exp) env))
                 ((application? exp)
                  (my-apply (eval (get-operator exp) env)
                         (eval-list-of-values (get-operands exp) env)))
                 (else
                  (error "Unknown expression type: EVAL" exp)))))))

; Applies procedure to arguments
(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (get-procedure-body procedure)
          (extend-environment
           (get-procedure-parameters procedure)
           arguments
           (get-procedure-env procedure))))
        (else
         (error "Unknown expression type: APPLY" exp))))


; Self-evaluating expressions

; Checks if expression is self-evaluating (i.e. number or symbol)
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


; =========================================
; Variables
; Checks if expression is a variable
(define (variable? exp) (symbol? exp))


; =========================================
; Quotes
; Has the form (quote <text-of-quotation?)
(define (get-quotation-text exp) (car exp))


; =========================================
; Assignments
; Has the form (set! <var> <value>)
(define (get-assignment-variable exp) (car exp))
(define (get-assignment-value exp) (cadr exp))

(define (make-assignment var val) (list 'set! var val))

; Assigns value to variable in the environment
(define (eval-assignment exp env)
  (set-variable-value! (get-assignment-variable exp)
                       (eval (get-assignment-value exp) env)
                       env)
  'ok)


; =========================================
; Definitions
; Has the form (define <var> <value>) or (define (<var> <param 1> ... <param n>) <body)
(define (get-definition-variable exp)
  (if (symbol? (car exp))
      (car exp)
      (caar exp)))
(define (get-definition-value exp)
  (if (symbol? (car exp))
      (cadr exp)
      (make-lambda (cdar exp)
                   (cdr exp))))
(define (get-definition-params exp)
  (if (symbol? (car exp))
      (error "DEFINE has no params:" exp)
      (cdar exp)))

(define (define? exp) (tagged-list? exp 'define))

; Defines variables
(define (eval-definition exp env)
  (define-variable! (get-definition-variable exp)
    (eval (get-definition-value exp) env)
    env)
  'ok)


; =========================================
; Lambdas

(define (get-lambda-parameters exp) (car exp))
(define (get-lambda-body exp) (cdr exp))

(define (lambda->procedure exp env)
  (make-procedure (get-lambda-parameters exp)
                  (scan-out-defines (get-lambda-body exp))
                  env))

; Constructs a lambda expression
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


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

; Evaluates if expressions
(define (eval-if exp env)
  (if (true? (eval (get-if-predicate exp) env))
      (eval (get-if-consequent exp) env)
      (eval (get-if-alternative exp) env)))

; Constructs an if expression
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; Converts cond expression into nested if expressions
(define (cond->if exp)
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (get-cond-actions first))
                  (error "ELSE clause isn't last: COND->IF" clauses))
              (let ((first-pred (get-cond-predicate first))
                    (first-action (get-cond-actions first)))
                (if (and (pair? first-action) (eq? (car first-action) '=>))
                    (make-if first-pred (list (cadr first-action) first-pred) (expand-clauses rest))
                    (make-if (get-cond-predicate first)
                       (sequence->exp (get-cond-actions first))
                       (expand-clauses rest))))))))
  (expand-clauses exp))
  

; =========================================
; Sequences
(define (last-exp? seq) (null? (cdr seq)))
(define (get-first-exp seq) (car seq))
(define (get-rest-exps seq) (cdr seq))

; Evaluates a sequence
(define (eval-sequence exps env)
  (cond ((null? exps) (error "BEGIN has no clauses: " exps))
        ((last-exp? exps)
         (eval (get-first-exp exps) env))
        (else
         (eval (get-first-exp exps) env)
         (eval-sequence (get-rest-exps exps) env))))

; Constructs an expression from a sequence
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (get-first-exp seq))
        (else (make-begin seq))))

; Constructs a sequence
(define (make-begin seq) (cons 'begin seq))


; =========================================
; Application
; Checks if expression is an application - not a pre-defined type
; but nevertheless a procedure that can be applied on a list of values
(define (application? exp) (pair? exp))

; Evaluates list of values from left-to-right
(define (eval-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (get-first-operand exps) env)))
        (let ((right (eval-list-of-values (get-rest-operands exps) env)))
          (cons left right)))))

(define (get-operator exp) (car exp))
(define (get-operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (get-first-operand exps) (car exps))
(define (get-rest-operands exps) (cdr exps))


; =========================================
; Binary operators

(define (last-bool-expr? exp) (null? (cdr exp)))
(define (get-first-bool-exp exp) (car exp))
(define (get-rest-bool-exps exp) (cdr exp))

; Converts AND expression into nested if expressions (to take advantage
; of short-circuiting
(define (and->if exp)
  (cond ((null? exp) true)
        ((last-bool-expr? exp) exp)
        (else
         (make-if (get-first-bool-exp exp)
                  (and->if (get-rest-bool-exps exp))
                  false))))

; Converts OR expression into nested if expressions (to take advantage
; of short circuiting
(define (or->if exp)
  (cond ((null? exp) false)
        ((last-bool-expr? exp) exp)
        (else
         (make-if (get-first-bool-exp exp)
                  true
                  (get-rest-bool-exps exp)))))


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

(define (make-let-assignment var val) (list var val))

; Constructors
(define (make-let assignments body) (list 'let assignments body))
(define (make-let* assignments body) (list 'let* assignments body))
(define (make-named-let var assignments body) (list 'let var assignments body))

; Transforms let expression into a procedure
(define (let->combination exp)
  (define (generate-assignment-lists assignments)
    (if (null? assignments)
        (cons nil nil)
        (let ((rest (generate-assignment-lists (cdr assignments)))
              (first-var (caar assignments))
              (first-val (cdar assignments)))
          (cons (cons first-var (car rest)) (cons first-val (cdr rest))))))
  (let ((assignments (generate-assignment-lists (get-let-assignments exp))))
    (let ((vars (car assignments))
          (vals (cdr assignments)))
      (let ((proc (make-lambda vars (get-let-body exp))))
        (if (named-let? exp)
            (cons (make-lambda '()
                               (list (list 'define (get-let-name exp) proc)
                                     (cons (get-let-name exp) vals))) '())
            (cons proc vals))))))

; Transforms let* expression into a sequence of nested let-expressions to
; guarantee assignment order and enable utilising previous assignments in
; subsequent assignments
(define (let*->nested-lets exp)
  (let ((assignments (get-let-assignments exp)))
    (if (null? assignments)
        (get-let-body exp)
        (let ((first-assignment (car assignments))
              (rest-assignment (cdr assignments)))
          (list 'let (list first-assignment)
                (let*->nested-lets (cons rest-assignment (get-let-body exp))))))))


; =========================================
; Loops
; These loop constructs are not fully derived expressions, but the current implementations
; so far avoid any possibly of causing a namespace clash

; Do expressions have the form (do <condition to remain true> <action>)
(define (get-do-condition exp) (car exp))
(define (get-do-action exp) (cdr exp))
(define (do->combination exp)
  (let ((body (sequence->exp (get-do-action exp)))
        (cond (get-do-condition exp)))
    (make-begin (list body
                      (make-if cond
                               (make-do cond body)
                               'exit)))))

(define (make-do condition action) (list 'do condition action))

; While expressions have the form (while <condition is true> <action>)
(define (get-while-condition exp) (car exp))
(define (get-while-action exp) (cdr exp))
(define (while->combination exp)
  (let ((body (sequence->exp (get-while-action exp)))
        (cond (get-while-condition exp)))
    (make-if cond
             (make-begin (list body
                               (make-while cond body)))
             'exit)))

(define (make-while condition action) (list 'while condition action))

; For expressions have the form (for <variable> <iterable> <body>) (Python-style)
(define (get-for-variable exp) (car exp))
(define (get-for-iterable exp) (cadr exp))
(define (get-for-body exp) (cddr exp))
(define (for->combination exp)
  (let ((var (get-for-variable exp))
        (iterable (get-for-iterable exp))
        (body (get-for-body exp)))
    (if (null? iterable)
        'exit
        (make-let (list (cons var (car iterable)))
                  (make-begin (list (sequence->exp body)
                                    (make-for var (cdr iterable) body)))))))

(define (make-for var iterable body) (list 'for var iterable body))

; Until expressions have the form (until <condition> <action>)
(define (get-until-condition exp) (car exp))
(define (get-until-action exp) (cdr exp))
(define (until->combination exp)
  (let ((condition (get-until-condition exp))
        (action (get-until-action exp)))
    (make-if condition
             'exit
             (make-begin (list action
                               (make-until condition action))))))

(define (make-until condition action) (list 'until condition action))


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
  (list 'procedure parameters body env))

; Transforms procedure body to one that does not have internal definitions
(define (scan-out-defines body)
  (define (split seq)
    (if (null? seq)
        (cons nil nil)
        (let ((first (get-first-exp seq))
              (rest (split (get-rest-exps seq))))
          (if (define? first)
              (cons (cons first (car rest)) (cdr rest))
              (cons (car rest) (cons first (cdr rest)))))))
  (let ((split-defs (split body)))
    (let ((defs (car split-defs))
          (body (cdr split-defs)))
      (if (null? defs)
          body
          (append defs body)))))
              

; Checks whether procedure is a user-defined procedure
(define (compound-procedure? p)
  (tagged-list? p 'procedure))

; Tagged lists
(define (tagged-list? exp tag) (and (pair? exp) (eq? tag (car exp))))

; Primitive procedures
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

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
  (let ((val (cdr (env-loop env var))))
    (if (eq? val unassigned-val)
        (error "Variable is unassigned:" var)
        val)))

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
  (put 'eval 'quote (lambda (exp env) (get-quotation-text exp)))
  (put 'eval 'set! (lambda (exp env) (eval-assignment exp env)))
  (put 'eval 'define (lambda (exp env) (eval-definition exp env)))
  (put 'eval 'if (lambda (exp env) (eval-if exp env)))
  (put 'eval 'lambda (lambda (exp env) (lambda->procedure exp env)))
  (put 'eval 'begin (lambda (exp env) (eval-sequence exp env)))
  (put 'eval 'cond (lambda (exp env) (eval (cond->if exp) env)))
  (put 'eval 'let (lambda (exp env) (let->combination exp)))
  (put 'eval 'let* (lambda (exp env) (let*->nested-lets exp)))
  (put 'eval 'and (lambda (exp env) (eval (and->if exp) env)))
  (put 'eval 'or (lambda (exp env) (eval (or->if exp) env)))
  (put 'eval 'do (lambda (exp env) (do->combination exp)))
  (put 'eval 'while (lambda (exp env) (while->combination exp)))
  (put 'eval 'for (lambda (exp env) (for->combination exp)))
  (put 'eval 'until (lambda (exp env) (until->combination exp)))
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
    initial-env))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

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
(define (test)
  (left-to-right-eval-test)
  (get-operator-test)
  (get-operands-test)
  
  (self-evaluating-test)
  (variable-test)
  (quote-test)
  (define-test)
  (set!-test)

  (unbind-test)
  )

(define (left-to-right-eval-test) (equal? (eval-list-of-values (list '(quote a) '(quote 2) '(quote 3)) '()) (list 'a 'b 'c)))
(define (get-operator-test) (equal? (get-operator '(quote a)) 'quote))
(define (get-operands-test) (equal? (get-operands '(quote a)) '(a)))

(define (self-evaluating-test)
  ; numbers
  (equal? (eval 2 '()) 2)
  ; strings
  (equal? (eval (string #\a) '()) (string #\a)))

(define (variable-test)
  (let ((env (extend-environment '(x) '(1) global-environment)))
    (equal? (eval 'x env) 1)
    (let ((rep-env (extend-environment '() '() env)))
      (equal? (eval 'x rep-env) 1))))

(define (quote-test) (equal? (eval '(quote a) '()) 'a))

(define (define-test)
  (let ((env (extend-environment nil nil global-environment)))
    (equal? (eval '(begin (define x 1) x) env) 1)))

(define (set!-test)
  (let ((env (extend-environment '(x) '(1) empty-environment)))
    (equal? (eval '(begin (set! x 2) x) env) 2)))

(define (unbind-test)
  (let ((env (extend-environment '(x) '(1) empty-environment)))
    (let ((rep-env (extend-environment '(x) '(2) env)))
      (make-unbound! 'x rep-env)
      (equal? (eval 'x rep-env) 1))))