#lang sicp

; Metacircular Evaluator in Chapter 4


; Evaluates expression
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        (else
         (let ((op (get 'eval (get-operator exp))))
           (cond ((not (eq? op false)) (op (get-operands exp) env))
                 ((application? exp)
                  (apply (eval (get-operator exp) env)
                         (eval-list-of-values (get-operands exp) env)))
                 (else
                  (error "Unknown expression type: EVAL" exp)))))))

; Applies procedure to arguments
(define (apply procedure arguments)
  (error "Unknown expression type: APPLY" exp))


; Self-evaluating expressions

; Checks if expression is self-evaluating (i.e. number or symbol)
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


; Variables

; Checks if expression is a variable
(define (variable? exp) (symbol? exp))


; Quotes
; Has the form (quote <text-of-quotation?)

(define (get-quotation-text exp) (cadr exp))


; Assignments
; Has the form (set! <var> <value>)

(define (get-assignment-variable exp) (car exp))
(define (get-assignment-value exp) (cadr exp))


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


; Lambdas

(define (get-lambda-parameters exp) (car exp))
(define (get-lambda-body exp) (cdr exp))

; Constructs a lambda expression
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


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
  (define (make-begin seq) (cons 'begin seq))
  (cond ((null? seq) seq)
        ((last-exp? seq) (get-first-exp seq))
        (else (make-begin seq))))


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


; Let expressions
(define (get-let-assignments exp) (cadr exp))
(define (get-let-body exp) (cddr exp))

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
      (cons (make-lambda vars (get-let-body exp)) vals))))



; Data Directed Programming
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (table-op proc-if-found proc-if-no-subkey proc-if-no-key)
      (let ((check-table (lambda (key-1 key-2)
        (let ((subtable
               (assoc key-1 (cdr local-table))))
          (if subtable
              (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (proc-if-found record)
                    (proc-if-no-subkey subtable)))
              (proc-if-no-key local-table))))))
        check-table))
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
  (put 'eval 'begin (lambda (exp env) (eval-sequence exp env)))
  (put 'eval 'cond (lambda (exp env) (eval (cond->if exp) env)))
  (put 'eval 'let (lambda (exp env) (let->combination exp)))
  (put 'eval 'and (lambda (exp env) (eval (and->if exp) env)))
  (put 'eval 'or (lambda (exp env) (eval (or->if exp) env))))