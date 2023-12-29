#lang sicp

; Metacircular Evaluator in Chapter 4


; Evaluates expression
(define (eval exp env)
  (cond ((application? exp)
         (apply (eval (get-operator exp) env)
                (eval-list-of-values (get-operands exp) env)))
        ((self-evaluating? exp) exp)
        ((quoted? exp) (get-quotation-text exp))
        ((begin? exp) (eval-sequence (get-begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else
         (error "Unknown expression type: EVAL" exp))))

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


;Quotes

; Checks if expression is a quote, which has the form
; (quote <text-of-quotation?)
(define (quoted? exp) (tagged-list? exp 'quote))

; Interface for interacting with quotes
(define (get-quotation-text exp) (cadr exp))


; Assignments

; Checks if expression is an assignment, which has the form
; (set! <var> <value>)
(define (assignment? exp) (tagged-list? exp 'set!))

; Interface for interacting with assignments
(define (get-assignment-variable exp) (cadr exp))
(define (get-assignment-value exp) (caddr exp))


; Definitions

; Checks if expression is a definition, which has the form
; (define <var> <value>) or
; (define (<var> <param 1> ... <param n>) <body)
(define (definition? exp) (tagged-list? exp 'define))

; Interface for interacting with definitions
(define (get-definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (get-definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))


; Lambdas

; Checks if expression is a lambda expression
(define (lambda? exp) (tagged-list? exp 'lambda))

; Interface for interacting with lambdas
(define (lambda parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

; Constructs a lambda expression
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


; Conditionals - uses lazy evaluation

; Checks if expression is a conditional (single predicate)
(define (if? exp) (tagged-list? exp 'if))

; Checks if expression is a conditional (multiple predicates)
(define (cond? exp) (tagged-list? exp 'cond))

; Interface for interacting with conditionals (single predicate)
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

; Interface for interacting with conditionals (multiple predicates)
(define (get-cond-clauses exp) (cdr exp))
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
              (make-if (get-cond-predicate first)
                       (sequence->exp (get-cond-actions first))
                       (expand-clauses rest))))))
  (expand-clauses (get-cond-clauses exp)))
  

; Sequences

; Evaluates a sequence
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (get-first-exp exps) env))
        (else
         (eval (get-first-exp exps) env)
         (eval-sequence (get-rest-exps exps) env))))

; Checks if expression is a sequence
(define (begin? exp) (tagged-list? exp 'begin))

; Constructs an expression from a sequence
(define (sequence->exp seq)
  (define (make-begin seq) (cons 'begin seq))
  (cond ((null? seq) seq)
        ((last-exp? seq) (get-first-exp seq))
        (else (make-begin seq))))

; Interface for interacting with sequences
(define (get-begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (get-first-exp seq) (car seq))
(define (get-rest-exps seq) (cdr seq))


; Application

; Checks if expression is an application - not a pre-defined type
; but nevertheless a procedure that can be applied on a list of values
(define (application? exp) (tagged-list? exp 'call))

; Evaluates list of values from left-to-right
(define (eval-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (get-first-operand exps) env)))
        (let ((right (eval-list-of-values (get-rest-operands exps) env)))
          (cons left right)))))

; Interface for interacting with application expressions
(define (get-operator exp) (cadr exp))
(define (get-operands exp) (cddr exp))
(define (no-operands? ops) (null? ops))
(define (get-first-operand exps) (car exps))
(define (get-rest-operands exps) (cdr exps))

; Checks if list is a tagged list
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))