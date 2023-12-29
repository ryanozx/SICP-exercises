#lang sicp

; Metacircular Evaluator in Chapter 4


; Evaluates expression
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((application? exp)
         (apply (eval (get-operator exp) env)
                (eval-list-of-values (get-operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

; Applies procedure to arguments
(define (apply procedure arguments)
  (error "Unknown expression type: APPLY" exp))


; Evaluates list of values
(define (eval-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (get-first-operand exps) env)
            (eval-list-of-values (get-rest-operands exps) env))))

; Checks if expression is self-evaluating (i.e. number or symbol)
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


; Checks if expression is an application - not a pre-defined type
; but nevertheless a procedure that can be applied on a list of values
(define (application? exp) (pair? exp))

; Interface for interacting with application expressions
(define (get-operator exp) (car exp))
(define (get-operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (get-first-operand exps) (car exps))
(define (get-rest-operands exps) (cdr exps))