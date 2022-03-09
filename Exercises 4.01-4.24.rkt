#lang sicp

(define apply-in-underlying-scheme apply)

; table to store procedures
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table)))
    (define (lookup key1 key2)
      (let ((subtable (assoc key1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key1 key2 value)
      (let ((subtable (assoc key1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable (cons (cons key2 value) (cdr subtable)))))
            (set-cdr! local-table (cons (list key1 (cons key2 value)) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; apply
(define (applyn procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment
                         (procedure-parameters procedure)
                         arguments
                         (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type: APPLY" procedure))))

; checks if expression is primitive
(define (self-evaluating? exp) (cond ((number? exp) true)
                                     ((string? exp) true)
                                     (else false)))

; checks if expression is a variable
(define (variable? exp) (symbol? exp))

; quotes
(define (text-of-quotation exp)(car exp))

; assignment
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (evaln (assignment-value exp) env)
                       env)
  'ok)

(define (assignment-variable exp) (car exp))
(define (assignment-value exp) (cadr exp))
(define (make-assignment variable value) (tag-exp 'set! (list variable value)))

; definitions
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (evaln (definition-value exp) env)
    env)
  'ok)

(define (definition-variable exp)
  (if (symbol? (car exp))
      (car exp)
      (caar exp)))
(define (definition-value exp)
  (if (symbol? (car exp))
      (cadr exp)
      (make-lambda (cdar exp)
                   (cdr exp))))

; conditionals
(define (eval-if exp env)
  (if (true? (evaln (if-predicate exp) env))
      (evaln (if-consequent exp) env)
      (evaln (if-alternative exp) env)))

(define (if-predicate exp) (car exp))
(define (if-consequent exp) (cadr exp))
(define (if-alternative exp)
  (if (not (null? (cddr exp)))
      (caddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (tag-exp 'if  (list predicate consequent alternative)))

; sequences
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (tag-exp 'begin seq))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (evaln (first-exp exps) env))
        (else
         (evaln (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

; lambda
(define (lambda-parameters exp) (car exp))
(define (lambda-body exp) (cdr exp))
(define (make-lambda parameters body) (tag-exp 'lambda (cons parameters body)))

; boolean
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

; procedure representation
(define (make-procedure parameters body env)
  (tag-exp 'procedure (list parameters (scan-out-defines body) env)))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (install-eval-syntax)
  (put 'eval 'quote (lambda (exp env) (text-of-quotation exp)))
  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'begin eval-sequence)
  (put 'eval 'cond (lambda (exp env) (evaln (cond->if exp) env)))
  (put 'eval 'let (lambda (exp env) (evaln (let->combination exp) env)))
  (put 'eval 'let* (lambda (exp env) (evaln (let*->nested-lets exp) env)))
  (put 'eval 'letrec (lambda (exp env) (evaln (letrec->let exp) env)))
  (put 'eval 'lambda (lambda (exp env) (make-procedure (car exp) (cdr exp) env)))
  (put 'eval 'and eval-and)
  (put 'eval 'or eval-or))

#|
Exercise 4.1:Notice that we cannot tell whether the metacircular evaluator evaluates
operands from left to right or from right to left. Its evaluation order is inherited from
the underlying Lisp: If the arguments to cons in list-of-values are evaluated from left to
right, then list-of-values will evaluate operands from left to right; and if the arguments
to cons are evaluated from right to left, then list-of-values will evaluate operands from
right to left. Write a version of list-of-values that evaluates operands from left to right
regardless of the order of evaluation in the underlying Lisp. Also write a version of
list-of-values that evaluates operands from right to left.
|#

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((left (evaln (first-operand exps) env)))
        (let ((right (list-of-values (rest-operands exps) env)))
          (cons left right)))))

(define (list-of-values-rl exps env)
  (if (no-operands? exp)
      '()
      (let ((right (list-of-values (rest-operands exps) env)))
        (let ((left (evaln (first-operand exps) env)))
          (cons left right)))))

#|
Exercise 4.2: Louis Reasoner plans to reorder the cond clauses in eval so that the clause
for procedure applications appears before the clause for assignments. He argues that this
will make the interpreter more efficient: Since programs usually contain more applications
than assignments, definitions, and so on, his modified eval will usually check fewer clauses
than the original eval before identifying the type of an expression.

a. What is wrong with Louis’s plan? (Hint: What will Louis’s evaluator do with the
expression (define x 3)?)
b. Louis is upset that his plan didn’t work. He is willing to go to any lengths to make his
evaluator recognize procedure applications before it checks for most other kinds of
expressions. Help him by changing the syntax of the evaluated language so that procedure
applications start with call. For example, instead of (factorial 3) we will now have to write
(call factorial 3) and instead of (+ 1 2) we will have to write (call + 1 2)
|#

#|
By reordering the clauses such that the clause for procedure applications appear before the
clause for assignments, any expression that would have satisfied the predicate for
assignment would also satisfy the predicate pair? for procedure applications, thus it is
treated as an application.
|#

(define (louis-application? exp) (tagged-list? exp 'call))
(define (louis-operator exp) (cadr exp))
(define (louis-operands exp) (cddr exp))

#|
Exercise 4.3: Rewrite eval so that the dispatch is done in data-directed style. Compare this
with the data-directed differentiation procedure of Exercise 2.73. (You may use the car of a
compound expression as the type of the expression, as is appropriate for the syntax
implemented in this section.)
|#

; superceded by 4.22

(define (old-evaln expr env)
  (cond ((self-evaluating? expr) expr)
        ((variable? expr) (lookup-variable-value expr env))
        (else (let ((op (get 'eval (operator expr))))
                (cond ((not (eq? op false)) (apply-in-underlying-scheme op (list (operands expr) env)))
                      ((application? expr)
                       (applyn (old-evaln (operator expr) env)
                               (list-of-values (operands expr) env)))
                      (else (error "Unknown expression type: EVAL" expr)))))))

(define (application? exp) (pair? exp))
(define (operator expt) (car expt))
(define (operands expt) (cdr expt))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (tag-exp tag exp) (cons tag exp))
(define (tagged-list? exp tag) (if (pair? exp)
                                   (eq? (car exp) tag)
                                   false))

#|
Exercise 4.4: Recall the definitions of the special forms and and or from Chapter 1:
• and: The expressions are evaluated from left to right. If any expression evaluates to
false, false is returned; any remaining expressions are not evaluated. If all the
expressions evaluate to true values, the value of the last expression is returned. If there
are no expressions then true is returned.
• or: The expressions are evaluated from left to right. If any expression evaluates to a
true value, that value is returned; any remaining expressions are not evaluated. If all
expressions evaluate to false, or if there are no expressions, then false is returned.

Install and and or as new special forms for the evaluator by defining appropriate syntax
procedures and evaluation procedures eval-and and eval-or. Alternatively, show how to
implement and and or as derived expressions.
|#

(define (last-pred? seq) (null? (cdr seq)))
(define (first-pred seq) (car seq))
(define (rest-preds seq) (cdr seq))

(define (eval-and exps env)
  (cond ((null? exps) 'true)
        ((last-pred? exps) (evaln (first-pred exps) env))
        ((evaln (first-pred exps) env) (eval-and (rest-preds exps) env))
        (else 'false)))

(define (eval-or exps env)
  (cond ((null? exps) 'false)
        ((last-pred? exps) (evaln (first-pred exps) env))
        ((evaln (first-pred exps) env) 'true)
        (else (eval-or (rest-preds exps) env))))

(define (and->if exps) (expand-and-predicates exps))

(define (expand-and-predicates preds)
  (if (null? preds)
      'true
      (make-if (first-pred preds) (expand-and-predicates (rest-preds preds)) 'false)))

(define (or->if exps) (expand-or-predicates exps))

(define (expand-or-predicates preds)
  (if (null? preds)
      'false
      (make-if (first-pred preds) 'true (expand-or-predicates (rest-preds preds)))))


#|
Exercise 4.5: Scheme allows an additional syntax for cond clauses, (⟨test⟩ => ⟨recipient⟩).
If ⟨test⟩ evaluates to a true value, then ⟨recipient⟩ is evaluated. Its value must be a
procedure of one argument; this procedure is then invoked on the value of the ⟨test⟩, and
the result is returned as the value of the cond expression. For example

(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false))

returns 2. Modify the handling of cond so that it supports this extended syntax.
|#

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses exp))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (let ((first-pred (cond-predicate first))
                  (first-action (cond-actions first)))
              (if (and (pair? first-action) (eq? (car first-action) '=>))
                  (make-if first-pred (list (cadr first-action) first-pred) (expand-clauses rest))
                  (make-if first-pred (sequence->exp first-action) (expand-clauses rest))))))))

#|
Exercise 4.6: let expressions are derived expressions, because

(let ((⟨var1⟩ ⟨exp1⟩) ... (⟨varn⟩ ⟨expn⟩))
   ⟨body⟩)

is equivalent to

((lambda (⟨var1⟩ . . . ⟨varn⟩)
   ⟨body⟩)
 ⟨exp1⟩
 ...
 ⟨expn⟩)

Implement a syntactic transformation let->combination that reduces evaluating let
expressions to evaluating combinations of the type shown above, and add the appropriate
clause to eval to handle let expressions.
|#

(define (old-let-inits exp) (car exp))
(define (old-let-body exp) (cdr exp))
(define (old-let-vars exp) (map car (old-let-inits exp)))
(define (old-let-vals exp) (map cadr (old-let-inits exp)))
(define (old-make-let args body) (list 'let args body))

; superceded by let->combination in 4.8
(define (old-let->combination exp)
  (cons (make-lambda (old-let-vars exp) (old-let-body exp)) (old-let-vals exp)))

#|
Exercise 4.7: let* is similar to let, except that the bindings of the let* variables are
performed sequentially from left to right, and each binding is made in an environment in
which all of the preceding bindings are visible. For example

(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
  (* x z))

returns 39. Explain how a let* expression can be rewritten as a set of nested let
expressions, and write a procedure let*->nested-lets that performs this transformation. If
we have already implemented let (Exercise 4.6) and we want to extend the evaluator to handle
let*, is it sufficient to add a clause to eval whose action is
(eval (let*->nested-lets exp) env) or must we explicitly expand let* in terms of non-derived
expressions?
|#

#|
A let* expression can be rewritten as a set of nested let expressions, whereby each let
expression will have one assignment, and the body of each let expression except for the
innermost let expression will be another let expression.
|#

(define (let*-inits exp) (car exp))
(define (let*-body exp) (cdr exp))
(define (let*->nested-lets exp)
  (let ((args (let*-inits exp))
        (body (sequence->exp (let*-body exp))))
    (define (expand-let* inits)
      (if (null? inits)
          body
          (make-let (list (car inits)) (expand-let* (cdr inits)))))
    (expand-let* args)))
   
#|
It is sufficient. When the let* expression is evaluated, it creates an expression that is
a set of let expressions, and evaluates this expression. When this expression is evaluated,
only the outermost let expression i.e. the first binding in the let* expression is called
with let->combination, which turns it into a lambda expression with parameter being the
variable of the first binding and body being the rest of the nested let-expressions.
As such, let->combination does not recursively expand the nested let-expressions. When this
lambda expression is evaluated, it creates a procedure that is then called on its arguments
i.e. the value which the first variable in the let* expression is bound to. The evaluation
of this procedure application causes the body of the procedure to be evaluated i.e. the
rest of the set of nested let-expressions, in an environment that is extended to include the
first binding of the let*-expression. As such, the set of nested let-expressions is evaluated
recursively, hence there is no need to explicitly expand let* in terms of non-derived
expressions.
|#

#|
Exercise 4.8: “Named let” is a variant of let that has the form

(let ⟨var⟩ ⟨bindings⟩ ⟨body⟩)

e ⟨bindings⟩ and ⟨body⟩ are just as in ordinary let, except that ⟨var⟩ is bound within
⟨body⟩ to a procedure whose body is ⟨body⟩ and whose parameters are the variables in the
⟨bindings⟩. Thus, one can repeatedly execute the ⟨body⟩ by invoking the procedure named ⟨var⟩.
For example, the iterative Fibonacci procedure (Section 1.2.2) can be rewritten using
named let as follows:

(define (fib n)
   (let fib-iter ((a 1)
                  (b 0)
                  (count n))
      (if (= count 0)
          b
          (fib-iter (+ a b) a (- count 1)))))

Modify let->combination of Exercise 4.6 to also support named let.
|#

(define (named-let? exp) (variable? (car exp)))
(define (let-name exp) (car exp))
(define (let-inits exp)
  (if (named-let? exp)
      (cadr exp)
      (car exp)))
(define (let-body exp)
  (if (named-let? exp)
      (cddr exp)
      (cdr exp)))
(define (let-vars exp) (map car (let-inits exp)))
(define (let-vals exp) (map cadr (let-inits exp)))
(define (make-let bindings body) (list 'let bindings body))
(define (make-named-let var bindings body) (list 'let var bindings body))

(define (let->combination exp)
  (define proc (make-lambda (let-vars exp) (let-body exp)))
  (if (named-let? exp)
      (cons (make-lambda '() (list (tag-exp 'define (list (let-name exp) proc))
                                   (cons (let-name exp) (let-vals exp)))) '())
      (cons proc (let-vals exp))))

#|
Exercise 4.9: Many languages support a variety of iteration constructs, such as do, for,
while, and until. In Scheme, iterative processes can be expressed in terms of ordinary
procedure calls, so special iteration constructs provide no essential gain in computational
power. On the other hand, such constructs are often convenient. Design some iteration
constructs, give examples of their use, and show how to implement them as derived
expressions.
|#

#|
(do-while condition body):

body is evaluated once, then the condition is evaluated to see if it is true. body will
continue to be executed until the condition is false.

Example:
('define i 0)
('do-while (< i 10) ('set! i (+ i 1)))
|#

(define (do-while-cond exp) (car exp))
(define (do-while-body exp) (cdr exp))

(define (do-while->if exp)
    (let ((body (sequence->exp (do-while-body exp))))
      (make-if (do-while-cond exp) (tag-exp 'do-while (cons (do-while-cond exp) body)) 'exit)))

#|
(while condition body)

condition is evaluated to return either true or false. If true, body is evaluated once, then
the condition is evaluated again to check if it returns true. If true, the loop repeats until
the condition returns false.

Example:
('define i 0)
('while (< i 10) ('set! i (+ i 1)))
|#

(define (while-cond exp) (car exp))
(define (while-body exp) (cdr exp))

(define (while->if exp)
  (make-if (while-cond exp) ('begin (sequence->exp (while-body exp)) (tag-exp 'while exp)) 'exit))

#|
(do-until condition body):

condition is evaluated to return either true or false. If false, body is evaluated once,
then the condition is reevaluated again to check if it returns false. Every time it returns
false, body is evaluated and then the condition is checked again. If the condition returns
true, the loop is exited.

Example:
('define i 0)
('do-until (= i 15) ('set! i (+ i 1)))
|#

(define (do-until-cond exp) (car exp))
(define (do-until-body exp) (cdr exp))

(define (do-until->if exp)
  (make-if (do-until-cond exp) 'exit ('begin (do-until-body exp) (tag-exp 'do-until exp))))

#|
(for var data body):

At the start of the loop, var is bound to the first item of data, which should be a list.
body is then evaluated once, before returning to the start of the loop where var is bound
to the next item of data, and the loop continues until it reaches the end of data i.e. nil.

Example:
('for 'x '(1 2 3 4 5) (print (+ 'x 1)))
|#

(define (for-var exp) (car exp))
(define (for-data exp)
  (if (list? (cadr exp))
      (cadr exp)
      (error "List not provided -- FOR" (cadr exp))))
(define (for-body exp)
  (cddr exp))

(define (for->seq exp)
  (let ((for-seq (for-data exp)))
    (if (null? for-seq)
        'exit
        ('begin (make-assignment (for-var exp) (car for-seq))
                (for-body exp)
                (tag-exp 'for (list (for-var exp) (cdr for-seq) (for-body exp)))))))

(define (install-iter-eval-syntax)
  (put 'do-while (lambda (exp env) (evaln (do-while->if exp) env)))
  (put 'while (lambda (exp env) (evaln (while->if exp) env)))
  (put 'do-until (lambda (exp env) (evaln (do-until->if exp) env)))
  (put 'for (lambda (exp env) (evaln (for->seq exp) env))))

; iter-eval-syntax is not installed; add in (install-iter-eval-syntax) below
; install-eval-syntax to install it

#|Exercise 4.10: By using data abstraction, we were able to write an eval procedure that is
independent of the particular syntax of the language to be evaluated. To illustrate this,
design and implement a new syntax for Scheme by modifying the procedures in this section,
without changing eval or apply.
|#

#|
If we were to modify the syntax such that the operator is at the end of the list, we only
need to modify the tag-exp, operator, operand, and make-xxx functions.
|#

#|
Exercise 4.11: Instead of representing a frame as a pair of lists, we can represent a frame
as a list of bindings, where each binding is a name-value pair. Rewrite the environment
operations to use this alternative representation.
|#

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (frame-pairs frame) (cdr frame))
(define (make-frame vars vals) (cons 'frame (map cons vars vals)))

(define (binding-var binding) (car binding))
(define (binding-val binding) (cdr binding))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))

          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (old-lookup-variable-value var env)
  (cdr (find-pair-in-env var env)))

; superceded by 4.16

(define (set-variable-value! var val env)
  (set-cdr! (find-pair-in-env var env) val))

(define (define-variable! var val env)
  (let ((pair (assoc var (frame-pairs (first-frame env)))))
    (if pair
        (set-cdr! pair val)
        (add-binding-to-frame! var val (first-frame env)))))

#|
Exercise 4.12: The procedures set-variable-value!, define-variable! and
lookup-variable-value can be expressed in terms of more abstract procedures for traversing
the environment structure. Define abstractions that capture the common paerns and redefine
the three procedures in terms of these abstractions.
|#

(define (find-pair-in-env var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((pair (assoc var (frame-pairs (first-frame env)))))
          (if pair
              pair
              (env-loop (cdr env))))))
  (env-loop env))

; see 4.11

#|
Exercise 4.13: Scheme allows us to create new bindings for variables by means of define,
but provides no way to get rid of bindings. Implement for the evaluator a special form
make-unbound! that removes the binding of a given symbol from the environment in which the
make-unbound! expression is evaluated. This problem is not completely specified. For
example, should we remove only the binding in the first frame of the environment? Complete
the specification and justify any choices you make.
|#

(define (make-unbound! var env)
  (define (iter frame)
    (cond ((null? frame) (error "Unbound variable" var))
          ((eq? var (caar frame)) (cdr frame))
          (else (cons (car frame) (iter (cdr frame))))))
  (let ((new-frame (iter (frame-pairs (first-frame env)))))
    (if new-frame
        (set-car! env (cons 'frame new-frame)))))

#|
We only remove the binding in the first frame of the environment as it would be dangerous
if we allow the removal of bindings in the enclosing environments as it may have unknown
effects on the rest of the program.
|#

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        (list '< <)
        (list '> >)
        (list '>= >=)
        (list '<= <=)
        (list 'assoc assoc)
        (list 'cadr cadr)
        (list 'cddr cddr)
        ))
(define (primitive-procedure-names) (map car primitive-procedures))
(define (primitive-procedure-objects) (map (lambda (proc) (list 'primitive (cadr proc)))
                                           primitive-procedures))

(define (setup-environment)
  (let ((initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (evaln input the-global-environment)))
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
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

#|
Exercise 4.14: Eva Lu Ator and Louis Reasoner are each experimenting with the metacircular
evaluator. Eva types in the definition of map, and runs some test programs that use it. They
work fine. Louis, in contrast, has installed the system version of map as a primitive for
the metacircular evaluator. When he tries it, things go terribly wrong. Explain why Louis’s
map fails even though Eva’s works.
|#

#|
Louis's map does not work as the primitive map is applied to the internal representation of
the procedure. The internal representation of procedures that our interpreter uses is
different from the one Racket uses, thus Racket's evaluator is unable to evaluate our
representation.
|#

#|
Exercise 4.15: Given a one-argument procedure p and an object a, p is said to “halt” on a
if evaluating the expression (p a) returns a value (as opposed to terminating with an
error message or running forever). Show that it is impossible to write a procedure halts?
that correctly determines whether p halts on a for any procedure p and object a. Use the
following reasoning: If you had such a procedure halts?, you could implement the following
program:

(define (run-forever) (run-forever))
(define (try p)
   (if (halts? p p) (run-forever) 'halted))

Now consider evaluating the expression (try try) and show that any possible outcome (either
halting or running forever) violates the intended behavior of halts?.
|#

#|
Suppose halts? can correctly determine whether p halts on a for any procedure p and object
a.

Then when we call (try p), if p halts, (run-forever) is called, thus implying that (try p)
does not halt. If p does not halt, (run-forever) is called, thus implying that (try p) halts.

When we evaluate (try try), assuming that it halts, (run-forever) is called, thus (try try)
does not halt. Likewise, assuming that (try try) does not halt, (run-forever) is called, thus
(try try) halts. This is a contradiction, thus implying that such a procedure halts? does not
exist.
|#

#|
Exercise 4.16: In this exercise we implement the method just described for interpreting
internal definitions. We assume that the evaluator supports let (see Exercise 4.6).

a. Change lookup-variable-value (Section 4.1.3) to signal an error if the value it finds is
the symbol *unassigned*.

b. Write a procedure scan-out-defines that takes a procedure body and returns an equivalent
one that has no internal definitions, by making the transformation described above.

c. Install scan-out-defines in the interpreter, either in make-procedure or in
procedure-body (see Section 4.1.3). Which place is better? Why?
|#

(define (lookup-variable-value var env)
  (let ((val (cdr (find-pair-in-env var env))))
    (if (eq? val '*unassigned*)
        (error "Unassigned variable" var)
        val)))

(define (scan-out-defines proc-body)
  (define (extract-defines proc)
    (if (null? proc)
        (cons nil nil)
        (let ((first (first-exp proc))
              (rest (extract-defines (rest-exps proc))))
          (if (tagged-list? first 'define)
              (cons (cons (definition-variable (cdr first)) (car rest))
                    (cons (make-assignment (definition-variable (cdr first))
                                           (definition-value (cdr first)))
                          (cdr rest)))
              (cons (car rest) (cons first (cdr rest)))))))
  (let ((split-defines (extract-defines proc-body)))
    (let ((split-vars (car split-defines))
          (split-body (cdr split-defines)))
      (if (null? split-vars)
          split-body
          (list (tag-exp 'let (cons (map (lambda (var) (list var ''*unassigned*)) split-vars)
                                    split-body)))))))

#|
scan-out-defines is installed in make-procedure, since a procedure is only made once,
however there may be multiple calls to procedure-body in a program. As such, by installing
it in make-procedure, scan-out-defines only needs to be called once per procedure definition.
|#

#|
Exercise 4.17: Draw diagrams of the environment in effect when evaluating the expression
<e3> in the procedure in the text, comparing how this will be structured when definitions
are interpreted sequentially with how it will be structured if definitions are scanned out
as described. Why is there an extra frame in the transformed program? Explain why this
difference in environment structure can never make a difference in the behavior of a
correct program. Design a way to make the interpreter implement the “simultaneous” scope
rule for internal definitions without constructing the extra frame.
|#

#|

(lambda (vars) (define u (e1)) (define v (e2)) (e3))

Global environment:
other variables

lambda1:
 - variables: vars
 - body: (define u (e1))
         (define v (e2))
         (e3)
 - points to global environment

E1 (lambda (vars) ...):
 - vars: values of vars supplied into the procedure
 - points to global environment
(define u (e1)) (define v (e2)) (e3)

E2 (define u (e1)) (define v (e2)) (e3):
 - u: (e1)
 - v: (e2)
 - points to E1
(e3)

E3 (e1)
 - points to E2

E4 (e2)
 - points to E2


(lambda (vars)
   (let ((u '*unassigned*)
         (v '*unassigned*))
      (set! u (e1))
      (set! v (e2))
      (e3))):

Global environment:
other variables

lambda1:
 - variables: vars
 - body: (let ((u '*unassigned*)
               (v '*unassigned*))
            (set! u (e1))
            (set! v (e2))
            (e3)))
 - points to global environment

E1 (lambda (vars) ...):
 - vars: values of vars supplied into the procedure
 - points to global environment
(let ((u '*unassigned).....)

E2 (let ((u '*unassigned) ...):
 - points to E1
((lambda (u v) (set! u (e1)) (set! v (e2)) e3)) '*unassigned* '*unassigned)

E3 (lambda (u v) (set! u (e1)) (set! v (e2)) (e3)):
 - u: (e1) (originally '*unassigned*, but changed to (e1))
 - v: (e2) (originally '*unassigned*, but changed to (e2))
 - points to E2
(set! u (e1)) (set! v (e2)) (e3)

E4 (set! u (e1)):
 - points to E3
(e1)

E5 (set! v (e2))
 - points to E3
(e2)

There is an extra frame in the transformed program as the body of the transformed lambda
procedure contains a let-expression, which can be transformed into another lambda procedure.
The difference in environment structure can never make a difference in the behaviour of a
correct program as the new lambda procedure does not do anything different from the original
procedure.
|#

(define (simultaneous-no-extra proc-body)
  (define (extract-defines proc)
    (if (null? proc)
        (cons nil nil)
        (let ((first (first-exp proc))
              (rest (extract-defines (rest-exps proc))))
          (if (tagged-list? first 'define)
              (cons (cons first (car rest)) (cdr rest))
              (cons (car rest) (cons first (cdr rest)))))))
  (let ((split-defines (extract-defines proc-body)))
    (let ((split-defs (car split-defines))
          (split-body (cdr split-defines)))
      (if (null? split-defs)
          split-body
          (append split-defs split-body)))))

#|simultaneous-no-extra rearranges the body of the procedure such that the define-expressions
are at the start of the body.|#

#|
Exercise 4.18: Consider an alternative strategy for scanning out definitions that translates
the example in the text to

(lambda ⟨vars⟩
   (let ((u '*unassigned*) (v '*unassigned*))
      (let ((a ⟨e1⟩) (b ⟨e2⟩))
         (set! u a)
         (set! v b))
         ⟨e3⟩))

Here a and b are meant to represent new variable names, created by the interpreter, that do
not appear in the user’s program. Consider the solve procedure from Section 3.5.4:

(define (solve f y0 dt)
   (define y (integral (delay dy) y0 dt))
   (define dy (stream-map f y))
   y)

Will this procedure work if internal definitions are scanned out as shown in this exercise?
What if they are scanned out as shown in the text? Explain.
|#

#|
Using the alternative strategy, the procedure is converted to

(lambda (f y0 dt)
   (let ((y '*unassigned*) (dy '*unassigned*))
      (let ((a (integral (delay dy) y0 dt)) (b (stream-map f y)))
         (set! y a)
         (set! dy b)
         y)))

At the point when b is assigned its value i.e. (b (stream-map f y)), y has the value
'*unassigned* in the environment, thus e2 i.e. (stream-map f y) is not evaluated correctly.
They would work if they were scanned out as shown in the text, since the value of dy when
te value of y is evaluated in (set! y ...) does not matter due to (delay dy), while by the
time the value of dy is evaluated in (set! dy ...), y has been assigned a value.
|#

#|
Exercise 4.19: Ben Bitdiddle, Alyssa P. Hacker, and Eva Lu Ator are arguing about the
desired result of evaluating the expression

(let ((a 1))
   (define (f x)
      (define b (+ a x))
      (define a 5)
      (+ a b))
   (f 10))

Ben asserts that the result should be obtained using the sequential rule for define: b is
defined to be 11, then a is defined to be 5, so the result is 16. Alyssa objects that mutual
recursion requires the simultaneous scope rule for internal procedure definitions, and that
it is unreasonable to treat procedure names differently from other names. Thus, she argues
for the mechanism implemented in Exercise 4.16. This would lead to a being unassigned at the
time that the value for b is to be computed. Hence, in Alyssa’s view the procedure should
produce an error. Eva has a third opinion. She says that if the definitions of a and b are
truly meant to be simultaneous, then the value 5 for a should be used in evaluating b. Hence,
in Eva’s view a should be 5, b should be 15, and the result should be 20. Which (if any) of
these view-points do you support? Can you devise a way to implement internal definitions so
that they behave as Eva prefers?
|#

#|
Ben's viewpoint supports the imperative style. Alyssa supports a similar style, except that
her implementation would avoid subtle bugs caused by the ordering of statements.

Eva supports a functional approach, whereby the order of the statements do not matter as
they are both defined simultaneously. However, in reality it would be difficult to implement
simultaneous definiions as there is a lack of a general, efficient mechanism.
|#

#|
Exercise 4.20: Because internal definitions look sequential but are actually simultaneous,
some people prefer to avoid them entirely, and use the special form letrec instead. letrec
looks like let, so it is not surprising that the variables it binds are bound simultaneously
and have the same scope as each other. The sample procedure f above can be written without
internal definitions, but with exactly the same meaning, as

(define (f x)
   (letrec
      ((even? (lambda (n)
                 (if (= n 0) true (odd? (- n 1)))))
       (odd? (lambda (n)
                (if (= n 0) false (even? (- n 1))))))
      ⟨rest of body of f⟩))

letrec expressions, which have the form

(letrec ((⟨var1⟩ ⟨exp1⟩) . . . (⟨varn⟩ ⟨expn⟩))
   ⟨body⟩)

are a variation on let in which the expressions ⟨exp_k⟩ that provide the initial values for
the variables ⟨var_k⟩ are evaluated in an environment that includes all the letrec bindings.
This permits recursion in the bindings, such as the mutual recursion of even? and odd? in
the example above, or the evaluation of 10 factorial with

(letrec
   ((fact (lambda (n)
             (if (= n 1) 1 (* n (fact (- n 1)))))))
   (fact 10))

a. Implement letrec as a derived expression, by transforming a letrec expression into a let
expression as shown in the text above or in Exercise 4.18. That is, the letrec variables
should be created with a let and then be assigned their values with set!.

b. Louis Reasoner is confused by all this fuss about internal definitions. The way he sees
it, if you don’t like to use define inside a procedure, you can just use let. Illustrate
what is loose about his reasoning by drawing an environment diagram that shows the
environment in which the ⟨rest of body of f⟩ is evaluated during evaluation of the expression
(f 5), with f defined as in this exercise. Draw an environment diagram for the same
evaluation, but with let in place of letrec in the definition of f.
|#

(define (letrec-vardefs exp) (car exp))
(define (letrec-body exp) (cdr exp))

(define (letrec-vars exp) (map car (letrec-vardefs exp)))
(define (letrec-vals exp) (map cadr (letrec-vardefs exp)))

(define (letrec->let exp)
  (let ((vars (letrec-vars exp))
        (vals (letrec-vals exp)))
    (tag-exp 'let (cons (map (lambda (var) (list var ''*unassigned*)) vars)
                        (append (map (lambda (var val) (make-assignment var val)) vars vals)
                                (letrec-body exp))))))

#|
Assume the expression is as follows:

(define (f x)
  (letrec ((even?
            (lambda (n)
              (if (= n 0)
                  true
                  (odd? (- n 1)))))
           (odd?
            (lambda (n)
              (if (= n 0)
                  false
                  (even? (- n 1))))))
    (even? x)))

Global environment:
other variables

f
 - variable: x
 - body: ((lambda (even? odd?) (set! even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
                               (set! odd? (lambda (n) (if (= n 0) false (even? (- n 1)))))
                               (even? x)) '*unassigned* '*unassigned*)
 - points to global environment

E1 (f 5)
 - x: 5
 - points to global environment
((lambda (even? ....) '*unassigned '*unassigned*)

E2 (lambda (even? ....):
 - even? (originally '*unassigned*)
    - parameter: n
    - body: (if (= n 0) false (even? (- n 1)))
    - points to E2
 - odd? (originally '*unassigned*)
    - parameter: n
    - body: (if (= n 0) false (even? (- n 1)))
    - points to E2
 - points to E1
(even? x)


If let was used instead:

(define (f x)
  (let ((even?
           (lambda (n)
              (if (= n 0)
                  true
                  (odd? (- n 1)))))
        (odd?
           (lambda (n)
              (if (= n 0)
                  false
                  (even? (- n 1))))))
    (even? x)))

Global environment:
other variables

f
 - variable: x
 - body: ((lambda (even? odd?) (even? x))
          (lambda (n) (if (= n 0) true (odd? (- n 1))))
          (lambda (n) (if (= n 0) false (even? (- n 1)))))
 - points to global environment

E1 (f 5):
 - x: 5
((lambda (even? odd?) .....)

E2 (lambda (even? odd?) (even? x))
 - even?
    - parameter: n
    - (if (= n 0) true (odd? (- n 1)))
    - points to E1
 - odd?
    - parameter: n
    - (if (= n 0) false (even? (- n 1))))
    - points to E1
 - points to E1
(even? x)

letrec can be used as even? and odd? reference E2, as such they can look up the even? and
odd? variables defined in the frame.

On the other hand, let cannot be used as even? and odd? reference E1 as they are evaluated
in the body of f, hence they cannot look up the even? and odd? variables defined in E2.
|#

#|
Exercise 4.21: Amazingly, Louis’s intuition in Exercise 4.20 is correct. It is indeed
possible to specify recursive procedures without using letrec (or even define), although the
method for accomplishing this is much more subtle than Louis imagined. The following
expression computes 10 factorial by applying a recursive factorial procedure:

((lambda (n)
    ((lambda (fact) (fact fact n))
        (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
 10)

a. Check (by evaluating the expression) that this really does compute factorials. Devise an
analogous expression for computing Fibonacci numbers.

b. Consider the following procedure, which includes mutually recursive internal definitions:
(define (f x)
   (define (even? n)
      (if (= n 0) true (odd? (- n 1))))
   (define (odd? n)
      (if (= n 0) false (even? (- n 1))))
   (even? x))

Fill in the missing expressions to complete an alternative definition of f, which uses
neither internal definitions nor letrec:

(define (f x)
   ((lambda (even? odd?) (even? even? odd? x))
    (lambda (ev? od? n)
       (if (= n 0) true (od? ⟨??⟩ ⟨??⟩ ⟨??⟩)))
    (lambda (ev? od? n)
       (if (= n 0) false (ev? ⟨??⟩ ⟨??⟩ ⟨??⟩)))))
|#

(define recursive-fibonacci
  (lambda (n)
    ((lambda (fib) (fib fib n)) (lambda (fb k) (cond ((= k 1) 0)
                                               ((= k 2) 1)
                                               (else (+ (fb fb (- k 1)) (fb fb (- k 2)))))))))

(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

#|
Exercise 4.22: Extend the evaluator in this section to support the special form let. (See
Exercise 4.6.)
|#

(define (evaln exp env) ((analyse exp) env))

(define (analyse-self-evaluating exp)
  (lambda (env) exp))

(define (analyse-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyse-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyse-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyse (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyse-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyse (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyse-if exp)
  (let ((pproc (analyse (if-predicate exp)))
        (cproc (analyse (if-consequent exp)))
        (aproc (analyse (if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
                      (cproc env)
                      (aproc env)))))

(define (analyse-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyse-sequence (lambda-body exp))))
    (lambda (env) (tag-exp 'procedure (list vars bproc env)))))

(define (analyse-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyse exps)))
    (if (null? procs) (error "Empty sequence: ANALYSE"))
    (loop (car procs) (cdr procs))))

(define (analyse-application exp)
  (let ((fproc (analyse (operator exp)))
        (aprocs (map analyse (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION"
                proc))))

(define (analyse exp)
  (cond ((self-evaluating? exp) (analyse-self-evaluating exp))
        ((variable? exp) (analyse-variable exp))
        (else (let ((op (get 'analyse (operator exp))))
                (cond ((not (eq? op false)) (op (operands exp)))
                      ((application? exp)
                       (analyse-application exp))
                      (else (error "Unknown expression type: EVAL" exp)))))))

(define (install-analyse-syntax)
  (put 'analyse 'quote analyse-quoted)
  (put 'analyse 'set! analyse-assignment)
  (put 'analyse 'define analyse-definition)
  (put 'analyse 'if analyse-if)
  (put 'analyse 'lambda analyse-lambda)
  (put 'analyse 'begin analyse-sequence)
  (put 'analyse 'cond (lambda (exp) (analyse (cond->if exp))))
  (put 'analyse 'let (lambda (exp) (analyse (let->combination exp)))))

#|
Exercise 4.23: Alyssa P. Hacker doesn’t understand why analyze-sequence needs to be so
complicated. All the other analysis procedures are straightforward transformations of the
corresponding evaluation procedures (or eval clauses) in Section 4.1.1. She expected
analyze-sequence to look like this:

(define (analyze-sequence exps)
   (define (execute-sequence procs env)
      (cond ((null? (cdr procs))
            ((car procs) env))
            (else
               ((car procs) env)
               (execute-sequence (cdr procs) env))))
   (let ((procs (map analyze exps)))
      (if (null? procs)
          (error "Empty sequence: ANALYZE"))
          (lambda (env)
             (execute-sequence procs env))))

Eva Lu Ator explains to Alyssa that the version in the text does more of the work of
evaluating a sequence at analysis time. Alyssa’s sequence-execution procedure, rather than
having the calls to the individual execution procedures built in, loops through the
procedures in order to call them: In effect, although the individual expressions in the
sequence have been analyzed, the sequence itself has not been.

Compare the two versions of analyze-sequence. For example, consider the common case
(typical of procedure bodies) where the sequence has just one expression. What work will
the execution procedure produced by Alyssa’s program do? What about the execution procedure
produced by the program in the text above? How do the two versions compare for a sequence
with two expressions?
|#

#|
When the sequence has only one expression:

In Alyssa's program, the expression is analysed, with a lambda expression
(lambda (env) (execute-sequence analysed-procs env)) returned. During execution time, the
execution procedure takes in an environment and calls execute-sequence. Since there is only
one expression in the sequence, the analysed expression is called on the environment.

In the program in the text, the expression is analysed, with loop called on (car procs) and
(cdr procs). Since there is only one expression in the sequence, loop returns the analysed
expression. As such, during execution time, the execution procedure takes in an environment
and returns the analysed expression directly.

When the sequence has two expressions:

In Alyssa's program, the expressions are analysed, with a lambda expression
(lambda (env) (execute-sequence procs env)) returned. During execution time, the execution
procedure takes in an environment and calls execute-sequence. Since there is more than one
expression in the sequence, execute-sequence calls the first analysed expression on the
environment, then calls execute-sequence on (cdr procs) i.e. the list containing the second
analysed expression. This second call to execute-sequence then calls the second analysed
expression on the environment, since there is no other expressions in the list.

In the program in the text, the expressions are analysed, with loop called on (car procs)
and (cdr procs). Since there is more than one expression, there is another call to loop,
with arguments (sequentially first-proc (car rest-procs) and (cdr rest-procs). sequentially
takes two analysed procedure and returns a lambda expression that when called on an
environment, returns the first analysed expression called on an environment and then the
second analysed expression called on the same environment. Since there is no other
expressions after the second expression, loop returns (sequentially first-proc (car
rest-procs)) i.e. sequentially called on the first and second analysed expressions, which is
then returned as the result of analyse-sequence. During execution time, the execution
procedure takes in an environment and reutrns the analysed expressions directly.
|#

#|
Exercise 4.24: Design and carry out some experiments to compare the speed of the original
metacircular evaluator with the version in this section. Use your results to estimate the
fraction of time that is spent in analysis versus execution for various procedures.
|#

#|
Two possible experiments: looping through a large number or calculating large fibonacci
numbers.
|#

(install-eval-syntax)
(install-analyse-syntax)

(define the-global-environment (setup-environment))
(driver-loop)
