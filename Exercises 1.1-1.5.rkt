#lang sicp
#| Exercise 1.1: Below is a sequence of expressions. What is
the result printed by the interpreter in response to each
expression? Assume that the sequence is to be evaluated in
the order in which it is presented. |#
10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6
(define a 3) ; a (note: DrRacket will not print any value for this expression)
(define b (+ a 1)) ; b
(+ a b (* a b)) ; 19
(= a b) ; #f
(if (and (> b a) (< b (* a b)))
    b
    a) ; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; 16
(+ 2 (if (> b a) b a)) ; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ; 16

#| Exercise 1.2: Translate the following expression into prefix
form: (5 + 4 + (2 - (3 - (6 + 4/5)))) / (3 * (6 - 2) * (2 - 7))

|#
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

#| Exercise 1.3: Define a procedure that takes three numbers
as arguments and returns the sum of the squares of the two
larger numbers. |#
(define (square x) (* x x))
(define (sum-of-squares a b c) (cond ((and (< a b) (< a c)) (+ (square b) (square c))) ; finds the smallest number and adds the squares of the other two numbers
                                     ((and (< b a) (< b c)) (+ (square a) (square c)))
                                     ((and (< c a) (< c b)) (+ (square a) (square b)))
                                     ((and (> a b) (> a c)) (+ (square a) (square b))) ; covers the case where a > b = c, cases where there is one smaller number and the two larger numbers are equal are already covered
                                     ((and (> b a) (> b c)) (+ (square b) (square c))) ; b > a = c
                                     ((and (> c a) (> c b)) (+ (square c) (square a))) ; c > a = b
                                     (else (+ (square a) (square b))))) ; covers the case where all 3 numbers are equal

#| Exercise 1.4: Observe that our model of evaluation allows
for combinations whose operators are compound expressions.
Use this observation to describe the behavior of the
following procedure: |#
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

#| The operator (if (> b 0) + -) along with the operands a and b are first evaluated.

Since the operator involves the if-conditional, the predicate (> b 0) is first evaluated.
The value of b from the argument of a-plus-abs-b is substituted into the predicate. If b is
greater than 0, the predicate returns #t, and the consequent (+) is evaluated. Since '+'
is a primitive procedure, the if-conditional returns '+'. On the other hand, if b is less than
or equal to 0, the predicate returns #f, and the alternative consequent (-) is evaluated.
Since '-' is a primitive procedure, the if-conditional returns '-'. Therefore, the if-conditional
is evaluated and returns either '+' or '-' as the procedure, depending on the value of b.

The operands a and b are then evaluated, with their values substituted with the values of the
arguments of a-plus-abs-b. The procedure (either '+' or '-') is then applied to the operands, with
the result returned as the result of a-plus-abs-b. |#

#| Exercise 1.5: Ben Bitdiddle has invented a test to determine
whether the interpreter he is faced with is using applicative-
order evaluation or normal-order evaluation. He defines the
following two procedures: |#
(define (p) (p))
(define (test x y)
(if (= x 0) 0 y))
#| Then he evaluates the expression |#
(test 0 (p))
#| What behavior will Ben observe with an interpreter that
uses applicative-order evaluation? What behavior will he
observe with an interpreter that uses normal-order evaluation?
Explain your answer. (Assume that the evaluation rule for the special form
if is the same whether the interpreter is using normal or applicative order: The
predicate expression is evaluated first, and the result determines
whether to evaluate the consequent or the alternative expression.) |#

#| An interpreter that uses applicative-order evaluation will get stuck in
an infinite loop. Since applicative-order evaluation involves
evaluating the operator and operand first before applying the resulting procedure, the
interpreter will get stuck evaluating the operand (p), which is defined as itself.

On the other hand, an interpreter that uses normal-order evaluation will return 0.
Since normal-order evaluation involves substituting operand expressions for parameters
until only primitive expressions remain, (test 0 (p)) is first expanded to
(if (= 0 0) 0 (p)). Given the evaluation rule for the special form if as described in
the question, the predicate (= 0 0) is first evaluated, which returns the value #t.
As such, the consequent 0 is evaluated, skipping the alternative consequent (p) entirely,
and thus (p) is not evaluated at all, avoiding the infinite-loop. |#

