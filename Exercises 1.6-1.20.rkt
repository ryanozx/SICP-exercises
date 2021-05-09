#lang sicp
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (improve guess x) (average guess (/ x guess)))
(define (good-enough? guess x) (< (abs (- (square guess) x)) 0.001))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))

#| Exercise 1.6: Alyssa P. Hacker doesn’t see why if needs to
be provided as a special form. “Why can’t I just define it as
an ordinary procedure in terms of cond?” she asks. Alyssa’s
friend Eva Lu Ator claims this can indeed be done, and she
defines a new version of if:
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
Eva demonstrates the program for Alyssa:
(new-if (= 2 3) 0 5) ; 5
(new-if (= 1 1) 0 5) ; 0
Delighted, Alyssa uses new-if to rewrite the square-root
program:
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))
What happens when Alyssa attempts to use this to compute
square roots? Explain. |#

#| The program will be stuck in an infinite loop. Since the
interpreter uses applicative-order evaluation, all 3 arguments of
new-if are evaluated first before it is applied to the arguments.
As such, the last argument of new-if, sqrt-iter, is also evaluated first.
This results in sqrt-iter calling itself recursively, thus resulting in an
infinite-loop.
|#

#| Exercise 1.7: The good-enough? test used in computing
square roots will not be very effective for finding the square
roots of very small numbers. Also, in real computers, arithmetic
operations are almost always performed with limited precision.
This makes our test inadequate for very large
numbers. Explain these statements, with examples showing
how the test fails for small and large numbers. An alternative
strategy for implementing good-enough? is to watch
how guess changes from one iteration to the next and to
stop when the change is a very small fraction of the guess.
Design a square-root procedure that uses this kind of end
test. Does this work better for small and large numbers? |#

#| For very small numbers, the tolerance of good-enough? (0.001)
is significant compared to the square root of the number. As such,
the result returned by sqrt may have a large percentage error compared
to the actual value. For example: |#
(sqrt 0.001) ; Returns 0.04124542607499115, actual value 0.0316227766, percentage error of 30.4%
(sqrt 0.0001) ; Returns 0.03230844833048122, actual value 0.01, percentage error of 223%
#| On the other hand, for very large numbers, the program may not terminate as sqrt-iter
 is unable to obtain a value that is within the tolerance of the actual value, since there is a
 limit to the precision of the value obtained due to representation of large integers and
floating-point numbers. For example: (sqrt 10000000000000) will
result in an infinite loop as (improve guess x) is unable to return a more precise value
that will fall within the tolerance of 0.001. |#
(define (improved-good-enough? guess x) (= guess (improve guess x))) ; by checking if the guess is equal to (improve guess x), the expression returns a value to the maximum precision possible as determined by the machine
#| This solves both the problem of low precision when finding the square root of small numbers by not hardcoding an absolute tolerance,
while also solving the problem of infinite-loop when finding the square root of large numbers by
 terminating when there is no further improvement possible at the current precision|#
(define (improved-sqrt-iter guess x) (if (improved-good-enough? guess x)
                                         guess
                                         (improved-sqrt-iter (improve guess x) x)))
(define (improved-sqrt x) (improved-sqrt-iter 1.0 x))

#| Exercise 1.8: Newton’s method for cube roots is based on
the fact that if y is an approximation to the cube root of x,
then a better approximation is given by the value ((x/y^2) + 2y) / 3

Use this formula to implement a cube-root procedure analogous
to the square-root procedure. |#

(define (improve-cube-root guess x) (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (good-enough-cube-root? guess x) (= guess (improve-cube-root guess x)))
(define (cube-root-iter guess x) (if (good-enough-cube-root? guess x)
                                     guess
                                     (cube-root-iter (improve-cube-root guess x) x)))
(define (cube-root x) (cube-root-iter 1.0 x))

#| Exercise 1.9: Each of the following two procedures defines
a method for adding two positive integers in terms of the
procedures inc, which increments its argument by 1, and
dec, which decrements its argument by 1.
(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))
(define (+ a b)
  (if (= a 0) b (+ (dec a) (inc b))))
Using the substitution model, illustrate the process generated
by each procedure in evaluating (+ 4 5). Are these
processes iterative or recursive? |#

#| For the first procedure:
(+ 4 5) ; consequent is evaluated as a is not equal to 0
(inc (+ (dec 4) 5)) ; start of expansion, (dec 4) is evaluated first
(inc (+ 3 5)) ; (+ 3 5) is evaluated, and so on
(inc (inc (+ (dec 3) 5)))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ (dec 2) 5))))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ (dec 1) 5)))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5)))) ; (+ 0 5) is evaluated; since a = 0, it returns b = 5; start of contraction
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

Therefore, the process generated by the first procedure is recursive.

For the second procedure:

(+ 4 5)
(+ (dec 4) (inc 5)) ; consequent is evaluated as a is not equal to 0
(+ 3 6) ; (dec 4) and (inc 5) are evaluated
(+ (dec 3) (inc 6))
(+ 2 7)
(+ (dec 2) (inc 7))
(+ 1 8)
(+ (dec 1) (inc 8))
(+ 0 9)
9 ; (+ 0 9) is evaluated; since a = 0, b = 9 is returned as the value of the expression

Therefore, the process generated by the second procedure is iterative.|#

#| Exercise 1.10: The following procedure computes a mathematical
function called Ackermann’s function.
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))
What are the values of the following expressions?
(A 1 10)
(A 2 4)
(A 3 3)
Consider the following procedures, where A is the procedure defined above:
(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))
Give concise mathematical definitions for the functions computed
by the procedures f, g, and h for positive integer values of n.
For example, (k n) computes 5n^2.|#

#|
(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
...
(A 0 (A 0 ... (A 1 1) ...))
(A 0 (A 0 ... (A 0 2) ...))
(A 0 (A 0 ... (* 2 2) ...))
...
1024 ; 2^10

Hence (A 1 10) evaluates to 1024.

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2))) ; as seen in the previous example, (A 1 x) evaluates to 2^x
...
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
...
(A 1 16)
...
65536 ; 2^16

Hence (A 2 4) evaluates to 65536.

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
...
(A 2 4) ; as seen in the previous example, (A 2 2) evaluates to 4 (compare 3rd line and 2nd last line of previous example)
...
65536 ; (A 2 4) evaluates to 65536 as seen in the previous example

Hence (A 3 3) evaluates to 65536.

(f n) evaluates to 2n.
(g n) evaluates to 2^n for n > 0.
(h n) evaluates to 2^2^...2 (raised n - 1 times) for n > 1
|#

#| Exercise 1.11: A function f is defined by the rule that
f (n) = { n if n < 3
          f (n - 1) + 2f (n - 2) + 3f (n - 3) if n ≥ 3.
Write a procedure that computes f by means of a recursive
process. Write a procedure that computes f by means of an
iterative process.|#

(define (recursive_f n) (if (n < 3)
                            n
                            ((recursive_f (- n 1)) + (* 2 (recursive_f (- n 2))) + (* 3 (recursive_f (- n 3))))))
(define (iterative_f_helper a b c iter) (if (= iter 0)
                                         a
                                         (iterative_f_helper (+ a (* 2 b) (* 3 c)) a b (- iter 1))))
(define (iterative_f n) (if (n < 3)
                            n
                            (iterative_f_helper 2 1 0 (- n 2))))


#| Exercise 1.12: The following paern of numbers is called
Pascal’s triangle. The numbers at the edge of the triangle are all 1,
and each number inside the triangle is the sum of the two numbers
above it. Write a procedure that computes elements of Pascal’s triangle
by means of a recursive process. |#

(define (pascal col row) (cond ((or (< col 1) (< row 1) (> col row)) 0)
                               ((or (= col 1) (= col row)) 1)
                               (else (+ (pascal (- col 1) (- row 1)) (pascal col (- row 1))))))

#| Exercise 1.13: Prove that Fib(n) is the closest integer to
φ^n / sqrt 5, where φ = (1 + sqrt 5)/2. Hint: Let ψ = (1 -
sqrt 5)/2. Use induction and the definition of the Fibonacci numbers
(see Section 1.2.2) to prove that Fib(n) = (φ^n - ψ^n) / sqrt 5.|#

#| The Fibonacci sequence can be defined as a sequence in which
 the value of an element is the sum of the values of the preceding
two elements in the sequence i.e. x_n = x_(n-1) + x_(n-2).

Assume Fib(n) = (φ^n - ψ^n) / sqrt 5.

Check that the formula holds for the first 2 elements in the Fibonacci
sequence:

Fib (1) = (φ - ψ) / sqrt 5
        = ((1 + sqrt 5) / 2 - (1 - sqrt 5) / 2) / sqrt 5
        = sqrt 5 / sqrt 5
        = 1 (correct)
Fib (2) = (φ^2 - ψ^2) / sqrt 5
        = ((1 + sqrt 5)^2 - (1 - sqrt 5)^2) / sqrt 5
        = ((6 + 2 sqrt 5) / 4 - (6 - 2 sqrt 5) / 4) / sqrt 5
        = sqrt 5 / sqrt 5
        = 1 (correct)

Using the definition of the Fibonacci sequence,
Fib (n) = Fib (n - 1) + Fib (n - 2)
        = (φ^(n-1) - ψ^(n-1)) / sqrt 5 + (φ^(n-2) - ψ^(n-2)) / sqrt 5
        = (φ^(n-1) - ψ^(n-1) + φ^(n-2) - ψ^(n-2)) / sqrt 5
        = (φ^(n-2) * (1+φ) - ψ^(n-2) * (1+ψ)) / sqrt 5
        = (φ^(n-2) * (1 + (1 + sqrt 5) / 2) - ψ^(n-2) * (1 + (1 - sqrt 5) / 2)) / sqrt 5
        = (φ^(n-2) * ((3 + sqrt 5) / 2) - ψ^(n-2) * ((3 - sqrt 5) / 2)) / sqrt 5
        = (φ^(n-2) * ((6 + 2sqrt 5) / 4) - ψ^(n-2) * ((6 - 2sqrt 5) / 4)) / sqrt 5
        = (φ^(n-2) * φ^2 - ψ^(n-2) * ψ^2) / sqrt 5
        = (φ^n - ψ^n) / sqrt 5

By induction, since Fib (1) and Fib (2) satisfy the equation Fib(n) = (φ^n - ψ^n) / sqrt 5,
all subsequent values in the Fibonacci sequence also satisfy the equation, hence
Fib(n) = (φ^n - ψ^n) / sqrt 5 is proven.

The difference between φ^n / sqrt 5 and Fib n is ψ^n / sqrt 5. To prove that φ^n / sqrt 5
is the closest integer to Fib n, we will show that the absolute value of ψ^n / sqrt 5 is less
than 0.5 i.e. - sqrt 5 / 2 < ψ^n < sqrt 5 / 2

ψ = (1 - sqrt 5) / 2
  = 1 / 2 - sqrt 5 / 2

- sqrt 5 / 2 < 1 / 2 - sqrt 5 / 2 < sqrt 5 / 2

Since ψ = - 0.618..., |ψ^n| is a strictly decreasing value as n approaches infinity, thus |ψ^n|
is less than 0.5 for all positive values of n. Therefore, Fib(n) is the closest integer to
φ^n / sqrt 5 (shown).
|#

#|Exercise 1.14: Draw the tree illustrating the process generated by the count-change procedure of Section 1.2.2 in
making change for 11 cents. What are the orders of growth
of the space and number of steps used by this process as
the amount to be changed increases?|#

#|
cc 11 5 -> cc -39 4 = 0
        -> cc 11 4 -> cc -14 3 = 0
                   -> cc 11 3 -> cc 1 3 -> cc -9 3 = 0
                                        -> cc 1 2 -> cc -4 2 = 0
                                                  -> cc 1 1 -> cc 1 0 = 0
                                                            -> cc 0 1 = 1
                              -> cc 11 2 -> cc 6 2 -> cc 1 2 -> cc -4 2 = 0
                                                             -> cc 1 1 -> cc 0 1 = 1
                                                                       -> cc 1 0 = 0
                                                   -> cc 6 1 (refer to (1)) 
                                         -> cc 11 1 -> cc 11 0 = 0
                                                    -> cc 10 1 -> cc 10 0 = 0
                                                               -> cc 9 1 -> cc 9 0
                                                                         -> cc 8 1 (refer to (2))
(1):
cc 6 1 -> cc 6 0 = 0
       -> cc 5 1 -> cc 5 0 = 0
                 -> cc 4 1 -> cc 4 0 = 0
                           -> cc 3 1 -> cc 3 0 = 0
                                     -> cc 2 1 -> cc 2 0 = 0
                                               -> cc 1 1 -> cc 1 0 = 0
                                                         -> cc 0 1 = 1
(2):
cc 8 1 -> cc 8 0 = 0
       -> cc 7 1 -> cc 7 0 = 0
                 -> cc 6 1 (refer to (1))

The order of growth of the space is Θ(n).
The order of growth of the number of steps is Θ(n^5).
|#

#|Exercise 1.15: The sine of an angle (specified in radians)
can be computed by making use of the approximation sin x ≈ x
if x is sufficiently small, and the trigonometric identity
sin x = 3 sin x/3 - 4 sin^3 x/3
to reduce the size of the argument of sin. (For purposes of
this exercise an angle is considered “sufficiently small” if its
magnitude is not greater than 0.1 radians.) These ideas are
incorporated in the following procedures:|#
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))
#|a. How many times is the procedure p applied when (sine
12.15) is evaluated?
b. What is the order of growth in space and number of
steps (as a function of a) used by the process generated
by the sine procedure when (sine a) is evaluated?|#

#|a. 5 times.
b. The order of growth in space and number of steps is Θ(lg a).|#


#|Exercise 1.16: Design a procedure that evolves an iterative
exponentiation process that uses successive squaring and uses
a logarithmic number of steps, as does fast-expt.
(Hint: Using the observation that (b^(n/2))^2 = (b^2)^(n/2),
keep, along with the exponent n and the base b, an additional
state variable a, and define the state transformation in such
a way that the product a(b^n) is unchanged from state to state.
At the beginning of the process a is taken to be 1, and the
answer is given by the value of a at the end of the process.
In general, the technique of defining an invariant quantity
that remains unchanged from state to state is a powerful
way to think about the design of iterative algorithms.)|#
(define (even? n)
  (= (remainder n 2) 0))
(define (iterative-fast-expt-helper n b a) (cond ((= n 0) a)
                                                ((even? n) (iterative-fast-expt-helper (/ n 2) (square b) a))
                                                (else (iterative-fast-expt-helper (- n 1) b (* a b)))))
(define (iterative-fast-expt n b) (iterative-fast-expt-helper n b 1))

#|Exercise 1.17: The exponentiation algorithms in this section
are based on performing exponentiation by means of
repeated multiplication. In a similar way, one can perform
integer multiplication by means of repeated addition. The
following multiplication procedure (in which it is assumed
that our language can only add, not multiply) is analogous
to the expt procedure:|#
(define (new_mult a b)
  (if (= b 0)
      0
      (+ a (new_mult a (- b 1)))))
#|This algorithm takes a number of steps that is linear in b.
Now suppose we include, together with addition, operations double,
which doubles an integer, and halve, which divides an (even) integer by 2.
Using these, design a multiplication procedure analogous to fast-expt that uses a
logarithmic number of steps.|#

(define (double x) (* 2 x))
(define (halve x) (/ x 2))
(define (fast-mult-recursive a b)
  (cond ((or (= b 0) (= a 0)) 0)
        ((= b 1) a)
        ((= b -1) (- 0 a))
        ((= a 1) b)
        ((= a -1) (- 0 b))
        ((even? b) (double (fast-mult-recursive a (halve b))))
        ((> b 1) (+ a (fast-mult-recursive a (- b 1))))
        (else (- (fast-mult-recursive a (+ b 1)) a))))

#|Exercise 1.18: Using the results of Exercise 1.16 and Exercise 1.17,
devise a procedure that generates an iterative process for multiplying
two integers in terms of adding, doubling, and halving and uses a
logarithmic number of steps.|#

(define (fast-mult-iterative-helper a b acc)
  (cond ((or (= a 0) (= b 0)) 0)
        ((= b 1) acc)
        ((= b -1) (- 0 acc))
        ((= a 1) b)
        ((= a -1) (- 0 b))
        ((even? b) (fast-mult-iterative-helper a (halve b) (double acc)))
        (else (fast-mult-iterative-helper a ((if (> b 1) - +) b 1) (+ acc a)))))
(define (fast-mult-iterative a b) (fast-mult-iterative-helper a b a))

#|Exercise 1.19: There is a clever algorithm for computing
the Fibonacci numbers in a logarithmic number of steps.
Recall the transformation of the state variables a and b in
the fib-iter process of Section 1.2.2: a ← a + b and b ← a.
Call this transformation T , and observe that applying T
over and over again n times, starting with 1 and 0, produces
the pair Fib(n + 1) and Fib(n). In other words, the Fibonacci
numbers are produced by applying T^n, the nth power of the
transformation T , starting with the pair (1, 0). Now consider
T to be the special case of p = 0 and q = 1 in a family of
transformations T_pq , where T_pq transforms the pair (a, b)
according to a ← bq + aq + ap and b ← bp + aq. Show
that if we apply such a transformation T_pq twice, the effect
is the same as using a single transformation T_p′q′ of the
same form, and compute p′ and q′ in terms of p and q. This
gives us an explicit way to square these transformations,
and thus we can compute T^n using successive squaring, as
in the fast-expt procedure. Put this all together to complete
the following procedure, which runs in a logarithmic
number of steps:

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   <??> ; compute p'
                   <??>; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))|#
#|T_pq (T_pq (a, b)) = T_pq (bq + aq + ap, bp + aq)
                     = (q * (bp + aq) + q * (bq + aq + ap) + p * (bq + aq + ap),
                        p * (bp + aq) + q * (bq + aq + ap))
                     = (bpq + aq^2 + bq^2 + aq^2 + apq + bpq + apq + ap^2,
                        bp^2 + apq + bq^2 + aq^2 + apq)

bp^2 + apq + bq^2 + aq^2 + apq can be rearranged as b * (p^2 + q^2) + a * (2pq + q^2) = bp' + aq'
If we use p' = (p^2 + q^2) and q = (2pq + q^2), bpq + aq^2 + bq^2 + aq^2 + apq + bpq + apq + ap^2
can be rearranged as:

2bpq + 2aq^2 + bq^2 + 2apq + ap^2 = b * (2pq + q^2) + a * (2q^2 + 2pq + p^2)
                                  = b * (2pq + q^2) + a * (2pq + q^2) + a * (p^2 + q^2)
                                  = bq' + aq' + ap'

Therefore,
p' = (p^2 + q^2)
q' = (2pq + q^2)
|#

(define (fib n) (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count) (fib-iter a b (+ (square p) (square q)) (+ (* 2 p q) (square q)) (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- count 1)))))

#|Exercise 1.20: The process that a procedure generates is
of course dependent on the rules used by the interpreter.
As an example, consider the iterative gcd procedure given
above. Suppose we were to interpret this procedure using
normal-order evaluation, as discussed in Section 1.1.5. (The
normal-order-evaluation rule for if is described in Exercise
1.5.) Using the substitution method (for normal order), illustrate
the process generated in evaluating (gcd 206 40) and indicate the
remainder operations that are actually performed. How many
remainder operations are actually performed in the normal-order
evaluation of (gcd 206 40)? In the applicative-order evaluation?|#

#|
Substitution method:
(gcd 206 40)
(if (= 40 0) ...)
(gcd 40 (remainder 206 40))
(if (= (remainder 206 40)) ...)
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
(if (= (remainder 40 (remainder 206 40)) 0) ...)
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) ...)
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
(if (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) ...)
(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))

18 remainder operations are performed in the normal-order evaluation.
4 remainder operations are performed in the applicative-order evaluation.
|#