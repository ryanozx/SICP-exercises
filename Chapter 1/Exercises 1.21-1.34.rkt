#lang sicp
(define (square x) (* x x))
(define (divides? a b) (= (remainder b a) 0))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (prime? n) (and (= n (smallest-divisor n)) (> n 1)))
#|Exercise 1.21: Use the smallest-divisor procedure to find
the smallest divisor of each of the following numbers: 199,
1999, 19999.|#

(smallest-divisor 199) ; 199
(smallest-divisor 1999) ; 1999
(smallest-divisor 19999) ; 7

#|Exercise 1.22: Most Lisp implementations include a primitive
called runtime that returns an integer that specifies
the amount of time the system has been running (measured, for
example, in microseconds). The following timedprime-test procedure,
when called with an integer n, print n and checks to see if n is
prime. If n is prime, the procedure prints three asterisks
followed by the amount of time used in performing the test.|#

(define (report-prime elapsed-time)
       (display " *** ")
       (display elapsed-time))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

#|Using this procedure, write a procedure search-for-primes
that checks the primality of consecutive odd integers in a
specified range. Use your procedure to find the three smallest
primes larger than 1000; larger than 10,000; larger than
100,000; larger than 1,000,000. Note the time needed to test
each prime. Since the testing algorithm has order of growth
of Θ(sqrt n), you should expect that testing for primes around
10,000 should take about sqrt 10 times as long as testing for
primes around 1000. Do your timing data bear this out?
How well do the data for 100,000 and 1,000,000 support the
Θ(sqrt n) prediction? Is your result compatible with the notion
that programs on your machine run in time proportional to
the number of steps required for the computation?|#

(define (search-for-primes lower upper)
  (define (iter n) (cond ((<= n upper) (timed-prime-test n) (iter (+ n 2)))))
  (iter (if (odd? lower) lower (+ lower 1))))

(search-for-primes 1000 1019) ; 5us + 5us + 5us = 15us
(search-for-primes 10000 10037) ; 13us + 12us + 13us = 38us
(search-for-primes 100000 100043) ; 33us + 33us + 34us = 100us
(search-for-primes 1000000 1000037) ; 100us + 101us + 100us = 301us

#|As the values for the lower and upper bounds increase by a factor
of 10, the time taken for each timed-prime-test increases by approximately
a factor of 3, which is approximately the square root of 10, thus the timing
data supports the Θ (sqrt n) prediction. It is compatible with the notion
that programs on the machine run in time proportional to the number of steps
required for the computation.|#

#|Exercise 1.23: The smallest-divisor procedure shown at
the start of this section does lots of needless testing: After it
checks to see if the number is divisible by 2 there is no point
in checking to see if it is divisible by any larger even numbers. This
suggests that the values used for test-divisor should not be 2, 3, 4, 5, 6,
..., but rather 2, 3, 5, 7, 9, ...

To implement this change, define a procedure next that returns 3 if its input
is equal to 2 and otherwise returns its input plus 2. Modify the smallest-divisor
procedure to use (next test-divisor) instead of (+ test-divisor 1).
With timed-prime-test incorporating this modified version of smallest-divisor,
run the test for each of the 12 primes found in Exercise 1.22. Since this modification
halves the number of test steps, you should expect it to run about
twice as fast. Is this expectation confirmed? If not, what is
the observed ratio of the speeds of the two algorithms, and
how do you explain the fact that it is different from 2?
|#

(define (next x) (if (= x 2) 3 (+ x 2)))
(define (new-find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (new-find-divisor n (next test-divisor)))))
(define (new-smallest-divisor n) (new-find-divisor n 2))
(define (new-prime? n) (and (= n (new-smallest-divisor n)) (> n 1)))
(define (new-start-prime-test n start-time)
  (if (new-prime? n)
      (report-prime (- (runtime) start-time))))
(define (new-timed-prime-test n)
  (newline)
  (display n)
  (new-start-prime-test n (runtime)))
(new-timed-prime-test 1009) ; 5us
(new-timed-prime-test 1013) ; 5us
(new-timed-prime-test 1019) ; 5us
(new-timed-prime-test 10007) ; 9us
(new-timed-prime-test 10009) ; 9us
(new-timed-prime-test 10037) ; 9us
(new-timed-prime-test 100003) ; 25us
(new-timed-prime-test 100019) ; 25us
(new-timed-prime-test 100043) ; 26us
(new-timed-prime-test 1000003) ; 88us
(new-timed-prime-test 1000033) ; 79us
(new-timed-prime-test 1000037) ; 80us

#|The results do not follow expectation. The observed ratio of the speeds
of the two algorithms vary between 3:4 to 1:1. The halving in the number
of test steps is partially offset by having to evaluate the if-expression
for each divisor.|#

#|Exercise 1.24: Modify the timed-prime-test procedure of
Exercise 1.22 to use fast-prime? (the Fermat method), and
test each of the 12 primes you found in that exercise. Since
the Fermat test has Θ(logn) growth, how would you expect
the time to test primes near 1,000,000 to compare with the
time needed to test primes near 1000? Do your data bear
this out? Can you explain any discrepancy you find?|#

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- (if (< n 4294967087) n 4294967087) 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
(define (fast-start-prime-test n start-time)
  (if (fast-prime? n 3)
      (report-prime (- (runtime) start-time))))
(define (fast-timed-prime-test n)
  (newline)
  (display n)
  (fast-start-prime-test n (runtime)))

(fast-timed-prime-test 1009) ; 10us
(fast-timed-prime-test 1013) ; 8us
(fast-timed-prime-test 1019) ; 7us
(fast-timed-prime-test 10007) ; 10us
(fast-timed-prime-test 10009) ; 9us
(fast-timed-prime-test 10037) ; 9us
(fast-timed-prime-test 100003) ; 10us
(fast-timed-prime-test 100019) ; 11us
(fast-timed-prime-test 100043) ; 11us
(fast-timed-prime-test 1000003) ; 11us
(fast-timed-prime-test 1000033) ; 11us
(fast-timed-prime-test 1000037) ; 12us
(fast-timed-prime-test 1000000007) ; 17us
(fast-timed-prime-test 1000000009) ; 16us
(fast-timed-prime-test 1000000021) ; 16us
(fast-timed-prime-test 10000000019) ; 29us
(fast-timed-prime-test 10000000033) ; 27us
(fast-timed-prime-test 10000000061) ; 29us
(fast-timed-prime-test 100000000003) ; 33us
(fast-timed-prime-test 100000000019) ; 33us
(fast-timed-prime-test 100000000057) ; 34us
(fast-timed-prime-test 1000000000039) ; 34us
(fast-timed-prime-test 1000000000061) ; 34us
(fast-timed-prime-test 1000000000063) ; 35us

#|A prime with approximately twice the number of digits as
another prime takes approximately 2-3 times as long, thus
supporting the Θ(logn) growth hypothesis. The variance in time
taken can be explained in part by the fact that the base chosen
is random.|#

#|Exercise 1.25: Alyssa P. Hacker complains that we went to
a lot of extra work in writing expmod. After all, she says,
since we already know how to compute exponentials, we
could have simply written
(define (expmod base exp m)
        (remainder (fast-expt base exp) m))
Is she correct? Would this procedure serve as well for our
fast prime tester? Explain.|#

#|This procedure would not be as ideal as the fast prime tester.
In the fast prime tester, each call to expmod is followed by a
call to remainder, thus the output of each evaluation of expmod
is a value that is smaller than m/the prime tested. On the other
hand, this procedure calculates the product of the exponentiation
first, which is likely to be a very big number that will likely result
in an integer overflow. As such, not only is this procedure going to
be much slower, it will possibly be inaccurate.|#

#|Exercise 1.26: Louis Reasoner is having great difficulty doing
Exercise 1.24. His fast-prime? test seems to run more
slowly than his prime? test. Louis calls his friend Eva Lu
Ator over to help. When they examine Louis’s code, they
find that he has rewritten the expmod procedure to use an
explicit multiplication, rather than calling square:
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (* (expmod base (/ exp 2) m) (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))
“I don’t see what difference that could make,” says Louis.
“I do.” says Eva. “By writing the procedure like that, you
have transformed the Θ(logn) process into a Θ(n) process.”
Explain.|#

#|By rewritting expmod to use explicit multiplication, each argument in the
multiplication expression has to be evaluated separately, thus generating
a tree recursion that grows exponentially with the depth of the tree (log n),
resulting in linear execution time N.|#

#|Exercise 1.27: Demonstrate that the Carmichael numbers
listed in Footnote 1.47 really do fool the Fermat test. That is,
write a procedure that takes an integer n and tests whether
a^n is congruent to a modulo n for every a < n, and try your
procedure on the given Carmichael numbers.|#

(define (carmichael n)
  (define (fooled? a)
    (cond ((>= a n) true)
          (else (if (= (expmod a n n) a)
                    (fooled? (+ a 1))
                    false))))
  (newline)
  (display n)
  (if (fooled? 2)
      (display " has passed the Fermat test")
      (display " has failed the Fermat test")))

(carmichael 561)
(carmichael 1105)
(carmichael 1729)
(carmichael 2465)
(carmichael 2821)
(carmichael 6601)

#|Exercise 1.28: One variant of the Fermat test that cannot
be fooled is called the Miller-Rabin test (Miller 1976; Rabin
1980). This starts from an alternate form of Fermat’s Little
Theorem, which states that if n is a prime number and a is
any positive integer less thann, then a raised to the (n-1)-st
power is congruent to 1 modulo n. To test the primality of a
number n by the Miller-Rabin test, we pick a random number
a < n and raise a to the (n -1)-st power modulo n using
the expmod procedure. However, whenever we perform the
squaring step in expmod, we check to see if we have discovered
a “nontrivial square root of 1 modulo n,” that is, a number
not equal to 1 or n-1 whose square is equal to 1 modulo
n. It is possible to prove that if such a nontrivial square root
of 1 exists, then n is not prime. It is also possible to prove
that if n is an odd number that is not prime, then, for at least
half the numbers a < n, computing a^(n-1) in this way will
reveal a nontrivial square root of 1 modulo n. (This is why
the Miller-Rabin test cannot be fooled.) Modify the expmod
procedure to signal if it discovers a nontrivial square root
of 1, and use this to implement the Miller-Rabin test with
a procedure analogous to fermat-test. Check your procedure
by testing various known primes and non-primes.
Hint: One convenient way to make expmod signal is to have
it return 0|#

(define (mr_expmod base exp m)
  (define (non_trivial_square x)
    (if (and (= (remainder (square x) m) 1) (not (= x 1)) (not (= x (- m 1))))
        0
        (square x)))
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (non_trivial_square (mr_expmod base (/ exp 2) m)) m))
        (else (remainder (* base (mr_expmod base (- exp 1) m)) m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (mr_expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- (if (< n 4294967087) n 4294967087) 1)))))

(miller-rabin-test 59) ; prime
(miller-rabin-test 337) ; prime
(miller-rabin-test 561) ; non-prime
(miller-rabin-test 1105) ; non-prime
(miller-rabin-test 1729) ; non-prime
(miller-rabin-test 2465) ; non-prime
(miller-rabin-test 2821) ; non-prime
(miller-rabin-test 6601) ; non-prime

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))
(define (inc n) (+ n 1))
#|Exercise 1.29: Simpson’s Rule is a more accurate method
of numerical integration than the method illustrated above.
Using Simpson’s Rule, the integral of a function f between
a and b is approximated as
h/3 (y_0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + · · · + 2y_n-2 + 4y_n-1 + y_n),
where h = (b - a)/n, for some even integer n, and y_k = f (a + kh).
(Increasing n increases the accuracy of the approximation.)
Define a procedure that takes as arguments f , a, b, and n
and returns the value of the integral, computed using Simpson’s
Rule. Use your procedure to integrate cube between 0 and 1
(with n = 100 and n = 1000), and compare the results to those
of the integral procedure shown above|#

(define (simpsons f a b n)
  (define h (/ (- b a) n))
  (define (y-term k) (* (f (+ a (* k h))) (cond ((or (= k 0) (= k n)) 1)
                                                ((even? k) 2)
                                                (else 4))))
  (* (/ h 3) (sum y-term 0 inc n)))
(define (cube x) (* x x x))
(simpsons cube 0 1 100)
(simpsons cube 0 1 1000)

#|Simpson's Rule gave a more accurate integral than the integral function.|#

#|Exercise 1.30: The sum procedure above generates a linear
recursion. The procedure can be rewritten so that the sum
is performed iteratively. Show how to do this by filling in
the missing expressions in the following definition:
(define (sum term a next b)
  (define (iter a result)
    (if ⟨??⟩
        ⟨??⟩
        (iter ⟨??⟩ ⟨??⟩)))
  (iter ⟨??⟩ ⟨??⟩))
|#

(define (iter_sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

#|Exercise 1.31:
a. The sum procedure is only the simplest of a vast number
of similar abstractions that can be captured as higher-
order procedures. Write an analogous procedure called
product that returns the product of the values of a
function at points over a given range. Show how to define
factorial in terms of product. Also use product
to compute approximations to π using the formula
π/4 = (2 * 4 * 4 * 6 * 6 * 8 * ...) / (3 * 3 * 5 * 5 * 7 * 7 * ...)

b. If your product procedure generates a recursive process,
write one that generates an iterative process. If
it generates an iterative process, write one that generates
a recursive process.|#

(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a) (product-rec term (next a) next b))))
(define (identity x) x)
(define (factorial n) (product-rec identity 1 inc n))
(define (pi-approx n)
  (define (frac_term k) (if (even? k)
                            (/ (+ k 2) (+ k 1))
                            (/ (+ k 1) (+ k 2))))
  (* 4 (product-rec frac_term 1 inc n)))
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

#|Exercise 1.32:
a. Show that sum and product (Exercise 1.31) are both
special cases of a still more general notion called accumulate
that combines a collection of terms, using some general accumulation
function:
(accumulate combiner null-value term a next b)

accumulate takes as arguments the same term and
range specifications as sum and product, together with
a combiner procedure (of two arguments) that specifies
how the current term is to be combined with the
accumulation of the preceding terms and a null-value
that specifies what base value to use when the terms
run out. Write accumulate and show how sum and
product can both be defined as simple calls to accumulate.

b. If your accumulate procedure generates a recursive
process, write one that generates an iterative process.
If it generates an iterative process, write one that generates a recursive process
|#

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-rec combiner null-value term (next a) next b))))
(define (acc-sum-rec term a next b)
  (accumulate-rec + 0 term a next b))
(define (acc-product-rec term a next b)
  (accumulate-rec * 1 term a next b))
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))
(define (acc-sum-iter term a next b)
  (accumulate-iter + 0 term a next b))
(define (acc-product-iter term a next b)
  (accumulate-iter * 1 term a next b))

#|Exercise 1.33: You can obtain an even more general version
of accumulate (Exercise 1.32) by introducing the notion of a
filter on the terms to be combined. That is, combine
only those terms derived from values in the range that satisfy
a specified condition. The resulting filtered-accumulate
abstraction takes the same arguments as accumulate, together
with an additional predicate of one argument that
specifies the filter. Write filtered-accumulate as a procedure.
Show how to express the following using filteredaccumulate:
a. the sum of the squares of the prime numbers in the
interval a to b (assuming that you have a prime? predicate already written)

b. the product of all the positive integers less than n that
are relatively prime to n (i.e., all positive integersi < n
such that GCD(i, n) = 1).|#

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (if (filter a) (combiner (term a) result) result))))
  (iter a null-value))
(define (square-sum-prime a b)
  (filtered-accumulate + 0 square a inc b prime?))
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (prod-prime n)
  (define (relative-prime? x) (= (gcd n x) 1))
  (filtered-accumulate * 1 identity 1 inc n relative-prime?))

#|Exercise 1.34: Suppose we define the procedure
(define (f g) (g 2))
Then we have
(f square)
4
(f (lambda (z) (* z (+ z 1))))
6
What happens if we (perversely) ask the interpreter to evaluate
the combination (f f)? Explain.
|#

#| It will result in an error as the first invocation of f will
try to call f on 2. The second invocation of f will try to apply
2 on 2, or (2 2), resulting in an error as 2 is not a procedure.|#