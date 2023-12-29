#lang racket


(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define the-empty-stream '())
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream head tail)
     (cons head (delay tail)))))
                             
(define (stream-null? stream) (null? stream))
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s)) (cons-stream (stream-car s) (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))
(define (display-stream s) (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

#|
Exercise 3.50: Complete the following definition, which generalizes stream-map to allow
procedures that take multiple arguments, analogous to map in Section 2.2.1, Footnote
12.

(define (stream-map proc . argstreams)
   (if (⟨??⟩ (car argstreams))
       the-empty-stream
       (⟨??⟩
        (apply proc (map ⟨??⟩ argstreams))
        (apply stream-map
               (cons proc (map ⟨??⟩ argstreams))))))
|#

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

#|
Exercise 3.51: In order to take a closer look at delayed evaluation, we will use the
following procedure, which simply returns its argument after printing it:
(define (show x)
   (display-line x)
   x)

What does the interpreter print in response to evaluating each expression in the following
sequence?

(define x
   (stream-map show
               (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)
|#

#|
(define x ...)
0

(stream-ref x 5):
1
2
3
4
5

(stream-ref x 7):
6
7
|#

#|
Exercise 3.52: Consider the sequence of expressions

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
   (stream-map accum
               (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z
   (stream-filter (lambda (x) (= (remainder x 5) 0))
                  seq))
(stream-ref y 7)
(display-stream z)

What is the value of sum aer each of the above expressions is evaluated? What is the
printed response to evaluating the stream-ref and display-stream expressions? Would these
responses differ if we had implemented (delay ⟨exp⟩) simply as (lambda () ⟨exp⟩) without
using the optimization provided by memo-proc? Explain.
|#

#|
(define sum 0)
sum: 0

(define seq ...)
sum: 1

(define y (stream-filter even? seq))
sum: 6

(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
sum: 10

(stream-ref y 7)
sum: 136
printed response: 136

(display-stream z)
sum: 210
printed response:
10
15
45
55
105
120
190
210

Yes, the responses will differ. Since there is no memoization involved, each call to
stream-cdr will recalculate sum, thus resulting in different values.

(define y ...)
sum: 6

(define z ...)
sum: 15

(stream-ref y 7)
sum: 162

(display-stream z)
15
180
230
305
|#

#|
Exercise 3.53: Without running the program, describe the elements of the stream defined by
(define s (cons-stream 1 (add-streams s s)))
|#

#|
The elements of the stream are: 1, 2, 4, 8, 16, 32...
|#

#|
Exercise 3.54: Define a procedure mul-streams, analogous to add-streams, that produces the
elementwise product of its two input streams. Use this together with the stream of integers
to complete the following definition of the stream whose nth element (counting from 0) is
n + 1 factorial:

(define factorials
   (cons-stream 1 (mul-streams ⟨??⟩ ⟨??⟩)))
|#

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams (stream-cdr factorials) factorials)))

#|
Exercise 3.55: Define a procedure partial-sums that takes as argument a stream S and
returns the stream whose elements are S_0, S_0+S_1, S_0+S_1+S_2, .... For example,
(partial-sums integers) should be the stream 1, 3, 6, 10, 15,....
|#

(define (partial-sums s) (add-streams s (cons-stream 0 (partial-sums s))))

#|
Exercise 3.56: A famous problem, first raised by R. Hamming, is to enumerate, in ascending
order with no repetitions, all positive integers with no prime factors other than 2, 3, or
5. One obvious way to do this is to simply test each integer in turn to see whether it has
any factors other than 2, 3, and 5. But this is very inefficient, since, as the integers
get larger, fewer and fewer of them fit the requirement. As an alternative, let us call the
required stream of numbers S and notice the following facts about it.
• S begins with 1.
• e elements of (scale-stream S 2) are also elements of S.
• e same is true for (scale-stream S 3) and (scale-stream S 5).
• ese are all the elements of S.

Now all we have to do is combine elements from these sources. For this we define a procedure
merge that combines two ordered streams into one ordered result stream, eliminating
repetitions:|#

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car) (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))

#|Then the required stream may be constructed with merge, as follows:

(define S (cons-stream 1 (merge ⟨??⟩ ⟨??⟩)))

Fill in the missing expressions in the places marked ⟨??⟩ above.
|#

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

#|
Exercise 3.57: How many additions are performed when we compute the nth Fibonacci number
using the definition of fibs based on the add-streams procedure? Show that the number of
additions would be exponentially greater if we had implemented (delay ⟨exp⟩) simply as
(lambda () ⟨exp⟩), without using the optimization provided by the memo-proc procedure
described in Section 3.5.1.
|#

#|
n - 2 additions are performed.

Without memoization, the number of additions to obtain the nth Fibonacci number is the sum
of the number of additions to obtain the (n - 1)th Fibonacci number and the number of
additions to obtain the (n - 2)th Fibonacci number. This has O(phi ^ n) complexity, thus
it is exponential.
|#

#|
Exercise 3.58: Give an interpretation of the stream computed by the following procedure:

(define (expand num den radix)
   (cons-stream
      (quotient (* num radix) den)
      (expand (remainder (* num radix) den) den radix)))

(quotient is a primitive that returns the integer quotient of two integers.) What are the
successive elements produced by (expand 1 7 10)? What is produced by (expand 3 8 10)?
|#

#|
It is the stream of decimals produced when num is divided by den in base radix.

(expand 1 7 10):
1, (expand 3 7 10)
4, (expand 2 7 10)
2, (expand 6 7 10)
8, (expand 4 7 10)
5, (expand 5 7 10)
7, (expand 1 7 10)

(expand 3 8 10):
3, (expand 6 8 10)
7, (expand 4 8 10)
5, (expand 0 8 10)
0, (expand 0 8 10)......
|#

#|
Exercise 3.59: In Section 2.5.3 we saw how to implement a polynomial arithmetic system
representing polynomials as lists of terms. In a similar way, we can work with power
series, such as

e^x = 1 + x + x^2 / 2 + x^3 / (3*2) + x^4 / (4*3*2) + ... ,
cos x = 1 - x^2 / 2 + x^4 / (4 * 3 * 2) - ...,
sin x = x - x^3 / (3 * 2) + x^5 / (5 * 4 * 3 * 2) - ...

represented as infinite streams. We will represent the series
a_0 + a_1x + a_2x^2 + a_3x^3 + ... as the stream whose elements are the coefficients a_0,
a_1, a_2, a_3, ....

a. The integral of the series a_0 + a_1x + a_2x^2 + a_3x^3 + ... is the series
c + a_0x + 1/2 a_1x^2 + 1/3 a_2x^3 + 1/4 a_3x^4 + ... , where c is any constant. Define a
procedure integrateseries that takes as input a stream a_0, a_1, a_2, . . . representing
a power series and returns the stream a_0, 1/2 a_1, 1/3 a_2, ... of coefficients of the
non-constant terms of the integral of the series. (Since the result has no constant term,
it doesn’t represent a power series; when we use integrate-series, we will cons on the
appropriate constant.)

b. e function x -> e^x is its own derivative. This implies that e^x and the integral of
e^x are the same series, except for the constant term, which is e^0 = 1. Accordingly, we
can generate the series for e^x as

(define exp-series
   (cons-stream 1 (integrate-series exp-series)))

Show how to generate the series for sine and cosine, starting from the facts that the
derivative of sine is cosine and the derivative of cosine is the negative of sine:
(define cosine-series (cons-stream 1 ⟨??⟩))
(define sine-series (cons-stream 0 ⟨??⟩))
|#

(define (integrate-series s)
  (mul-streams (stream-map (lambda (x) (/ 1 x)) integers) s))

(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

#|
Exercise 3.60: With power series represented as streams of coefficients as in Exercise 3.59,
adding series is implemented by add-streams. Complete the definition of the following
procedure for multiplying series:

(define (mul-series s1 s2)
   (cons-stream ⟨??⟩ (add-streams ⟨??⟩ ⟨??⟩)))

You can test your procedure by verifying that sin^2x + cos^2x = 1, using the series from
Exercise 3.59.
|#

(define (mul-series s1 s2) (cons-stream (* (stream-car s1) (stream-car s2)) (add-streams (scale-stream (stream-cdr s1) (stream-car s2)) (mul-series s1 (stream-cdr s2)))))

#|
Exercise 3.61: Let S be a power series (Exercise 3.59) whose constant term is 1. Suppose we
want to find the power series 1/S, that is, the series X such that SX = 1. Write
S = 1 + SR where SR is the part of S aer the constant term. en we can solve for X as
follows:

S * X = 1,
(1 + SR) * X = 1,
X + SR * X = 1,
X = 1 - SR * X.

In other words, X is the power series whose constant term is 1 and whose higher-order terms
are given by the negative of SR times X. Use this idea to write a procedure invert
unit-series that computes 1/S for a power series S with constant term 1. You will need to
use mul-series from Exercise 3.60.
|#

(define (invert-unit-series s) (cons-stream 1 (scale-stream (mul-series (stream-cdr s) (invert-unit-series s)) -1)))

#|
Exercise 3.62: Use the results of Exercise 3.60 and Exercise 3.61 to define a procedure
div-series that divides two power series. div-series should work for any two series,
provided that the denominator series begins with a nonzero constant term. (If the
denominator has a zero constant term, then div-series should signal an error.) Show how to
use div-series together with the result of Exercise 3.59 to generate the power series for
tangent.
|#

(define (div-series s1 s2) (let ((s2-car (stream-car s2)))
                             (if (= s2-car 0)
                                 (error "Denominator must begin with a non-zero constant term" (stream-car s2))
                                 (let ((r-s2-car (/ 1 s2-car)))
                                   (mul-series s1 (scale-stream (invert-unit-series (scale-stream s2 r-s2-car)) s2-car))))))

(define tangent-series (div-series sine-series cosine-series))

#|
Exercise 3.63: Louis Reasoner asks why the sqrt-stream procedure was not written in the
following more straightforward way, without the local variable guesses:

(define (sqrt-stream x)
   (cons-stream 1.0 (stream-map
                       (lambda (guess)
                          (sqrt-improve guess x))
                       (sqrt-stream x))))

Alyssa P. Hacker replies that this version of the procedure is considerably less efficient
because it performs redundant computation. Explain Alyssa’s answer. Would the two versions
still differ in efficiency if our implementation of delay used only (lambda () ⟨exp⟩)
without using the optimization provided by memo-proc (Section 3.5.1)?
|#

#|
With Louis' approach, each call to sqrt-stream results in two evaluations of (sqrt-stream x):
one as the argument to stream-cdr and one in the course of applying that procedure to its
argument. As such, each call results in a redundant evaluation. With each element of the
stream accessed, the number of calls to (sqrt-stream x) and (stream-cdr ....) grows
by O(n^2), since the interpreter does not realise that the second application of
stream-cdr to (sqrt-stream x) is the an application to the same object as the first application,
thus memoization is not used. On the other hand, in the original procedure, evaluation of the
(sqrt-stream x) argument passed to stream-cdr merely assigns the guesses variable to a value
in the environment, whereby after the first evaluation of the call, the second procedure
merely has to refer to the result of the previous evaluation, thus there is no n^2 growth in
redundant evaluations. If there was no use of memoization, both versions would not differ in
efficiency, as the original procedure would have to make two full evaluations of
(sqrt-stream x), resulting in the same redundant evaluations as Louis' approach.
|#

#|
Exercise 3.64: Write a procedure stream-limit that takes as arguments a stream and a number
(the tolerance). It should examine the stream until it finds two successive elements that
differ in absolute value by less than the tolerance, and return the second of the two
elements. Using this, we could compute square roots up to a given tolerance by

(define (sqrt x tolerance)
(stream-limit (sqrt-stream x) tolerance))
|#

(define (stream-limit s tolerance)
  (let ((next (stream-cdr s)))
    (if (< (abs (- (stream-car s) (stream-car next))) tolerance)
        (stream-car next)
        (stream-limit next tolerance))))

#|
Exercise 3.65: Use the series

ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...

to compute three sequences of approximations to the natural logarithm of 2, in the same way
we did above for pi. How rapidly do these sequences converge?
|#

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define (ln-2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-2-summands (+ n 1)))))

(define ln-2-stream (partial-sums (ln-2-summands 1)))

(define ln-2-stream-euler (euler-transform ln-2-stream))

(define ln-2-stream-accelerated (accelerated-sequence euler-transform ln-2-stream))

#|
ln-2-stream took the longest to converge, followed by ln-2-stream-euler, then
ln-2-stream-accelerated. The latter converged within 10 steps.
|#

#|
Exercise 3.66: Examine the stream (pairs integers integers). Can you make any general
comments about the order in which the pairs are placed into the stream? For example,
approximately how many pairs precede the pair (1, 100)? the pair (99, 100)? the pair
(100, 100)? (If you can make precise mathematical statements here, all the better. But
feel free to give more qualitative answers if you find yourself getting bogged down.)
|#

#|
For pair (s, t) in the stream, where s < t,

(s, t) is the (t - s) * 2 = (2^1 * (t - s))th element of the stream starting with (s, s)
       is the 1 + 2 * (2 * (t - s)) = (1 + 2^2 * (t - s))th element of the stream
       starting with (s - 1, s - 1)
       is the 1 + 2 * (1 + 2 * (2 * (t - s))) = (1 + 2^1 + 2^3 * (t - s))th element of the
       stream starting with (s - 2, s - 2)

For pair (s, s) in the stream,

(s, s) is the 1st element of the stream starting with (s, s)
       is the 1 + 2^1 = 3rd element of the stream starting with (s - 1, s - 1)
       is the 1 + 2 * (1 + 2^1) = 1 + 2^1 + 2^2 = 7th element of the stream starting with
       (s - 2, s - 2)

In general, for pair (s, t), it is the (1 + 2^1 + 2^2 + ... + 2^(s - 3) + 2^(s - 2) +
2^s * (t - s)) = (2^(s - 1) + 2^s * (t - s) - 1)th element of the stream starting with
(1, 1). For pair (s, s), it is the (1 + 2^1 + 2^2 + ... + 2^(s - 2) + 2^(s - 1)) =
(2^s - 1)th element of the stream starting with (1, 1)

(1, 100):

It is the (2^0 + 2^1 * 99 - 1) = 197th element of the series. 196 pairs precede it.

(99, 100):

It is the (2^98 + 2^100 - 1)th element of the stream. (2^98 + 2*100 - 2) pairs precede it.

(100, 100):

It is the (2^100 - 1)th element of the stream. (2^100 - 2) pairs precede it.
|#

#|
Exercise 3.67: Modify the pairs procedure so that (pairs integers integers) will produce
the stream of all pairs of integers (i, j) (without the condition i ≤ j). Hint: You will
need to mix in an additional stream.
|#

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1) (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave (stream-map (lambda (x) (list (stream-car s) x))
                            (stream-cdr t))
                (stream-map (lambda (x) (list x (stream-car t)))
                            (stream-cdr s)))
    (pairs (stream-cdr s) (stream-cdr t)))))

#|
Exercise 3.68: Louis Reasoner thinks that building a stream of pairs from three parts is
unnecessarily complicated. Instead of separating the pair (S0,T0) from the rest of the
pairs in the first row, he proposes to work with the whole first row, as follows:

(define (pairs s t)
   (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  t)
      (pairs (stream-cdr s) (stream-cdr t))))

Does this work? Consider what happens if we evaluate (pairs integers integers) using
Louis’s definition of pairs.
|#

#|
No, it does not work. The call to interleave requires its arguments be evaluated first,
thus (pairs (stream-cdr s) (stream-cdr t)) is called. This will result in infinite
recursion as (pairs (stream-cdr s) (stream-cdr t)) will call its interleave, which
also requires its arguments be evaluated first, hence the recursion continues.
|#

#|
Exercise 3.69: Write a procedure triples that takes three infinite streams, S, T, and U,
and produces the stream of triples (S_i, T_j, U_k) such that i ≤ j ≤ k. Use triples to
generate the stream of all Pythagorean triples of positive integers, i.e., the triples
(i, j, k) such that i ≤ j and i^2 + j^2 = k^2.
|#

(define (triples s t u)
  (let ((i (stream-car s))
        (j (stream-car t))
        (k (stream-car u)))
    (cons-stream (list i j k)
                 (interleave
                  (stream-map (lambda (pair) (cons i pair)) (stream-cdr (pairs t u)))
                  (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

(define pythagorean-triples
  (stream-filter (lambda (triple)
                   (= (+ (square (car triple))
                         (square (cadr triple)))
                      (square (caddr triple)))) (triples integers integers integers)))

#|
Exercise 3.70: It would be nice to be able to generate streams in which the pairs appear
in some useful order, rather than in the order that results from an ad hoc interleaving
process. We can use a technique similar to the merge procedure of Exercise 3.56, if we
define a way to say that one pair of integers is “less than” another. One way to do this
is to define a “weighting function” W(i, j) and stipulate that (i_1, j_1) is less than
(i_2, j_2) if W (i_1, j_1) < W (i_2, j_2). Write a procedure merge-weighted that is like
merge, except that mergeweighted takes an additional argument weight, which is a procedure
that computes the weight of a pair, and is used to determine the order in which elements
should appear in the resulting merged stream. Using this, generalize pairs to a procedure
weighted-pairs that takes two streams, together with a procedure that computes a weighting
function, and generates the stream of pairs, ordered according to weight. Use your
procedure to generate

a. the stream of all pairs of positive integers (i, j) with i ≤ j ordered according to the
sum i + j,

b. the stream of all pairs of positive integers (i, j) with i ≤ j, where neither i nor j
is divisible by 2, 3, or 5, and the pairs are ordered according to the sum 2i + 3j + 5ij.
|#

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (let ((s1-weight (weight s1car))
                 (s2-weight (weight s2car)))
             (cond ((< s1-weight s2-weight)
                    (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                   (else
                    (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))))))))

(define (weighted-pairs s t weight)
  (cons-stream (list (stream-car s) (stream-car t))
               (merge-weighted (stream-map (lambda (x) (list (stream-car s) x))
                                           (stream-cdr t))
                               (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
                               weight)))

(define pw-total (weighted-pairs integers integers (lambda (pair) (+ (car pair) (cadr pair)))))

(define no-2-3-5 (stream-filter
                    (lambda (x) (not
                                 (or (= (remainder x 2) 0)
                                     (= (remainder x 3) 0)
                                     (= (remainder x 5) 0))))
                    integers))
(define pw-div (weighted-pairs no-2-3-5 no-2-3-5 (lambda (pair) (let ((i (car pair))
                                                         (j (cadr pair)))
                                                     (+ (* 2 i)
                                                        (* 3 j)
                                                        (* 5 i j))))))

#|
Exercise 3.71:Numbers that can be expressed as the sum of two cubes in more than one way
are sometimes called Ramanujan numbers, in honor of the mathematician Srinivasa Ramanujan.
Ordered streams of pairs provide an elegant solution to the problem of computing these
numbers. To find a number that can be written as the sum of two cubes in two different ways,
we need only generate the stream of pairs of integers (i, j) weighted according to the sum
i^3 + j^3 (see Exercise 3.70), then search the stream for two consecutive pairs with the
same weight. Write a procedure to generate the Ramanujan numbers. e first such number is
1,729. What are the next five?
|#

(define (cube x) (* x x x))
(define (pair-cube-sum pair) (+ (cube (car pair)) (cube (cadr pair))))

(define cube-sort (weighted-pairs integers integers pair-cube-sum))

(define (ramanujan s)
  (define (iter prev-weight ss)
    (let ((next (stream-car ss)))
      (let ((next-sum (pair-cube-sum next)))
        (if (= prev-weight next-sum)
            (cons-stream prev-weight (iter prev-weight (stream-cdr (stream-cdr ss))))
            (iter next-sum (stream-cdr ss))))))
  (iter (pair-cube-sum (stream-car s)) (stream-cdr s)))

#|The next 5 Ramanujan numbers are 4104, 13832, 20683, 32832, and 39312.|#

#|
Exercise 3.72: In a similar way to Exercise 3.71 generate a stream of all numbers that can
be written as the sum of two squares in three different ways (showing how they can be so
written).
|#

(define (square-sum pair) (+ (square (car pair)) (square (cadr pair))))

(define square-sort (weighted-pairs integers integers square-sum))
                                    
(define (triple-square s)
  (define (iter ss)
    (let ((first (stream-car ss))
          (second (stream-car (stream-cdr ss)))
          (third (stream-car (stream-cdr (stream-cdr ss)))))
      (let ((first-weight (square-sum first))
            (second-weight (square-sum second))
            (third-weight (square-sum third)))
        (if (and (= first-weight second-weight) (= second-weight third-weight))
            (cons-stream (list first-weight first second third) (iter (stream-cdr (stream-cdr (stream-cdr ss)))))
            (iter (stream-cdr ss))))))
  (iter s))

#|
Exercise 3.73: We can model electrical circuits using streams to represent the values of
currents or voltages at a sequence of times. For instance, suppose we have an RC circuit
consisting of a resistor of resistance R and a capacitor of capacitance C in series. The
voltage response v of the circuit to an injected current i is determined by the formula in
Figure 3.33, whose structure is shown by the accompanying signal-flow diagram.

Write a procedure RC that models this circuit. RC should take as inputs the values of R, C,
and dt and should return a procedure that takes as inputs a stream representing the current
i and an initial value for the capacitor voltage v_0 and produces as output the stream of
voltages v. For example, you should be able to use RC to model an RC circuit with
R = 5 ohms, C = 1 farad, and a 0.5-second time step by evaluating (define RC1 (RC 5 1 0.5)).
This defines RC1 as a procedure that takes a stream representing the time sequence of
currents and an initial capacitor voltage and produces the output stream of voltages.
|#

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (v0 i) (add-streams (scale-stream (integral i v0 dt) (/ 1 C)) (scale-stream i R))))

#|
Exercise 3.74: Alyssa P. Hacker is designing a system to process signals coming from
physical sensors. One important feature she wishes to produce is a signal that describes the
zero crossings of the input signal. at is, the resulting signal should be +1 whenever the
input signal changes from negative to positive, -1 whenever the input signal changes from
positive to negative, and 0 otherwise. (Assume that the sign of a 0 input is positive.) For
example, a typical input signal with its associated zero-crossing signal would be

... 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4 ...
... 0 0  0  0  0   -1   0  0  0   0   1  0 0 ...

In Alyssa’s system, the signal from the sensor is represented as a stream sense-data and the
stream zero-crossings is the corresponding stream of zero crossings. Alyssa first writes a
procedure sign-change-detector that takes two values as arguments and compares the signs of
the values to produce an appropriate 0, 1, or - 1. She then constructs her zero-crossing
stream as follows:

(define (make-zero-crossings input-stream last-value)
   (cons-stream
      (sign-change-detector
         (stream-car input-stream)
         last-value)
      (make-zero-crossings
         (stream-cdr input-stream)
         (stream-car input-stream))))

(define zero-crossings
   (make-zero-crossings sense-data 0))

Alyssa’s boss, Eva Lu Ator, walks by and suggests that this program is approximately
equivalent to the following one, which uses the generalized version of stream-map from
Exercise 3.50:

(define zero-crossings
   (stream-map sign-change-detector
               sense-data
               ⟨expression⟩))

Complete the program by supplying the indicated ⟨expression⟩.
|#

(define (sign-change-detector next prev)
  (cond ((and (< prev 0) (>= next 0)) 1)
        ((and (>= prev 0) (< next 0)) -1)
        (else 0)))

(define (list->stream xs)
  (if (null? xs)
      the-empty-stream
      (cons-stream (car xs) (list->stream (cdr xs)))))

(define sense-data
  (list->stream
   (list 0 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

; list->stream and sense-data taken from https://wizardbook.wordpress.com/2010/12/23/exercise-3-74/

(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))

#|
Exercise 3.75: Unfortunately, Alyssa’s zero-crossing detector in Exercise 3.74 proves to be
insufficient, because the noisy signal from the sensor leads to spurious zero crossings.
Lem E. Tweakit, a hardware specialist, suggests that Alyssa smooth the signal to filter out
the noise before extracting the zero crossings. Alyssa takes his advice and decides to
extract the zero crossings from the signal constructed by averaging each value of the
sense data with the previous value. She explains the problem to her assistant, Louis
Reasoner, who aempts to implement the idea, altering Alyssa’s program as follows:

(define (make-zero-crossings input-stream last-value)
   (let ((avpt (/ (+ (stream-car input-stream)
                     last-value)
                  2)))
      (cons-stream
         (sign-change-detector avpt last-value)
         (make-zero-crossings
            (stream-cdr input-stream) avpt))))

This does not correctly implement Alyssa’s plan. Find the bug that Louis has installed and
fix it without changing the structure of the program. (Hint: You will need to increase the
number of arguments to make-zero-crossings.)
|#

(define (make-zero-crossings input-stream last-value last-avg)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream
     (sign-change-detector avpt last-avg)
     (make-zero-crossings (stream-cdr input-stream) (stream-car input-stream) avpt))))

#|
Exercise 3.76: Eva Lu Ator has a criticism of Louis’s approach in Exercise 3.75. The program
he wrote is not modular, because it intermixes the operation of smoothing with the
zero-crossing extraction. For example, the extractor should not have to be changed if Alyssa
finds a better way to condition her input signal. Help Louis by writing a procedure smooth
that takes a stream as input and produces a stream in which each element is the average of
two successive input stream elements. Then use smooth as a component to implement the
zero-crossing detector in a more modular style.
|#

(define (smooth s)
  (stream-map (lambda (x y) (/ (+ x y) 2)) s (cons-stream 0 s)))

(define (new-zero-crossings s)
  (let ((smoothed (smooth s)))
    (stream-map sign-change-detector smoothed (cons-stream 0 smoothed))))

#|
Exercise 3.77: The integral procedure used above was analogous to the “implicit” definition
of the infinite stream of integers in Section 3.5.2. Alternatively, we can give a definition
of integral that is more like integers-startingfrom (also in Section 3.5.2):

(define (integral integrand initial-value dt)
   (cons-stream
      initial-value
      (if (stream-null? integrand)
          the-empty-stream
          (integral (stream-cdr integrand)
                    (+ (* dt (stream-car integrand))
                       initial-value)
                    dt))))

When used in systems with loops, this procedure has the same problem as does our original
version of integral. Modify the procedure so that it expects the integrand as a delayed
argument and hence can be used in the solve procedure shown above.
|#

(define (delayed-integral delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (delayed-integral (delay (stream-cdr integrand))
                           (+ (* dt (stream-car integrand))
                              initial-value)
                           dt)))))

#|
Exercise 3.78: Consider the problem of designing a signalprocessing system to study the
homogeneous second-order linear differential equation

d^2y / dt^2 - a dy / dt - by = 0.

The output stream, modeling y, is generated by a network that contains a loop. This is
because the value of d^2y / dt^2 depends upon the values of y and dy/dt and both of these
are determined by integrating d^2y / dt^2. The diagram we would like to encode is shown in
Figure 3.35. Write a procedure solve-2nd that takes as arguments the constants a, b, and dt
and the initial values y0 and dy0 for y and dy/dt and generates the stream of successive
values of y.
|#

(define (solve-2nd a b dt y0 dy0)
  (define y (delayed-integral (delay dy) y0 dt))
  (define dy (delayed-integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
  y)

#|
Exercise 3.79: Generalize the solve-2nd procedure of Exercise 3.78 so that it can be used
to solve general secondorder differential equations d^2y / dt^2 = f(dy / dt, y).
|#

(define (solve-2nd-gen f dt dy0 y0)
  (define y (delayed-integral (delay dy) y0 dt))
  (define dy (delayed-integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

#|
Exercise 3.80: A series RLC circuit consists of a resistor, a capacitor, and an inductor
connected in series, as shown in Figure 3.36. If R, L, and C are the resistance, inductance,
and capacitance, then the relations between voltage (v) and current (i) for the three
components are described by the equations

v_R = i_R * R, v_L = L * di_L / dt , i_C = C * dv_C / dt,

and the circuit connections dictate the relations

i_R = i_L = -i_C , v_C = v_L + v_R.

Combining these equations shows that the state of the circuit (summarized by v_C , the
voltage across the capacitorand iL, the current in the inductor) is described by the pair
of differential equations
dv_C / dt = - i_L / C, di_L / dt = 1 / L * v_C - R / L * i_L.

The signal-flow diagram representing this system of differential equations is shown in
Figure 3.37. Write a procedure RLC that takes as arguments the parameters R, L, and C of
the circuit and the time increment dt. In a manner similar to that of the RC procedure of
Exercise 3.73, RLC should produce a procedure that takes the initial values of the state
variables, v_C0 and i_L0, and produces a pair (using cons) of the streams of states vC and
i_L. Using RLC, generate the pair of streams that models the behavior of a series RLC
circuit with R = 1 ohm, C = 0.2 farad, L = 1 henry, dt = 0.1 second, and initial values
i_L0 = 0 amps and v_C0 = 10 volts.
|#

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define iL (delayed-integral (delay diL) iL0 dt))
    (define vC (delayed-integral (delay dvC) vC0 dt))
    (define diL (add-streams (scale-stream vC (/ 1 L)) (scale-stream iL (/ (- R) L))))
    (define dvC (scale-stream iL (/ -1 C)))
    (cons vC iL)))

(define RLC1 ((RLC 1 1 0.2 0.1) 10 0))

#|
Exercise 3.81: Exercise 3.6 discussed generalizing the randomnumber generator to allow one
to reset the random-number sequence so as to produce repeatable sequences of “random”
numbers. Produce a stream formulation of this same generator that operates on an input
stream of requests to generate a new random number or to reset the sequence to a specified
value and that produces the desired stream of random numbers. Don’t use assignment in your
solution.
|#

(define (random-stream seed upper)
  (define (random-update x)
    (remainder (+ (* 29 x) 37) upper))
  (define (command req old-val)
    (cond ((and (pair? req) (eq? (car req) 'reset)) (cadr req))
          ((eq? req 'reset) (random-update seed))
          ((eq? req 'generate) (random-update old-val))
          (else (error "Invalid command - random-stream: " req))))
  (define (make-generator req-stream)
    (define s
      (cons-stream (command (stream-car req-stream) seed)
                   (stream-map command (stream-cdr req-stream) s)))
    s)
  make-generator)

#|
Exercise 3.82: Redo Exercise 3.5 on Monte Carlo integration in terms of streams. The stream
version of estimate-integral will not have an argument telling how many trials to perform.
Instead, it will produce a stream of estimates based on successively more trials.
|#

(define (monte-carlo experiment-stream passed failed total)
  (define (next p f t)
    (cons-stream
     (/ p t)
     (monte-carlo (stream-cdr experiment-stream) p f t)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed (+ total 1))
      (next passed (+ failed 1) (+ total 1))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral-stream P x1 x2 y1 y2)
  (let ((area (* (- y2 y1) (- x2 x1))))
    (define (rng-stream x1 x2 y1 y2)
      (cons-stream (cons (random-in-range x1 x2)
                         (random-in-range y1 y2))
                   (rng-stream x1 x2 y1 y2)))
    (define mc-stream (monte-carlo (stream-map P (rng-stream x1 x2 y1 y2)) 0 0 0))
    (scale-stream mc-stream area)))
       
  

