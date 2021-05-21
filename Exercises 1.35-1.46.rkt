#lang sicp

(define (square x) (* x x))

(define (average p1 p2) (/ (+ p1 p2) 2))

(define (close-enough? x y tolerance-val) (< (abs (- x y)) tolerance-val))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point 0.001)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next 0.00001)
          next
          (try next))))
  (try first-guess))

#|Exercise 1.35: Show that the golden ratio φ (Section 1.2.2)
is a fixed point of the transformation x -> 1 + 1/x, and
use this fact to compute φ by means of the fixed-point
procedure.
|#

#|Multiplying both sides of the equation x = 1 + 1/x by x,
we get x^2 = x + 1, which is the definition of phi.

Rearranging the equation to get a quadratic equation:
x^2 - x - 1 = 0

Solving for x: x = (1 +- sqrt ((-1)^2 - 4*1*(-1))) / 2
                 = (1 +- sqrt(1 + 4)) / 2
                 = (1 +- sqrt 5) / 2

Discarding the negative root, we get (1 + sqrt 5) / 2,
which is equal to phi.|#

(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

#|Exercise 1.36: Modify fixed-point so that it prints the
sequence of approximations it generates, using the newline
and display primitives shown in Exercise 1.22. Then find
a solution to x^x = 1000 by finding a fixed point of
x -> log(1000)/ log(x). (Use Scheme’s primitive log procedure,
which computes natural logarithms.) Compare the number
of steps this takes with and without average damping. (Note
that you cannot start fixed-point with a guess of 1, as this
would cause division by log(1) = 0.)|#

(define (print-fixed-point f first-guess)
  (define (try guess count)
    (let ((next (f guess)))
      (begin
        (display next)
        (newline)
        (if (close-enough? guess next 0.00001)
            (begin
              (display "Steps taken: ")
              (display count)
              (newline))
            (try next (+ count 1))))))
  (try first-guess 1))

(print-fixed-point (lambda (x) (/ (log 1000) (log x))) 10) ; 33 steps without damping
(print-fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 10) ; 10 steps without damping

#|Exercise 1.37: a. An infinite continued fraction is an
expression of the form
f = N1 / (D1 + N2 / (D2 + N3 / (D3 + ...)))

As an example, one can show that the infinite continued
fraction expansion with the N_i and the D_i all
equal to 1 produces 1/φ, where φ is the golden ratio
(described in Section 1.2.2). One way to approximate
an infinite continued fraction is to truncate the expansion
after a given number of terms. Such a truncation —
a so-called k-term finite continued fraction — has the form
N1 / (D1 + N2 / (... + Nk / Dk))

Suppose that n and d are procedures of one argument
(the term index i) that return the N_i and D_i of the
terms of the continued fraction. Define a procedure
cont-frac such that evaluating (cont-frac n d k)
computes the value of the k-term finite continued fraction.
Check your procedure by approximating 1/φ using
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)
for successive values of k. How large must you make
k in order to get an approximation that is accurate to
4 decimal places?

b. If your cont-frac procedure generates a recursive process,
write one that generates an iterative process. If it generates
an iterative process, write one that generates a recursive process.|#

(define (cont-frac-rec n d k)
  (if (> k 1)
      (/ (n k) (+ (d k) (cont-frac-rec n d (- k 1))))
      (/ (n k) (d k))))

(define phi-iter
  (let ((goal (/ 1 phi)))
    (define (try k)
      (let ((calc (cont-frac-rec (lambda (i) 1.0) (lambda (i) 1.0) k)))
        (if (close-enough? calc goal 0.0001)
            k
            (try (+ k 1)))))
    (try 1))) ; k = 10

phi-iter

(define (cont-frac-iter n d k)
  (define (helper k acc)
    (if (< k 1)
        acc
        (helper (- k 1) (/ (n k) (+ (d k) acc)))))
  (helper k 0))

#|Exercise 1.38: In 1737, the Swiss mathematician Leonhard
Euler published a memoir De Fractionibus Continuis, which
included a continued fraction expansion for e - 2, where
e is the base of the natural logarithms. In this fraction, the
N_i are all 1, and the D_i are successively 1, 2, 1, 1, 4, 1, 1,
6, 1, 1, 8, . . .. Write a program that uses your cont-frac
procedure from Exercise 1.37 to approximate e, based on
Euler’s expansion.|#

(define (e-approx k)
    (+ 2.0 (cont-frac-rec (lambda (i) 1) (lambda (i) (if (= (remainder i 3) 2)
                                                         (/ (+ i 1) 1.5)
                                                         1)) k)))


#|Exercise 1.39: A continued fraction representation of the
tangent function was published in 1770 by the German mathematician
J.H. Lambert:

tan x = x / (1 - x^2 / (3 - x^2 / 5 - . . .,

where x is in radians. Define a procedure (tan-cf x k) that
computes an approximation to the tangent function based
on Lambert’s formula. k specifies the number of terms to
compute, as in Exercise 1.37.|#

(define (tan-cf x k)
  (let ((x-sq (- (square x))))
    (cont-frac-rec (lambda (i) (if (= i 1)
                                   x
                                   x-sq)) (lambda (i) (- (* i 2) 1)) k)))


(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
(define (cube x) (* x x x))

#|Exercise 1.40: Define a procedure cubic that can be used
together with the newtons-method procedure in expressions
of the form

(newtons-method (cubic a b c) 1)

to approximate zeros of the cubic x^3 + ax^2 + b^x + c.
|#

(define (cubic a b c) (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

#|Exercise 1.41: Define a procedure double that takes a procedure
of one argument as argument and returns a procedure that applies
the original procedure twice. For example, if inc is a procedure
that adds 1 to its argument, then (double inc) should be a procedure
that adds 2. What value is returned by (((double (double double)) inc) 5)|#

(define (double proc) (lambda (x) (proc (proc x))))
(define (inc x) (+ x 1))
(((double (double double)) inc) 5) ; 16

#|Exercise 1.42: Let f and g be two one-argument functions.
The composition f after g is defined to be the function
x -> f (g(x)). Define a procedure compose that implements
composition. For example, if inc is a procedure that adds 1 to
its argument,
((compose square inc) 6)
49|#

(define (compose f g) (lambda (x) (f (g x))))

#|Exercise 1.43: If f is a numerical function and n is a positive integer,
then we can form the nth repeated application
of f , which is defined to be the function whose value at
x is f (f (. . . (f (x)) . . . )). For example, if f is the function
x -> x + 1, then the nth repeated application of f is the
function x -> x + n. If f is the operation of squaring a number,
then the nth repeated application of f is the function
that raises its argument to the 2n-th power. Write a procedure that
takes as inputs a procedure that computes f and a
positive integer n and returns the procedure that computes
the nth repeated application of f . Your procedure should be
able to be used as follows:
((repeated square 2) 5)
625
Hint: You may find it convenient to use compose from Exercise 1.42.
|#

(define (repeated f n) (if (> n 1)
                           (compose f (repeated f (- n 1)))
                           f))

#|Exercise 1.44: The idea of smoothing a function is an important
concept in signal processing. If f is a function and
dx is some small number, then the smoothed version of f is
the function whose value at a point x is the average of
f (x - dx), f (x), and f (x+dx). Write a procedure smooth that takes
as input a procedure that computes f and returns a procedure
that computes the smoothed f . It is sometimes valuable to
repeatedly smooth a function (that is, smooth the smoothed function,
and so on) to obtain the n-fold smoothed function. Show how to
generate the n-fold smoothed function of any given function using
smooth and repeated from Exercise 1.43.|#

(define (smooth f) (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))
(define (n-fold-smoothed f n) ((repeated smooth n) f))

#|Exercise 1.45: We saw in Section 1.3.3 that attempting to
compute square roots by naively finding a fixed point of
y -> x/y does not converge, and that this can be fixed by
average damping. The same method works for finding cube
roots as fixed points of the average-damped y -> x/y^2.
Unfortunately, the process does not work for fourth roots — a
single average damp is not enough to make a fixed-point
search for y -> x/y^3 converge. On the other hand, if we
average damp twice (i.e., use the average damp of the average
damp of y -> x/y^3) the fixed-point search does converge. Do some experiments
to determine how many average damps are required to compute nth roots as a
fixedpoint search based upon repeated average damping of y -> x/y^(n-1).
Use this to implement a simple procedure for computing nth roots
using fixed-point, average-damp, and the repeated procedure of
Exercise 1.43. Assume that any arithmetic operations you need
are available as primitives.|#

(define (pow b p)
  (define (even? x) (= (remainder x 2) 0))
  (define (iter res a n)
    (if (= n 0)
        res
        (if (even? n)
            (iter res (square a) (/ n 2))
            (iter (* res a) a (- n 1)))))
  (iter 1 b p))
(define (average-damp f) (lambda (x) (average x (f x))))
(define (nth-root-damp x n) (fixed-point ((repeated average-damp (/ (log n) (log 2))) (lambda (y) (/ x (pow y (- n 1))))) 1.0))

#|Exercise 1.46: Several of the numerical methods described
in this chapter are instances of an extremely general computational
strategy known as iterative improvement. Iterative improvement says
that, to compute something, we start with an initial guess
for the answer, test if the guess is good enough, and otherwise
improve the guess and continue the process using the improved
guess as the new guess. Write a procedure iterative-improve
that takes two procedures as arguments: a method for telling
whether a guess is good enough and a method for improving a guess.
iterativeimprove should return as its value a procedure that takes a
guess as argument and keeps improving the guess until it is
good enough. Rewrite the sqrt procedure of Section 1.1.7
and the fixed-point procedure of Section 1.3.3 in terms of
iterative-improve.|#

(define (iterative-improve check improve) (lambda (guess) (if (check guess)
                                                              guess
                                                              ((iterative-improve check improve) (improve guess)))))

(define (new-sqrt x) ((iterative-improve (lambda (guess) (close-enough? (square guess) x 0.000001)) (lambda (guess) (average guess (/ x guess)))) 1.0))
(define (new-fixed-point f first-guess) ((iterative-improve (lambda (guess) (close-enough? guess (f guess) 0.000001)) f) first-guess)) 
 