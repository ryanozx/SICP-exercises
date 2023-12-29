#lang sicp

(define (square x) (* x x))

#|Exercise 2.1: Define a better version of make-rat that handles both
positive and negative arguments. make-rat should normalize the sign
so that if the rational number is positive, both the numerator and
denominator are positive, and if the rational number is negative,
only the numerator is negative|#
(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (gcd n d))))
    (cons (/ n g) (/ d g))))

#|Exercise 2.2: Consider the problem of representing line
segments in a plane. Each segment is represented as a pair
of points: a starting point and an ending point. Define a
constructor make-segment and selectors start-segment and
end-segment that define the representation of segments in
terms of points. Furthermore, a point can be represented
as a pair of numbers: the x coordinate and the y coordinate.
Accordingly, specify a constructor make-point and selectors
x-point and y-point that define this representation. Finally,
using your selectors and constructors, define a procedure
 midpoint-segment that takes a line segment as argument and
returns its midpoint (the point whose coordinates are the
average of the coordinates of the endpoints). To try your
procedures, you’ll need a way to print points:|#


(define (make-segment point-a point-b) (cons point-a point-b))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))
(define (average a b) (/ (+ a b) 2))
(define (midpoint-segment segment) (make-point (average (x-point (start-segment segment)) (x-point (end-segment segment)))
                                               (average (y-point (start-segment segment)) (y-point (end-segment segment)))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ".")
  (display (y-point p))
  (display ")"))

#|Exercise 2.3: Implement a representation for rectangles in
a plane. (Hint: You may want to make use of Exercise 2.2.) In
terms of your constructors and selectors, create procedures
that compute the perimeter and the area of a given rectangle.
Now implement a different representation for rectangles. Can
you design your system with suitable abstraction
barriers, so that the same perimeter and area procedures
will work using either representation?|#

(define (make-rect-1 length height origin angle) (cons (cons length height) (cons origin angle)))
(define (height1 rect) (cdr (car rect)))
(define (length1 rect) (car (car rect)))
(define (perimeter1 rect) (* (+ (height1 rect) (length1 rect)) 2))
(define (area1 rect) (* (height1 rect) (length1 rect)))

(define (segment-length segment) (sqrt (+ (square (- (x-point (end-segment segment)) (x-point (start-segment segment))))
                                          (square (- (y-point (end-segment segment)) (y-point (start-segment segment)))))))
(define (sub-vector v1 v2) (make-point (- (x-point v2) (x-point v1)) (- (y-point v2) (y-point v1))))
(define (dot-product v1 v2) (+ (* (x-point v1) (x-point v2)) (* (y-point v1) (y-point v2))))
(define (make-rect-2 p1 p2 p3) (if (= (dot-product (sub-vector p1 p2) (sub-vector p1 p3)) 0)
                                  (cons p1 (cons p2 p3))
                                  (error "Points specified do not describe a rectangle!")))
(define (p1 rect) (car rect))
(define (p2 rect) (car (cdr rect)))
(define (p3 rect) (cdr (cdr rect)))
(define (height2 rect) (segment-length (make-segment (p1 rect) (p3 rect))))
(define (length2 rect) (segment-length (make-segment (p1 rect) (p2 rect))))
(define (perimeter2 rect) (* (+ (height2 rect) (length2 rect)) 2))
(define (area2 rect) (* (height2 rect) (length2 rect)))

#|Exercise 2.4: Here is an alternative procedural representation
of pairs. For this representation, verify that (car (cons x y))
yields x for any objects x and y.

(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))

What is the corresponding definition of cdr? (Hint: To verify
that this works, make use of the substitution model of
Section 1.1.5.)|#

(define (new-cons x y) (lambda (m) (m x y)))
(define (new-car z) (z (lambda (p q) p)))

#|Calling new-car on z, where z is the procedure returned by
new-cons, gives the following:
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) x y) -> x|#

(define (new-cdr z) (z (lambda (p q) q)))

#|Exercise 2.5: Show that we can represent pairs of nonnegative
integers using only numbers and arithmetic operations if we
represent the pair a and b as the integer that is the product
2^a*3^b. Give the corresponding definitions of the
procedures cons, car, and cdr|#

(define (divisible? x divisor) (= (remainder x divisor) 0))
(define (pow base exp)
  (define (iter acc b e)
    (cond ((= e 0) acc)
          ((divisible? e 2) (iter acc (square b) (/ e 2)))
          (else (iter (* b acc) b (- e 1)))))
  (iter 1 base exp))
(define (intpair-iter-div acc quo div)
    (if (divisible? quo div)
        (intpair-iter-div (+ acc 1) (/ quo div) div)
        acc))

(define (intpair a b) (* (pow 2 a) (pow 3 b)))
(define (intcar pair) (intpair-iter-div 0 pair 2))
(define (intcdr pair) (intpair-iter-div 0 pair 3))

#|Exercise 2.6: In case representing pairs as procedures wasn’t
mind-boggling enough, consider that, in a language that
can manipulate procedures, we can get by without numbers
(at least insofar as nonnegative integers are concerned) by
implementing 0 and the operation of adding 1 as|#

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))

#|This representation is known as Church numerals, after its
inventor, Alonzo Church, the logician who invented the λcalculus.
Define one and two directly (not in terms of zero and add1).
(Hint: Use substitution to evaluate (add-1 zero)). Give
a direct definition of the addition procedure + (not in terms
of repeated application of add-1)|#

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (church-add a b) (lambda (f) (lambda (x) ((a f) ((b f) x)))))

#|Exercise 2.7: Alyssa’s program is incomplete because she
has not specified the implementation of the interval abstraction.
Here is a definition of the interval constructor:|#
(define (make-interval a b) (cons a b))
#|Define selectors upper-bound and lower-bound to complete
the implementation|#

(define (lower-bound interval) (min (car interval) (cdr interval)))
(define (upper-bound interval) (max (car interval) (cdr interval)))


(define (add-interval a b) (make-interval (+ (lower-bound a) (lower-bound b))
                                          (+ (upper-bound a) (upper-bound b))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))

#|Exercise 2.8: Using reasoning analogous to Alyssa’s, describe
how the difference of two intervals may be computed. Define a
corresponding subtraction procedure, called sub-interval.|#

(define (sub-interval x y) (let ((diff1 (- (upper-bound y) (lower-bound x)))
                                 (diff2 (- (upper-bound x) (lower-bound y))))
                             (make-interval (min diff1 diff2) (max diff1 diff2))))

#|Exercise 2.9: The width of an interval is half of the difference
between its upper and lower bounds. The width is a measure of the
uncertainty of the number specified by the interval. For some
arithmetic operations the width of the result of combining
two intervals is a function only of the widths of the argument
intervals, whereas for others the width of the combination is
not a function of the widths of the argument intervals. Show
that the width of the sum (or difference) of two intervals
is a function only of the widths of the intervals being added
(or subtracted). Give examples to show that this is not true
for multiplication or division.
|#

(define (width interval) (/ (- (upper-bound interval) (lower-bound interval)) 2))

#|Let the lower and upper bounds be described as (r - e) and (r + e)
respectively, with r being the midpoint of the range and e being the
width of the range.

Since adding two intervals gives an interval with a lower-bound of the
sum of the lower-bounds of both intervals, and a upper-bound of the sum of
the upper-bounds of both intervals, the lower-bound of the sum can be
expressed as:

(r_1 - e_1) + (r_2 - e_2) = (r_1 + r_2) - (e_1 + e_2)

Likewise, the upper-bound can be expressed as:

(r_1 + e_1) + (r_2 + e_2) = (r_1 + r_2) + (e_1 + e_2)

As such, the width of the sum is ((e_1 + e_2) - (- (e_1 + e_2))) / 2 = e_1 + e_2
Therefore, the width of the sum of two intervals is a function only of the widths
of the intervals being added.

For subtracting two intervals, assuming that r2 > r1:
Lower-bound of difference = (r_2 - e_2) - (r_1 + e_1)
                          = (r_2 - r_1) - (e_2 + e_1)
Upper-bound of difference = (r_2 + e_2) - (r_1 - e_1)
                          = (r_2 - r_1) + (e_2 + e_1)

The width of the difference is ((e_2 + e_1) - (- (e_2 + e_1))) / 2 = e_1 + e2
Therefore, the width of the difference of two intervals is a function only
of the widths of the intervals being subtracted.

For multiplication,
p1 = (r_1 - e_1) * (r_2 - e_2)
   = (r_1 * r_2) - (e_1 * r_2) - (e_2 * r_1) + (e_1 * e_2)
p2 = (r_1 - e_1) * (r_2 + e_2)
   = (r_1 * r_2) - (e_1 * r_2) + (e_2 * r_1) - (e_1 * e_2)
p3 = (r_1 + e_1) * (r_2 - e_2)
   = (r_1 * r_2) + (e_1 * r_2) - (e_2 * r_1) - (e_1 * e_2)
p4 = (r_1 + e_1) * (r_2 + e_2)
   = (r_1 * r_2) + (e_1 * r_2) + (e_2 * r_1) + (e_1 * e_2)

Assume that the smallest value of these 4 is p1, and the largest value is p4.
Lower-bound of interval = ((r_1 * r_2) + (e_1 * e_2)) - ((e_1 * r_2) + (e_2 * r_1))
Upper-bound of interval = ((r_1 * r_2) + (e_1 * e_2)) + ((e_1 * r_2) + (e_2 * r_1))

Width = (((e_1 * r_2) + (e_2 * r_1)) - (- ((e_1 * r_2) + (e_2 * r_1)))) / 2
      = (e_1 * r_2) + (e_2 * r_1)

This is not equal to e_1 + e_2, thus the width of the product of two intervals is not a
function only of the widths of the intervals being multiplied. By similar argument, this
does not hold true for division, since division involves multiplication.|#


#|Exercise 2.10: Ben Bitdiddle, an expert systems programmer, looks over
Alyssa’s shoulder and comments that it is not clear what it means to divide
by an interval that spans zero. Modify Alyssa’s code to check for this condition and
to signal an error if it occurs.|#

(define (better-div-interval x y)
  (if (< (* (lower-bound y) (upper-bound y)) 0)
      (error "Cannot divide by zero!")
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))))))

#|Exercise 2.11: In passing, Ben also cryptically comments:
“By testing the signs of the endpoints of the intervals, it is
possible to break mul-interval into nine cases, only one
of which requires more than two multiplications.” Rewrite
this procedure using Ben’s suggestion|#

(define (case-mul-interval x y)
  (let ((p1 (lower-bound x))
        (p2 (upper-bound x))
        (p3 (lower-bound y))
        (p4 (upper-bound y)))
    (let ((p1-pos? (positive? p1))
          (p2-pos? (positive? p2))
          (p3-pos? (positive? p3))
          (p4-pos? (positive? p4)))
      (cond ((and p1-pos? p2-pos?) (make-interval (* (if p3-pos? p1 p2) p3) (* (if p4-pos? p2 p1) p4)))
            ((not (or p1-pos? p2-pos?)) (make-interval (* (if p4-pos? p1 p2) p4) (* (if p3-pos? p2 p1) p3)))
            (else (cond ((and p3-pos? p4-pos?) (make-interval (* p1 p4) (* p2 p4)))
                        ((not (or p3-pos? p4-pos?)) (make-interval (* p2 p3) (* p1 p3)))
                        (else (make-interval (min (* p2 p3) (* p1 p4)) (max (* p1 p3) (* p2 p4))))))))))

#|Cases:
(p1,p2,p3,p4) |    min    |    max
  (-,-,-,-)   | (p2 * p4) | (p1 * p3)
  (-,-,-,+)   | (p1 * p4) | (p1 * p3)
  (-,-,+,+)   | (p1 * p4) | (p2 * p3)
  (-,+,-,-)   | (p2 * p3) | (p1 * p3)
  (-,+,-,+)   | (p2 * p3) | (p1 * p3)
              |     or    |     or
              | (p1 * p4) | (p2 * p4)
  (-,+,+,+)   | (p1 * p4) | (p2 * p4)
  (+,+,-,-)   | (p2 * p3) | (p1 * p4)
  (+,+,-,+)   | (p2 * p3) | (p2 * p4)
  (+,+,+,+)   | (p1 * p3) | (p2 * p4)
|#

#|Exercise 2.12: Define a constructor make-center-percent
that takes a center and a percentage tolerance and produces
the desired interval. You must also define a selector
percent that produces the percentage tolerance for a given
interval. The center selector is the same as the one shown
above.|#

(define (make-center-percent c p)
  (let ((w (/ (* c p) 100)))
    (make-interval (- c w) (+ c w))))
(define (percent interval)
  (let ((x (lower-bound interval))
        (y (upper-bound interval)))
    (* (/ (- y x) (+ x y)) 100)))

#|Exercise 2.13: Show that under the assumption of small
percentage tolerances there is a simple formula for the
approximate percentage tolerance of the product of two
intervals in terms of the tolerances of the factors. You may
simplify the problem by assuming that all numbers are positive|#

#|Assuming all numbers are positive,
the lower-bound of the first interval (x_low) can be expressed as c_1 * (100 - p_1) / 100
the upper-bound of the first interval (x_high) can be expressed as c_1 * (100 + p_1) / 100
where p_1 is the percentage tolerance of the first interval

The lower-bound of the second interval (y_low) can be expressed as c_2 * (100 - p_2) / 100
the upper-bound of the second interval (y_high) can be expressed as c_2 * (100 + p_2) / 100
where p_2 is the percentage tolerance of the second interval

Since 0 < x_low < x_high and 0 < y_low < y_high,
the lower-bound of the product is the product of the lower-bounds of both intervals,
while the upper-bound of the product is the product of the upper-bounds of both intervals.

Lower-bound = x_low * y_low
            = (c_1 * (100 - p_1) / 100) * (c_2 * (100 - p_2) / 100)
            = (c_1 * c_2) * (1 - p_1 / 100) * (1 - p_2 / 100)
            = (c_1 * c_2) * (1 - p_1 / 100 - p_2 / 100 + (p_1 * p_2) / 10000)

Upper-bound = x_high * y_high
            = (c_1 * (100 + p_1) / 100) * (c_2 * (100 + p_2) / 100)
            = (c_1 * c_2) * (1 + p_1 / 100) * (1 + p_2 / 100)
            = (c_1 * c_2) * (1 + p_1 / 100 + p_2 / 100 + (p_1 * p_2) / 10000)

For small values of p_1 and p_2, (p_1 * p_2) / 10000 is negligible, thus the
percentage tolerance can be expressed as p_1 + p_2 i.e. the approximate percentage
tolerance of the product of two intervals is the sum of the percentage tolerance
of the factors.
|#

#|Exercise 2.14: Demonstrate that Lem is right. Investigate
the behavior of the system on a variety of arithmetic expressions.
Make some intervals A and B, and use them in computing the
expressions A/A and A/B. You will get the most insight by using
intervals whose width is a small percentage of the center value.
Examine the results of the computation in center-percent form
(see Exercise 2.12).|#

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one (add-interval (div-interval one r1) (div-interval one r2)))))

(percent (par1 (make-center-percent 50 1) (make-center-percent 65 1))) ; 3.00%
(percent (par2 (make-center-percent 50 1) (make-center-percent 65 1))) ; 1.00%
(percent (div-interval (make-center-percent 50 1) (make-center-percent 50 1))) ; 2.00%
(percent (div-interval (make-center-percent 50 1) (make-center-percent 65 1))) ; 2.00%

#|Exercise 2.15: Eva Lu Ator, another user, has also noticed
the different intervals computed by different but algebraically
equivalent expressions. She says that a formula to compute
with intervals using Alyssa’s system will produce tighter
error bounds if it can be written in such a form that no variable
that represents an uncertain number is repeated. us,
she says, par2 is a “better” program for parallel resistances
than par1. Is she right? Why?|#

#|She is right. Each time an interval is introduced into the
expression, the tolerance of the overall expression increases.
Since there is no way to reduce the tolerance by introducing an
interval into the expression, the only way to produce a tighter
error bound to use an algebraic form where no variable that represents
an uncertain number is repeated.
|#

#|Exercise 2.16: Explain, in general, why equivalent algebraic
expressions may lead to different answers. Can you
devise an interval-arithmetic package that does not have
this shortcoming, or is this task impossible? (Warning: This
problem is very difficult.)|#

#|Each interval, though they may share the same center value
and range, may not have the same actual value, thus each time
they are introduced into the expression, they add to the overall
tolerance of the expression. For example, (make-interval 1 5) can
refer to both 2 and 4. Any interval-arithmetic package devised will
therefore need to incorporate a notion of identity, i.e. intervals A,
B, C, etc. used in the expression, so that the expression can be
converted to a different algebraic form that uses fewer variables
that represent uncertain numbers.

Some examples of expressions that the package should be able to convert,
with A and B being intervals:

A + A + A + ... (n times) = nA
mA / nA = m / n
nA - mA = (n - m)A
AB / (A + B) = 1/ (1/A + 1/B)|#

