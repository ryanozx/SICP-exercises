#lang sicp

(#%require rnrs/base-6)
(#%require rnrs/mutable-pairs-6)

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
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


#|
Original code; replaced with code in 2.78

(define (attach-tag-original tag x) (cons tag x))
(define (type-tag-original datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents-original datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))|#


#|
Original code; replaced with code in 2.85:

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))
|#

(define (square x) (* x x))

; generic arithmetic operations

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; scheme-number package

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  ; negate added in 2.88
  (put 'negate '(scheme-number)
       (lambda (x) (tag (- x))))
  ; equ? added in 2.79
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  ; =zero? added in 2.80
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put 'raise '(integer) (lambda (x) (make-rational x 1)))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; rational-number package

; modified to use generic operations in 2.93
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  ; make-rat modified in 2.94 to accomodate simplifying rationals made with polynomials
  (define (make-rat n d)
    (if (=zero? d)
        (error "DIVIDE-BY-ZERO ERROR")
        (if (and (integer? n) (integer? d))
            (make-rat (make-integer n) (make-integer d))
            (let ((reduced-terms (reduce n d)))
              (cons (car reduced-terms) (cadr reduced-terms))))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer x))))
  ; equ? added in 2.79
  (define (equals? x y)
    (equ? (mul (numer x) (denom y)) (mul (numer y) (denom x))))
  ; equalszero? added in 2.80
  (define (equalszero? x)
    (=zero? (numer x)))
  ; convert-real modified in 2.93 to support polynomials for numerators and denominators
  (define (convert-real x)
    (if (not (or (is-poly? (numer x)) (is-poly? (denom x))))
        (div (numer x) (denom x))
        #f))
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  ; negate added in 2.88
  (put 'negate '(rational)
       (lambda (x) (make-rational (- (numer x)) (denom x))))
  ; sine, cosine, exp, arctan added in 2.86
  (put 'sine '(rational) (lambda (x) (make-real (sin (contents (convert-real x))))))
  (put 'cosine '(rational) (lambda (x) (make-real (cos (contents (convert-real x))))))
  (put 'exp '(rational rational) (lambda (x y) (make-real (expt (contents (convert-real x)) (contents (convert-real y))))))
  (put 'arctan '(rational) (lambda (x) (make-real (atan (contents (convert-real x))))))
  (put 'equ? '(rational rational) (lambda (x y) (equals? x y)))
  (put '=zero? '(rational) equalszero?)
  ; raise added in 2.83
  ; raise and project modified in 2.93 to support polynomials for numerators and denominators
  (put 'raise '(rational) (lambda (x) (let ((converted (convert-real x)))
                                        (if converted
                                            (make-real converted)
                                            #f))))
  (put 'project '(rational) (lambda (x) (let ((converted (convert-real x)))
                                          (if converted
                                              (make-integer (round (contents converted)))
                                              #f))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

; complex number package

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z2) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (* (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (/ (angle z1) (angle z2))))
  ; equ? added in 2.79
  (define (equals? z1 z2)
    (and (equ? (real-part z1) (real-part z2)) (equ? (imag-part z1) (imag-part z2))))
  ; =zero? added in 2.80
  (define (equalzero? z1)
    (=zero? (magnitude z1)))
  (define (tag z) (attach-tag 'complex z))
  (install-rectangular-package)
  (install-polar-package)
  ; added in 2.77
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  ; end of added section
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  ; negate added in 2.88
  (put 'negate '(complex) (lambda (z) (make-from-real-imag (sub (real-part z)) (imag-part z))))
  (put 'equ? '(complex complex) (lambda (x y) (equals? x y)))
  (put '=zero? '(complex) (lambda (x) (equalzero? x)))
  (put 'project '(complex) (lambda (x) (real-part x)))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z) (sqrt (+ (square (real-part z))
                                 (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a)))) 'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

#|Exercise 2.77: Louis Reasoner tries to evaluate the expression
(magnitude z) where z is the object shown in Figure
2.24. To his surprise, instead of the answer 5 he gets an error
message from apply-generic, saying there is no method
for the operation magnitude on the types (complex). He
shows this interaction to Alyssa P. Hacker, who says “The
problem is that the complex-number selectors were never
defined for complex numbers, just for polar and rectangular
numbers. All you have to do to make this work is add the
following to the complex package:”

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

Describe in detail why this works. As an example, trace
through all the procedures called in evaluating the expression
(magnitude z) where z is the object shown in Figure
2.24. In particular, how many times is apply-generic invoked?
What procedure is dispatched to in each case?|#

#|
Initially, the complex package does not include selectors for complex numbers,
thus apply-generic returns an error message when it tries to call for the magnitude
procedure on z, where z is a complex number created by either make-complex-from-real-imag
or make-complex-from-mag-ang. However, by adding the above code to the complex package,
the selectors are now defined for complex numbers.

(magnitude z) invokes apply-generic on the complex number z:
(apply-generic 'magnitude z), which returns the magnitude procedure as defined in the
complex package i.e. (define (magnitude z) (apply-generic 'magnitude z))

However, since the first apply-generic applies the magnitude procedure on the contents of z
i.e. it strips the 'complex tag from z, resulting in a complex number defined by the rectangular
package, the magnitude procedure returned is applied on the rectangular object.

This magnitude procedure again invokes apply-generic, which looks up the magnitude operation
in the rectangular package and returns the magnitude procedure i.e. (define (magnitude z)
(sqrt (+ (square (real-part z)) (square (imag-part z))))), therefore returning the answer of 5.

Overall, apply-generic is invoked twice.
|#

#|Exercise 2.78: The internal procedures in the scheme-number
package are essentially nothing more than calls to the primitive
procedures +, -, etc. It was not possible to use the primitives of
the language directly because our type-tag system requires that each
data object have a type attached to it. In fact, however, all Lisp
implementations do have a type system, which they use internally.
Primitive predicates such as symbol? and number? determine whether data
objects have particular types. Modify the definitions of type-tag,
contents, and attach-tag from Section 2.4.2 so that our generic system
takes advantage of Scheme’s internal type system. That is to say,
the system should work as before except that ordinary numbers should
be represented simply as Scheme numbers rather than as pairs whose car is the
symbol scheme-number|#

(define (attach-tag tag x) (if (eq? tag 'scheme-number)
                                              x
                                              (cons tag x)))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))

#|Exercise 2.79: Define a generic equality predicate equ? that
tests the equality of two numbers, and install it in the generic
arithmetic package. This operation should work for ordinary numbers,
rational numbers, and complex numbers.|#

(define (equ? x y) (apply-generic 'equ? x y))

#|Exercise 2.80: Define a generic predicate =zero? that tests
if its argument is zero, and install it in the generic arithmetic package.
This operation should work for ordinary numbers, rational numbers, and complex numbers.|#

(define (=zero? x) (apply-generic '=zero? x))

; combining data of different types

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

#|Exercise 2.81: Louis Reasoner has noticed that apply-generic
may try to coerce the arguments to each other’s type even
if they already have the same type. Therefore, he reasons,
we need to put procedures in the coercion table to coerce
arguments of each type to their own type. For example, in
addition to the scheme-number->complex coercion shown
above, he would do:

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number
'scheme-number
scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

a. With Louis’s coercion procedures installed, what happens if apply-generic is called with two arguments
of type scheme-number or two arguments of type complex
for an operation that is not found in the table for those
types? For example, assume that we’ve defined a generic
exponentiation operation:
(define (exp x y) (apply-generic 'exp x y))
and have put a procedure for exponentiation in the
Scheme-number package but not in any other package:
;; following added to Scheme-number package
(put 'exp '(scheme-number scheme-number)
(lambda (x y) (tag (expt x y))))
; using primitive expt
What happens if we call exp with two complex numbers as arguments?
b. Is Louis correct that something had to be done about
coercion with arguments of the same type, or does
apply-generic work correctly as is?
c. Modify apply-generic so that it doesn’t try coercion
if the two arguments have the same type.
|#

#|
a. The program will be stuck in a loop. As there is no exponentiation
operation for complex numbers, apply-generic will first try to coerce
the first complex number into a complex number, which is the type of the
second number. Since the coercion procedure exists, apply-generic is called again
with the "new" arguments that are both complex numbers, which is essentially the
same call as the previous call, thus resulting in a infinite loop.

b. Louis is wrong. Since there are no coercion procedures that coerce an argument
to the same type as itself, apply-generic will simply return an error message if
both arguments have the same type and the operation does not exist in the package.

c.|#
(define (apply-generic-skip-same op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (not (eq? type1 type2))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic-skip-same op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic-skip-same op a1 (t2->t1 a2)))
                            (else (error "No method for these types"
                                         (list op type-tags)))))
                    (error "No method for these types" (list op type-tags))))
                (error "No method for these types"
                     (list op type-tags)))))))

#|Exercise 2.82: Show how to generalize apply-generic to handle coercion in
the general case of multiple arguments. One strategy is to attempt to coerce
all the arguments to the type of the first argument, then to the type of
the second argument, and so on. Give an example of a situation where this strategy
(and likewise the two-argument version given above) is not sufficiently general.
(Hint: Consider the case where there are some suitable mixed-type operations present
in the table that will not be tried.)|#

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (apply-generic-multiple op . args)
  (define error-message
    (error "No method for these types" (list op (map type-tag args))))
  (define (coerce x xs target-type)
    (let ((x-type (type-tag x)))
      (if xs
          (if (eq? x-type target-type)
              (cons x xs)
              (let ((coerce-proc (get-coercion x-type target-type)))
                (if coerce-proc
                    (cons (coerce-proc x) xs)
                    false))))))
  (define (loop loop-tags args)
    (if (null? loop-tags)
        (error-message)
        (let ((target-type-tag (type-tag (car loop-tags))))
          (let ((coerced-args (accumulate (lambda (x xs) (coerce x xs target-type-tag)) nil args)))
            (if coerced-args
                (let ((new-proc (get op (map type-tag coerced-args))))
                  (if new-proc
                      (apply new-proc (map contents coerced-args))
                      (error-message)))
                (loop (cdr loop-tags) args))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (loop type-tags args)))))

#|This strategy will not work if there is an operation that requires the arguments to be
coerced to a type that none of the arguments currently have.|#

#|Exercise 2.83: Suppose you are designing a generic arithmetic system for dealing
with the tower of types shown in Figure 2.25: integer, rational, real, complex.
For each type (except complex), design a procedure that raises objects of
that type one level in the tower. Show how to install a generic raise operation
that will work for each type (except complex).
|#
(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  ; gcd added in 2.94
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  ; reduce-integers added in 2.97
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (let ((res (/ x y)))
                       (if (integer? res)
                           (tag res)
                           (make-real res)))))
  ; sine, cosine, exp, arctan added in 2.86
  (put 'sine '(integer)
       (lambda (x) (make-real (sin x))))
  (put 'cosine '(integer)
       (lambda (x) (make-real (cos x))))
  (put 'exp '(integer integer)
       (lambda (x y) (make-real (expt x y))))
  (put 'arctan '(integer)
       (lambda (x) (make-real (atan x))))
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  (put '=zero? '(integer) (lambda (x) (= x 0)))
  ; negate added in 2.88
  (put 'negate '(integer) (lambda (x) (tag (- x))))
  ; greatest-commmon-divisor added in 2.94
  (put 'greatest-common-divisor '(integer integer) (lambda (a b) (tag (gcd a b))))
  (put 'reduce '(integer integer) (lambda (a b) (let ((reduced-ints (reduce-integers a b)))
                                                  (list (tag (car reduced-ints)) (tag (cadr reduced-ints))))))
  (put 'raise '(integer) (lambda (x) (make-rational x 1)))
  (put 'make 'integer (lambda (x) (tag x)))
  'done)

(define (make-integer x) ((get 'make 'integer) x))

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  ; sine, cosine, exp, arctan added in 2.86
  (put 'sine '(real)
       (lambda (x) (tag (sin x))))
  (put 'cosine '(real)
       (lambda (x) (tag (cos x))))
  (put 'exp '(real real)
       (lambda (x y) (tag (expt x y))))
  (put 'arctan '(real)
       (lambda (x) (tag (atan x))))
  (put '=zero? '(real) (lambda (x) (= x 0)))
  (put 'equ? '(real real) (lambda (x y) (= x y)))
  ; negate added in 2.88
  (put 'negate '(real) (lambda (x) (tag (- x))))
  (put 'make 'real (lambda (x) (tag (if (number? x)
                                        x
                                        (contents x)))))
  (put 'project '(real) (lambda (x) (make-rational (round x) 1)))
  (put 'raise '(real) (lambda (x) (make-complex-from-real-imag (make-real x) (make-integer 0))))
  'done)

(define (make-real x) ((get 'make 'real) x))

(define (raise x) (apply-generic 'raise x))

#|Exercise 2.84: Using the raise operation of Exercise 2.83,
modify the apply-generic procedure so that it coerces its
arguments to have the same type by the method of successive
raising, as discussed in this section. You will need to
devise a way to test which of two types is higher in the
tower. Do this in a manner that is “compatible” with the
rest of the system and will not lead to problems in adding
new levels to the tower.|#


(define (apply-generic-tower op . args)
  (define error-message
    (error "No method for these types" (list op (map type-tag args))))
  (define (raise num target-type)
    (let ((current-type (type-tag num)))
      (let ((raise-proc (get 'raise current-type)))
        (if raise-proc
            (let ((new-num (raise-proc num)))
              (if (eq? (type-tag new-num) target-type)
                  new-num
                  (raise new-num target-type)))
            #f))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((num1 (car args))
                    (num2 (cadr args)))
                (let ((raise-num1 (raise num1 (type-tag num2)))
                      (raise-num2 (raise num2 (type-tag num1))))
                  (cond (raise-num1 (apply-generic op raise-num1 num2))
                        (raise-num2 (apply-generic op num1 raise-num2))
                        (else error-message))))
              (error-message))))))

#|Exercise 2.85: This section mentioned a method for “simplifying” a data object
by lowering it in the tower of types as far as possible. Design a procedure drop
that accomplishes this for the tower described in Exercise 2.83. The key is to
decide, in some general way, whether an object can be lowered. For example, the
complex number 1.5 + 0i can be lowered as far as real, the complex number 1 + 0i
can be lowered as far as integer, and the complex number 2 + 3i cannot be lowered
at all. Here is a plan for determining whether an object can be lowered: Begin by
defining a generic operation project that “pushes” an object down in the tower.
For example, projecting a complex number would involve throwing away the imaginary
part. Then a number can be dropped if, when we project it and raise the result back
to the type we started with, we end up with something equal to what we started with.
Show how to implement this idea in detail, by writing a drop procedure that drops
an object as far as possible. You will need to design the various projection operations
and install project as a generic operation in the system. You will also need to make
use of a generic equality predicate, such as described in Exercise 2.79. Finally,
use drop to rewrite apply-generic from Exercise 2.84 so that it “simplifies” its
answers.|#

(define (project x) (apply-generic 'project x))

(define (apply-generic op . args)
  (define (error-message)
    (error "No method for these types" (list op (map type-tag args))))
  (define (raise-target num target-type)
    (let ((raise-proc (get 'raise (list (type-tag num)))))
      (if raise-proc
          (let ((new-num (raise num)))
            (if (eq? (type-tag new-num) target-type)
                new-num
                (raise-target new-num target-type)))
          #f)))
  ; drop modified in 2.93 to skip over polynomials
  (define (drop num)
    (let ((project-proc (get 'project (list (type-tag num)))))
      (cond ((not project-proc) num)
            ((not (project num)) num)
            ((equ? num (raise (project num))) (drop (project num)))
            (else num))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (let ((result (apply proc (map contents args))))
            (if (not (or (eq? op '=zero?) (eq? op 'raise) (eq? op 'equ?) (eq? op 'project) (eq? op 'empty-termlist?) (eq? op 'first-term) (eq? op 'rest-terms) (eq? op 'adjoin-term)))
                (drop result)
                result))
          (if (= (length args) 2)
              (let ((num1 (car args))
                    (num2 (cadr args)))
                (let ((raise-num1 (raise-target num1 (type-tag num2)))
                      (raise-num2 (raise-target num2 (type-tag num1))))
                  (cond (raise-num1 (apply-generic op raise-num1 num2))
                        (raise-num2 (apply-generic op num1 raise-num2))
                        (else error-message))))
              (error-message))))))


#|Exercise 2.86: Suppose we want to handle complex numbers whose real parts,
imaginary parts, magnitudes, and angles can be either ordinary numbers, rational
numbers, or other numbers we might wish to add to the system. Describe and
implement the changes to the system needed to accommodate this. You will have
to define operations such as sine and cosine that are generic over ordinary numbers
and rational numbers.|#

(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (exp x y) (apply-generic 'exp x y))
(define (arctan x) (apply-generic 'arctan x))

(define (install-new-complex-package)
  (define (convert-scheme-number x)
    (cond ((integer? x) (make-integer x))
          ((number? x) (make-real x))
          (else x)))
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) (convert-scheme-number x) (convert-scheme-number y)))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) (convert-scheme-number r) (convert-scheme-number a)))
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z2) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (define (is-equ? z1 z2)
    (and (equ? (real-part z1) (real-part z2)) (equ? (imag-part z1) (imag-part z2))))
  (define (equalszero? z1)
    (=zero? (magnitude z1)))
  (define (tag z) (attach-tag 'complex z))
  (install-new-rectangular-package)
  (install-new-polar-package)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex) is-equ?)
  (put '=zero? '(complex) (lambda (x) (equalszero? x)))
  (put 'project '(complex) (lambda (x) (real-part x)))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-new-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z) (exp (add (exp (real-part z) (make-integer 2))
                                  (exp (imag-part z) (make-integer 2))) (make-real 0.5)))
  (define (angle z)
    (arctan (div (imag-part z) (real-part z))))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a)))) 'done)

(define (install-new-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z) (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (exp (add (exp x (make-integer 2)) (exp y (make-integer 2))) (make-real 0.5)) (arctan y x)))
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

#|Exercise 2.87: Install =zero? for polynomials in the generic
arithmetic package. This will allow adjoin-term to work
for polynomials with coefficients that are themselves polynomials.|#

(define (install-polynomial-package)
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ; (define (the-empty-termlist) '())   removed in 2.90 to facilitate tagging of empty list
  (define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))
  (define (variable? x) (symbol? x))
  ; is-polynomial and order-by-priority added in 2.92
  (define (is-polynomial? x) (eq? (type-tag x) 'polynomial))
  (define (order-by-priority p1 p2)
    (let ((var1 (variable p1))
          (var2 (variable p2)))
      (if (string<? (symbol->string var1) (symbol->string var2))
          (cons p1 p2)
          (cons p2 p1))))
  (define (the-empty-termlist tl) (get 'the-empty-termlist (type-tag tl)))
  (define (add-poly p1 p2)
    ; replaced (make-poly (variable p1) (add-terms (term-list p1) (term-list p2))) with current block of code in 2.92
    (let ((var-order (order-by-priority p1 p2)))
      (let ((high (car var-order))
            (low (cdr var-order)))
        (let ((var-high (variable high)))
          (make-poly var-high (add-terms (term-list high) (term-list (change-var low var-high))))))))
  ; sub-poly added in 2.88
  (define (sub-poly p1 p2)
    ; replaced (make-poly (variable p1) (sub-terms (term-list p1) (term-list p2))) with current block of code in 2.92
    (let ((var-order (order-by-priority p1 p2)))
      (let ((high (car var-order))
            (low (cdr var-order)))
        (let ((var-high (variable high)))
          (make-poly var-high (sub-terms (term-list high) (term-list (change-var low var-high))))))))
  (define (mul-poly p1 p2)
    ; replaced (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2))) with current block of code in 2.92
      (let ((var-order (order-by-priority p1 p2)))
      (let ((high (car var-order))
            (low (cdr var-order)))
        (let ((var-high (variable high)))
          (make-poly var-high (mul-terms (term-list high) (term-list (change-var low var-high))))))))
  ; div-poly and div-terms added in 2.91
  (define (div-poly p1 p2)
    (let ((result-termlist (div-terms (term-list p1) (term-list p2))))
      (list (make-poly (variable p1) (car result-termlist)) (make-poly (variable p1) (cadr result-termlist)))))
  ; gcd-poly added in 2.94
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (gcd-terms (term-list p1) (term-list p2)))
        (error "ERROR: Both polynomials must have the same variable" (list (variable p1) (variable p2)))))
  ;reduce-poly added in 2.97
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((reduced-terms (reduce-terms (term-list p1) (term-list p2))))
          (list (make-poly (variable p1) (car reduced-terms)) (make-poly (variable p2) (cadr reduced-terms))))
        (error "ERROR: Both polynomials must have the same variable" (list (variable p1) (variable p2)))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                ; conditional statement added in 2.92
                                (cond ((and (is-polynomial? (coeff t1)) (not (is-polynomial? (coeff t2)))) (add (coeff t1) (make-polynomial (variable (contents (coeff t1))) (make-sparse (list (make-term 0 (coeff t2)))))))
                                                 ((and (not (is-polynomial? (coeff t1))) (is-polynomial? (coeff t2))) (add (make-polynomial (variable (contents (coeff t2))) (make-sparse (list (make-term 0 (coeff t1))))) (coeff t2)))
                                                 (else (add (coeff t1) (coeff t2)))))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
   ; sub-terms and negate-termlist added in 2.88
  (define (sub-terms L1 L2)
    (add-terms L1 (negate-termlist L2)))
  (define (negate-termlist termlist)
    (if (empty-termlist? termlist)
        (the-empty-termlist termlist)
        (let ((firstterm (first-term termlist)))
          (adjoin-term (make-term (order firstterm) (negate (coeff firstterm))) (negate-termlist (rest-terms termlist))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist L1)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
 (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist L)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      ; conditional statement added in 2.92
                      (cond ((and (is-polynomial? (coeff t1)) (not (is-polynomial? (coeff t2)))) (mul (coeff t1) (make-polynomial (variable (contents (coeff t1))) (make-sparse (list (make-term 0 (coeff t2)))))))
                            ((and (not (is-polynomial? (coeff t1))) (is-polynomial? (coeff t2))) (mul (make-polynomial (variable (contents (coeff t2))) (make-sparse (list (make-term 0 (coeff t1))))) (coeff t2)))
                            (else (mul (coeff t1) (coeff t2)))))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (div-terms L1 L2)
    (let ((list-type (type-tag L1)))
      (let ((empty-termlist-L1 (the-empty-termlist L1)))
        (cond ((empty-termlist? L2) (error "DIVIDE-BY-ZERO ERROR"))
              ((empty-termlist? L1) (list empty-termlist-L1 empty-termlist-L1))
              (else 
               (let ((t1 (first-term L1))
                     (t2 (first-term L2)))
                 (if (> (order t2) (order t1))
                     (list empty-termlist-L1 L1)
                     (let ((new-c (div (coeff t1) (coeff t2)))
                           (new-o (- (order t1) (order t2))))
                       (let ((rest-of-result (div-terms (sub-terms L1 (mul-term-by-all-terms (make-term new-o new-c) L2)) L2)))
                         (list (adjoin-term (make-term new-o new-c) (car rest-of-result)) (cadr rest-of-result)))))))))))
  ; remainder-terms and gcd-terms added in 2.94
  (define (remainder-terms a b)
    (cadr (div-terms a b)))
  ; pseudoremainder-terms and divide-all-coeffs-by-gcd added in 2.96
  (define (pseudoremainder-terms a b)
    (cadr (div-terms (mul-term-by-all-terms (make-term 0 (exp (coeff (first-term b)) (make-integer (add 1 (sub (order (first-term a)) (order (first-term b))))))) a) b)))
  (define (divide-all-coeffs-by-gcd termlist)
    (define (get-all-coeffs termlist)
      (if (empty-termlist? termlist)
          '()
          (cons (contents (coeff (first-term termlist))) (get-all-coeffs (rest-terms termlist)))))
    (if (empty-termlist? termlist)
        (the-empty-termlist termlist)
        (let ((tl-all-coeffs (get-all-coeffs termlist)))
          (let ((gcd-of-all-coeffs (apply gcd tl-all-coeffs)))
            (mul-term-by-all-terms (make-term 0 (div (make-integer 1) (make-integer gcd-of-all-coeffs))) termlist)))))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (divide-all-coeffs-by-gcd a)
        (gcd-terms b (pseudoremainder-terms a b))))
  ; reduce-terms added in 2.97
  (define (reduce-terms n d)
    (let ((gcd (gcd-terms n d)))
      (let ((lead-coeff (coeff (first-term gcd)))
            (num-max-ord (order (first-term n)))
            (den-max-ord (order (first-term d))))
        (let ((O1 (if (> num-max-ord den-max-ord)
                      num-max-ord
                      den-max-ord))
              (O2 (order (first-term gcd))))
          (let ((factor (exp lead-coeff (make-integer (+ 1 (- O1 O2))))))
            (list (car (div-terms (mul-term-by-all-terms (make-term 0 factor) n) gcd)) (car (div-terms (mul-term-by-all-terms (make-term 0 factor) d) gcd))))))))
  (define (zero-terms? termlist)
    (or (empty-termlist? termlist) (and (=zero? (coeff (first-term termlist))) (zero-terms? (rest-terms termlist)))))
  ; swap-vars and change-var added in 2.92
  (define (swap-vars poly innervar innerorder)
    (define (tl-iter termlist)
      (if (empty-termlist? termlist)
          (the-empty-termlist termlist)
          (adjoin-term (make-term (order (first-term termlist)) (make-polynomial innervar (make-sparse (list (make-term innerorder (coeff (first-term termlist))))))) (tl-iter (rest-terms termlist)))))
    (let ((tl (term-list poly))
          (outervar (variable poly)))
      (if (same-variable? outervar innervar)
          poly
          (cond ((empty-termlist? tl) (make-poly outervar (the-empty-termlist tl)))
                ((= (length (contents tl)) 1) (make-poly outervar (make-sparse (list (make-term (order (first-term tl)) (make-polynomial innervar (make-sparse (list (make-term innerorder (coeff (first-term tl)))))))))))
                (else (make-poly outervar (tl-iter tl)))))))
  (define (change-var poly newvar)
    (define (change-iter termlist oldvar)
      (if (empty-termlist? termlist)
          (the-empty-termlist termlist)
          (let ((next (change-iter (rest-terms termlist) oldvar))
                (first-coeff (coeff (first-term termlist)))
                (first-order (order (first-term termlist))))
            (add-terms (if (is-polynomial? first-coeff)
                           (term-list (swap-vars (change-var first-coeff newvar) oldvar first-order))
                           (make-sparse (list (make-term 0 (make-polynomial oldvar (make-sparse (list (first-term termlist))))))))
                       next))))                                
    (if (same-variable? (variable poly) newvar)
        poly
        (make-poly newvar (change-iter (term-list poly) (variable poly)))))
  (define (tag p) (attach-tag 'polynomial p))
  (install-terms-package)
  (install-dense-polynomial-package)
  (install-sparse-polynomial-package)
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (let ((div-results (div-poly p1 p2)))
                         (list (tag (car div-results)) (tag (cadr div-results))))))
  (put 'negate '(polynomial) (lambda (poly) (make-polynomial (variable poly) (negate (term-list poly)))))
  (put '=zero? '(polynomial) (lambda (poly) (zero-terms? (term-list poly))))
  (put 'is-polynomial? 'polynomial (lambda (poly) (is-polynomial? poly)))
  (put 'greatest-common-divisor '(polynomial polynomial) (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'reduce '(polynomial polynomial) (lambda (p1 p2) (let ((reduce-polys (reduce-poly p1 p2)))
                                                          (list (tag (car reduce-polys)) (tag (cadr reduce-polys))))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (is-poly? polynomial)
  ((get 'is-polynomial? 'polynomial) polynomial))

#|Exercise 2.88: Extend the polynomial system to include
subtraction of polynomials. (Hint: You may find it helpful
to define a generic negation operation.)|#

(define (negate x) (apply-generic 'negate x))

#|Exercise 2.89: Define procedures that implement the term-list representation
described above as appropriate for dense polynomials.|#

(define (install-dense-polynomial-package)
  (define (first-term term-list)
    (make-term (- (length term-list) 1) (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (pack-zeros count term-list)
    (if (= count 0)
        term-list
        (cons (make-integer 0) (pack-zeros (- count 1) term-list))))
  (define (get-nth-coeff n term-list)
    (if (= n 0)
        (car term-list)
        (get-nth-coeff (- n 1) (rest-terms term-list))))
  (define (replace-nth-coeff n term term-list)
    (if (= n 0)
        (cons (coeff term) (cdr term-list))
        (cons (car term-list) (replace-nth-coeff (- n 1) term (cdr term-list)))))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (let ((position (- (length term-list) (order term) 1)))
          (cond ((>= (order term) (length term-list)) (cons (coeff term) (pack-zeros (- (order term) (length term-list)) term-list)))
                ((=zero? (get-nth-coeff position term-list)) (replace-nth-coeff position term term-list))
                (else (error "Term already exists in polynomial" term))))))
  (define (empty-termlist? term-list) (null? term-list))
  (define (the-empty-termlist) '())
  (define (make-termlist termlist)
    (if (empty-termlist? termlist)
        (the-empty-termlist)
        (let ((car-coeff (car termlist)))
          (cons (if (integer? car-coeff)
                    (make-integer car-coeff)
                    car-coeff)
                (make-termlist (rest-terms termlist))))))
  (define (tag term-list) (attach-tag 'dense term-list))
  (put 'first-term '(dense) first-term)
  (put 'rest-terms '(dense) (lambda (term-list) (tag (rest-terms term-list))))
  (put 'adjoin-term 'dense (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'empty-termlist? '(dense) (lambda (termlist) (empty-termlist? termlist)))
  (put 'the-empty-termlist 'dense (tag (the-empty-termlist)))
  (put 'make-termlist 'dense (lambda (termlist) (tag (make-termlist termlist))))
  'done)

(define (make-dense term-list) ((get 'make-termlist 'dense) term-list))
        
(define (first-term term-list) (apply-generic 'first-term term-list))
(define (rest-terms term-list) (apply-generic 'rest-terms term-list))
(define (adjoin-term term term-list) ((get 'adjoin-term (type-tag term-list)) term (contents term-list)))
(define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list))

#|Exercise 2.90: Suppose we want to have a polynomial system that is efficient for both
sparse and dense polynomials. One way to do this is to allow both kinds of term-list
representations in our system. The situation is analogous to the complex-number example
of Section 2.4, where we allowed both rectangular and polar representations. To do this we
must distinguish different types of term lists and make the operations on term lists generic. Redesign the polynomial
system to implement this generalization. This is a major effort, not a local change.|#

(define (install-sparse-polynomial-package)
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (empty-termlist? term-list) (null? term-list))
  (define (the-empty-termlist) '())
  (define (make-termlist termlist)
    (if (empty-termlist? termlist)
        (the-empty-termlist)
        (let ((first (first-term termlist)))
          (adjoin-term (if (integer? (coeff first))
                           (make-term (order first) (make-integer (coeff first)))
                           first)
                       (make-termlist (rest-terms termlist))))))
  (define (tag term-list) (attach-tag 'sparse term-list))
  (put 'first-term '(sparse) first-term)
  (put 'rest-terms '(sparse) (lambda (term-list) (tag (rest-terms term-list))))
  (put 'adjoin-term 'sparse (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'empty-termlist? '(sparse) (lambda (termlist) (empty-termlist? termlist)))
  (put 'the-empty-termlist 'sparse (tag (the-empty-termlist)))
  (put 'make-termlist 'sparse (lambda (termlist) (tag (make-termlist termlist))))
  'done)

(define (make-sparse termlist) ((get 'make-termlist 'sparse) termlist))

(define (install-terms-package)
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (put 'make-term 'term (lambda (order coeff) (make-term order coeff)))
  (put 'order 'term (lambda (term) (order term)))
  (put 'coeff 'term (lambda (term) (coeff term)))
  'done)

(define (make-term order coeff) ((get 'make-term 'term) order coeff))
(define (order term) ((get 'order 'term) term))
(define (coeff term) ((get 'coeff 'term) term))

#|
Exercise 2.91: A univariate polynomial can be divided by another one to
produce a polynomial quotient and a polynomial remainder. For example,

(x^5 - 1) / (x^2 - 1) = x^3 + x , remainder x - 1.

Division can be performed via long division. That is, divide the highest-order
term of the dividend by the highest-order term of the divisor. The result is
the first term of the quotient. Next, multiply the result by the divisor, subtract that
from the dividend, and produce the rest of the answer by recursively dividing the
difference by the divisor. Stop when the order of the divisor exceeds the order of
the dividend and declare the dividend to be the remainder. Also, if the dividend ever
becomes zero, return zero as both quotient and remainder.

We can design a div-poly procedure on the model of add-poly and mul-poly. The
procedure checks to see if the two polys have the same variable. If so, div-poly
strips off the variable and passes the problem to div-terms, which performs the
division operation on term lists. div-poly finally reattaches the variable to
the result supplied by div-terms. It is convenient to design div-terms to compute
both the quotient and the remainder of a division. div-terms can take two term lists
as arguments and return a list of the quotient term list and the remainder term list.
Complete the following definition of div-terms by filling in the missing expressions.
Use this to implement div-poly, which takes two polys as arguments and returns a list
of the quotient and remainder polys.|#

#|Refer to 2.87|#

#|Exercise 2.92: By imposing an ordering on variables, extend the polynomial package
so that addition and multiplication of polynomials works for polynomials in different
variables. (is is not easy!)|#

#|Refer to 2.87|#

#|Exercise 2.93: Modify the rational-arithmetic package to use generic operations,
but change make-rat so that it does not aempt to reduce fractions to lowest terms.
Test your system by calling make-rational on two polynomials to produce a rational
function:

(define p1 (make-polynomial 'x '((2 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational p2 p1))

Now add rf to itself, using add. You will observe that this addition procedure does
not reduce fractions to lowest terms|#

#|Refer to rational-number-package|#

#|Exercise 2.94: Using div-terms, implement the procedure remainder-terms and use this
to define gcd-terms as above. Now write a procedure gcd-poly that computes the polynomial
GCD of two polys. (e procedure should signal an error if the two polys are not in the
same variable.) Install in the system a generic operation greatest-common-divisor that
reduces to gcd-poly for polynomials and to ordinary gcd for ordinary numbers. As a test,
try

(define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-polynomial 'x '((3 1) (1 -1))))
(greatest-common-divisor p1 p2)

and check your result by hand.|#

(define (greatest-common-divisor a b) (apply-generic 'greatest-common-divisor a b))

#|Exercise 2.95: Define P1, P2, and P3 to be the polynomials

P_1 : x^2 - 2x + 1,
P_2 : 11x^2 + 7,
P_3 : 13x + 5.

Now define Q1 to be the product of P1 and P2 and Q2 to be the product of P1 and P3,
and use greatest-commondivisor (Exercise 2.94) to compute the  of Q1 and Q2. Note
that the answer is not the same as P1. is example introduces noninteger operations
into the computation, causing difficulties with the GCD algorithm. To understand
what is happening, try tracing gcd-terms while computing the GCD or try performing
the division by hand|#

#|
(define P1 (make-polynomial 'x (make-dense (list 1 -2 1))))
(define P2 (make-polynomial 'x (make-sparse (list (list 2 11) (list 0 7)))))
(define P3 (make-polynomial 'x (make-dense (list 13 5))))

(define Q1 (mul P1 P2)) ; 11x^4 - 22x^3 + 18x^2 - 14x^1 + 7
(define Q2 (mul P1 P3)) ; 13x^3 - 21x^2 + 3x + 5
(greatest-common-divisor Q1 Q2)


(gcd-terms Q1 Q2):

Dividing Q1 by Q2 gives a quotient of 11/13 x + -55/169, and a remainder of
1458/169 x^2 - 2916/169 x + 1458/169

Calling gcd-terms with Q2 and 1458/169 x^2 - 2916/169 x + 1458/169:

Dividing Q2 by 1458/169 x^2 - 2916/169 x + 1458/169 gives a quotient of
2197/1458x + 845/1458 and no remainder

Calling gcd-terms with 1458/169 x^2 - 2916/169 x + 1458/169 and an empty termlist
(no remainder) returns 1458/169 x^2 - 2916/169 x + 1458/169 as the greatest common divisor
|#

#|Exercise 2.96:
a. Implement the procedure pseudoremainder-terms, which is just like remainder-terms
except that it multiplies the dividend by the integerizing factor described above
before calling div-terms. Modify gcd-terms to use pseudoremainder-terms, and verify
that greatestcommon-divisor now produces an answer with integer coefficients on the
example in Exercise 2.95.

b. e  now has integer coefficients, but they are larger than those of P1. Modify
gcd-terms so that it removes common factors from the coefficients of the answer by
dividing all the coefficients by their (integer) greatest common divisor.|#

#|Refer to 2.87|#

#|Exercise 2.97:
a. Implement this algorithm as a procedure reduce-terms that takes two term lists n
and d as arguments and re291turns a list nn, dd, which are n and d reduced to lowest
terms via the algorithm given above. Also write a procedure reduce-poly, analogous to
add-poly, that checks to see if the two polys have the same variable. If so, reduce-poly
strips off the variable and passes the problem to reduce-terms, then reaaches the variable
to the two term lists supplied by reduce-terms.

b. Define a procedure analogous to reduce-terms that does what the original make-rat did
for integers:

(define (reduce-integers n d)
(let ((g (gcd n d)))
(list (/ n g) (/ d g))))

and define reduce as a generic operation that calls apply-generic to dispatch to either
reduce-poly (for polynomial arguments) or reduce-integers (for schemenumber arguments).
You can now easily make the rationalarithmetic package reduce fractions to lowest terms
by having make-rat call reduce before combining the given numerator and denominator to
form a rational number. e system now handles rational expressions in either integers
or polynomials. To test your program, try the example at the beginning of this extended
exercise:

(define p1 (make-polynomial 'x '((1 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1) (0 -1))))
(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))
(add rf1 rf2)

See if you get the correct answer, correctly reduced to lowest terms.|#

(define (reduce n d) (apply-generic 'reduce n d))

#| Tests:
(define p1 (make-polynomial 'x (make-sparse (list (list 1 1) (list 0 1)))))
(define p2 (make-polynomial 'x (make-sparse (list (list 3 1) (list 0 -1)))))
(define p3 (make-polynomial 'x (make-sparse (list (list 1 1)))))
(define p4 (make-polynomial 'x (make-sparse (list (list 2 1) (list 0 -1)))))
(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))
(add rf1 rf2)

|#

(install-scheme-number-package)
(install-rational-package)
(install-integer-package)
(install-real-package)
(install-new-complex-package)
(install-polynomial-package)