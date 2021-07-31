#lang sicp

#|Exercise 3.1: An accumulator is a procedure that is called repeatedly with a single
numeric argument and accumulates its arguments into a sum. Each time it is called, it
returns the currently accumulated sum. Write a procedure make-accumulator that generates
accumulators, each maintaining an independent sum. e input to make-accumulator
should specify the initial value of the sum; for example

(define A (make-accumulator 5))
(A 10)
15
(A 10)
25|#

(define (make-accumulator sum)
  (lambda (amt)
    (begin (set! sum (+ sum amt))
           sum)))

#|Exercise 3.2: In soware-testing applications, it is useful to be able to count the
number of times a given procedure is called during the course of a computation. Write a
procedure make-monitored that takes as input a procedure, f, that itself takes one input.
e result returned by make-monitored is a third procedure, say mf, that keeps track of the
number of times it has been called by maintaining an internal counter. If the input to mf
is the special symbol how-many-calls?, then mf returns the value of the counter. If the
input is the special symbol reset-count, then mf resets the counter to zero. For any other
input, mf returns the result of calling f on that input and increments the counter. For
instance, we could make a monitored version of the sqrt procedure:

(define s (make-monitored sqrt))
(s 100)
10
(s 'how-many-calls?)
1|#

(define (make-monitored f)
  (let ((calls 0))
    (lambda (arg) (cond ((eq? arg 'how-many-calls?) calls)
                        ((eq? arg 'reset-count) (set! calls 0))
                        (else (set! calls (+ calls 1))
                              (f arg))))))

#|Exercise 3.3: Modify the make-account procedure so that it creates password-protected accounts.at is, make-account
should take a symbol as an additional argument, as in

(define acc (make-account 100 'secret-password))

e resulting account object should process a request only if it is accompanied by the
password with which the account was created, and should otherwise return a complaint:

((acc 'secret-password 'withdraw) 40)
60
((acc 'some-other-password 'deposit) 50)
"Incorrect password|#

(define (make-account balance password)
  (let ((incorrect-no 0))
    (define (call-the-cops)
      (error "Called the cops!"))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch pwd m)
      (if (eq? pwd password)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request: MAKE-ACCOUNT" m)))
          (if (>= incorrect-no 6)
              (call-the-cops)
              (begin (set! incorrect-no (+ incorrect-no 1))
                     (error "Incorrect password")))))
    dispatch))

#|Exercise 3.4: Modify the make-account procedure of Exercise 3.3 by adding another
local state variable so that, if an account is accessed more than seven consecutive times
with an incorrect password, it invokes the procedure callthe-cops.|#

#|Refer to 3.3|#

#|Exercise 3.5: Monte Carlo integration is a method of estimating definite integrals by
means of Monte Carlo simulation. Consider computing the area of a region of space described
by a predicate P(x , y) that is true for points (x , y) in the region and false for points
not in the region. For example, the region contained within a circle of radius 3 centered
at (5, 7) is described by the predicate that tests whether
(x - 5)^2 + (y - 7)^2 ≤ 3^2.

To estimate the area of the region described by such a predicate, begin by choosing a
rectangle that contains the region. For example, a rectangle with diagonally opposite
corners at (2, 4) and (8, 10) contains the circle above. e desired integral is the
area of that portion of the rectangle that lies in the region. We can estimate the
integral by picking, at random, points (x , y) that lie in the rectangle, and testing
P(x , y) for each point to determine whether the point lies in the region. If we try
this with many points, then the fraction of points that fall in the region should give
an estimate of the proportion of the rectangle that lies in the region. Hence,
multiplying this fraction by the area of the entire rectangle should produce
an estimate of the integral.

Implement Monte Carlo integration as a procedure estimateintegral that takes as
arguments a predicate P, upper and lower bounds x1, x2, y1, and y2 for the rectangle,
and the number of trials to perform in order to produce the estimate. Your procedure
should use the same monte-carlo procedure that was used above to estimate π. Use your
estimateintegral to produce an estimate of π by measuring the area of a unit circle.
You will find it useful to have a procedure that returns a number chosen at random from
a given range. e following random-in-range procedure implements this in terms of the
random procedure used in Section 1.2.6, which returns a nonnegative number less than its
input.

(define (random-in-range low high)
(let ((range (- high low)))
(+ low (random range))))
|#

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (circle-test cx cy r) (lambda (x y) (<= (+ (square (- x cx)) (square (- y cy))) (square r))))

(define (estimate-integral test x1 x2 y1 y2 trials)
  (let ((experiment (lambda () (test (random-in-range x1 x2) (random-in-range y1 y2)))))
    (* (monte-carlo trials experiment) (* (- x2 x1) (- y2 y1)))))

(define (estimate-pi trials)
  (estimate-integral (circle-test 0.0 0.0 1.0) -1.0 1.0 -1.0 1.0 trials))

#|Exercise 3.6: It is useful to be able to reset a random-number generator to
produce a sequence starting from a given value. Design a new rand procedure that is
called with an argument that is either the symbol generate or the symbol
reset and behaves as follows: (rand 'generate) produces a new random number;
((rand 'reset) ⟨new-value ⟩) resets the internal state variable to the designated
⟨new-value⟩. Thus, by reseing the state, one can generate repeatable sequences. These
are very handy to have when testing and debugging programs that use random numbers|#

(define rand
  (let ((x 0))
    (define (dispatch message)
      (cond ((eq? message 'generate) (begin (set! x (rand-update x)) x))
            ((eq? message 'reset) (lambda (y) (set! x y)))
            (else (error "Invalid argument" message))))
    dispatch))

(define (rand-update x) (modulo (+ (* 213498031281 x) 442315397) 213423564))

#|Exercise 3.7: Consider the bank account objects created by make-account, with the
password modification described in Exercise 3.3. Suppose that our banking system requires
the ability to make joint accounts. Define a procedure make-joint that accomplishes
this. make-joint should take three arguments. e first is a password-protected account.
e second argument must match the password with which the account was defined in order
for the make-joint operation to proceed. e third argument is a new password. make-joint
is to create an additional access to the original account using the new password.
For example, if peter-acc is a bank account with password open-sesame, then

(define paul-acc
(make-joint peter-acc 'open-sesame 'rosebud))

will allow one to make transactions on peter-acc using the name paul-acc and the password
rosebud. You may wish to modify your solution to Exercise 3.3 to accommodate this
new feature.
|#

(define (make-joint acc oldpwd newpwd)
  (define (withdraw amt) ((acc oldpwd 'withdraw) amt))
  (define (deposit amt) ((acc oldpwd 'deposit) amt))
  (define (dispatch pwd m)
    (if (eq? pwd newpwd)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-JOINT" m)))
        (error "Wrong password")))
  dispatch)

#|Exercise 3.8: When we defined the evaluation model in Section 1.1.3, we said that the
first step in evaluating an expression is to evaluate its subexpressions. But we never
specified the order in which the subexpressions should be evaluated (e.g., left to right
or right to left). When we introduce assignment, the order in which the arguments to a
procedure are evaluated can make a difference to the result.

Define a simple procedure f such that evaluating

(+ (f 0) (f 1))

will return 0 if the arguments to + are evaluated from left to right but will return 1
if the arguments are evaluated from right to le.|#

(define f
  (let ((count 1))
    (lambda (x) (set! count (* count x)) count)))



