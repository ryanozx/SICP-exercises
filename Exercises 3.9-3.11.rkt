#lang sicp

#|Exercise 3.9: In Section 1.2.1 we used the substitution model to analyze two procedures
for computing factorials, a recursive version

(define (factorial n)
(if (= n 1) 1 (* n (factorial (- n 1)))))

and an iterative version

(define (factorial n) (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
(if (> counter max-count)
product
(fact-iter (* counter product)
(+ counter 1)
max-count)))

Show the environment structures created by evaluating (factorial 6) using each version
of the factorial procedure.
|#

#|
In the recursive version:

Global environment:
other variables
factorial:
 - variables: n
 - body: (if (= n 1) 1 (* n (factorial (- n 1)))))
 - points to global environment 

E1:
 - n: 6
 - points to global environment
(* 6 (factorial 5))

E2:
 - n: 5
 - points to global environment
(* 5 (factorial 4))

E3:
 - n: 4
 - points to global environment
(* 4 (factorial 3))

E4:
 - n: 3
 - points to global environment
(* 3 (factorial 2))

E5:
 - n: 2
 - points to global environment
(* 2 (factorial 1))

E6:
 - n: 1
 - points to global environment
1



In the iterative version:

Global environment:
other variables
factorial:
 - variables: n
 - body: (fact-iter 1 1 n)
 - points to global environment
fact-iter:
 - variables: product, counter, max-count
 - body: (if (> counter max count) product (fact-iter (* counter product) (+ counter 1) max-count))
 - points to global environment

E1:
 - n: 6
 - points to global environment
(fact-iter 1 1 6)

E2:
 - product: 1, counter: 1, max-count: 6
 - points to global environment
(fact-iter 1 2 6)

E3:
 - product: 1, counter: 2, max-count: 6
 - points to global environment
(fact-iter 2 3 6)

E4:
 - product: 2, counter: 3, max-count: 6
 - points to global environment
(fact-iter 6 4 6)

E5:
 - product: 6, counter: 4, max-count: 6
 - points to global environment
(fact-iter 24 5 6)

E6:
 - product: 24, counter: 5, max-count: 6
 - points to global environment
(fact-iter 120 6 6)

E7:
 - product: 120, counter: 6, max-count: 6
 - points to global environment
(fact-iter 720 7 6)

E8:
 - product: 720, counter: 7, max-count: 6
 - points to global environment
720
|#

#|
Exercise 3.10: In the make-withdraw procedure, the local variable balance is created
as a parameter of make-withdraw. We could also create the local state variable explicitly,
using let, as follows:

(define (make-withdraw initial-amount)
(let ((balance initial-amount))
(lambda (amount)
(if (>= balance amount)
(begin (set! balance (- balance amount))
balance)
"Insufficient funds"))))

Recall from Section 1.3.2 that let is simply syntactic sugar for a procedure call:

(let ((⟨var⟩ ⟨exp⟩)) ⟨body⟩)

is interpreted as an alternate syntax for

((lambda (⟨var⟩) ⟨body⟩) ⟨exp⟩)

Use the environment model to analyze this alternate version of make-withdraw, drawing
figures like the ones above to illustrate the interactions

(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))

Show that the two versions of make-withdraw create objects with the same behavior.
How do the environment structures differ for the two versions?
|#

#|
(define W1 (make-withdraw 100))

Global environment:
other variables
make-withdraw:
 - variable: initial-amount
 - body: ((lambda (balance) (lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds"))) initial-amount)
 - points to global environment
W1:
 - variable: amount
 - body: (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds")
 - points to E2

E1 
 - initial-amount: 100
 - points to global environment

E2
 - balance: initial-amount
 - points to E1



(W1 50)

Global environment:
other variables
make-withdraw:
 - variable: initial-amount
 - body: ((lambda (balance) (lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds"))) initial-amount)
 - points to global environment
W1:
 - variable: amount
 - body: (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds")
 - points to E3

E1 
 - initial-amount: 100
 - points to global environment

E2
 - balance: 50
 - points to E1

E3
 - amount: 50
 - points to E2


(define W2 (make-withdraw 100))

Global environment:
other variables
make-withdraw:
 - variable: initial-amount
 - body: ((lambda (balance) (lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds"))) initial-amount)
 - points to global environment
W1:
 - variable: amount
 - body: (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds")
 - points to E3
W2:
 - variable: amount
 - body: (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds")
 - points to E5

E1 
 - initial-amount: 100
 - points to global environment

E2
 - balance: 50
 - points to E1

E3
 - amount: 50
 - points to E2

E4
 - initial-amount: 100
 - points to global environment

E5
 - balance: initial-amount
 - points to E4
|#

#|
Exercise 3.11: In Section 3.2.3 we saw how the environment model described the behavior
of procedures with local state. Now we have seen how internal definitions work. A
typical message-passing procedure contains both of these aspects. Consider the bank account
procedure of Section 3.1.1:

(define (make-account balance)
(define (withdraw amount)
(if (>= balance amount)
(begin (set! balance (- balance amount))
balance)
"Insufficient funds"))
(define (deposit amount)
(set! balance (+ balance amount))
balance)
(define (dispatch m)
(cond ((eq? m 'withdraw) withdraw)
((eq? m 'deposit) deposit)
(else
(error "Unknown request: MAKE-ACCOUNT"
m))))
dispatch)

Show the environment structure generated by the sequence of interactions

(define acc (make-account 50))
((acc 'deposit) 40)
90
((acc 'withdraw) 60)
30

Where is the local state for acc kept? Suppose we define another account

(define acc2 (make-account 100))

How are the local states for the two accounts kept distinct?
Which parts of the environment structure are shared between acc and acc2?
|#

#|

(define acc (make-account 50))

Global environment:
other variables
make-account:
 - variable: balance
 - body: ((define (withdraw amount) ...)
          (define (deposit amount) ...)
          (define (dispatch m) ...)
         dispatch)
 - points to global environment
acc:
 - variable: m
 - body: (cond ((eq? m 'withdraw) withdraw)
               ((eq? m 'deposit) deposit)
               (else (error "Unknown request: MAKE-ACCOUNT" m)))
 - points to E1

E1:
 - balance: 50
withdraw:
 - variable: amount
 - body: (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds))
 - points to E1
deposit:
 - variable: amount
 - body: (set! balance (+ balance amount)) balance)
 - points to E1
dispatch:
 - variable: m
 - body: (cond ((eq? m 'withdraw) withdraw)
               ((eq? m 'deposit) deposit)
               (else (error "Unknown request: MAKE-ACCOUNT" m)))


((acc 'deposit) 40)

Global environment:
other variables
make-account:
 - variable: balance
 - body: ((define (withdraw amount) ...)
          (define (deposit amount) ...)
          (define (dispatch m) ...)
         dispatch)
 - points to global environment
acc:
 - variable: m
 - body: (cond ((eq? m 'withdraw) withdraw)
               ((eq? m 'deposit) deposit)
               (else (error "Unknown request: MAKE-ACCOUNT" m)))
 - points to E1

E1:
 - balance: 90
withdraw:
 - variable: amount
 - body: (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds))
 - points to E1
deposit:
 - variable: amount
 - body: (set! balance (+ balance amount)) balance)
 - points to E1
dispatch:
 - variable: m
 - body: (cond ((eq? m 'withdraw) withdraw)
               ((eq? m 'deposit) deposit)
               (else (error "Unknown request: MAKE-ACCOUNT" m)))

E2 (dispatch m):
 - m: 'deposit
 - points to E1

E3 (deposit amount):
 - amount: 40
 - points to E1


((acc 'withdraw) 60)

Global environment:
other variables
make-account:
 - variable: balance
 - body: ((define (withdraw amount) ...)
          (define (deposit amount) ...)
          (define (dispatch m) ...)
         dispatch)
 - points to global environment
acc:
 - variable: m
 - body: (cond ((eq? m 'withdraw) withdraw)
               ((eq? m 'deposit) deposit)
               (else (error "Unknown request: MAKE-ACCOUNT" m)))
 - points to E1

E1:
 - balance: 30
withdraw:
 - variable: amount
 - body: (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds))
 - points to E1
deposit:
 - variable: amount
 - body: (set! balance (+ balance amount)) balance)
 - points to E1
dispatch:
 - variable: m
 - body: (cond ((eq? m 'withdraw) withdraw)
               ((eq? m 'deposit) deposit)
               (else (error "Unknown request: MAKE-ACCOUNT" m)))

E2 (dispatch m):
 - m: 'deposit
 - points to E1

E3 (deposit amount):
 - amount: 40
 - points to E1

E4 (dispatch m):
 - m: 'withdraw
 - points to E1

E5 (withdraw amount):
 - amount: 60
 - points to E1


The local state for acc is kept in E1.

(define acc2 (make-account 100))

Global environment:
other variables
make-account:
 - variable: balance
 - body: ((define (withdraw amount) ...)
          (define (deposit amount) ...)
          (define (dispatch m) ...)
         dispatch)
 - points to global environment
acc:
 - variable: m
 - body: (cond ((eq? m 'withdraw) withdraw)
               ((eq? m 'deposit) deposit)
               (else (error "Unknown request: MAKE-ACCOUNT" m)))
 - points to E1
acc2:
 - variable: m
 - body: (cond ((eq? m 'withdraw) withdraw)
               ((eq? m 'deposit) deposit)
               (else (error "Unknown request: MAKE-ACCOUNT" m)))
 - points to E6

E1:
 - balance: 30
withdraw:
 - variable: amount
 - body: (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds))
 - points to E1
deposit:
 - variable: amount
 - body: (set! balance (+ balance amount)) balance)
 - points to E1
dispatch:
 - variable: m
 - body: (cond ((eq? m 'withdraw) withdraw)
               ((eq? m 'deposit) deposit)
               (else (error "Unknown request: MAKE-ACCOUNT" m)))
 - points to E1

E2 (dispatch m):
 - m: 'deposit
 - points to E1

E3 (deposit amount):
 - amount: 40
 - points to E1

E4 (dispatch m):
 - m: 'withdraw
 - points to E1

E5 (withdraw amount):
 - amount: 60
 - points to E1

E6:
 - balance: 100
withdraw:
 - variable: amount
 - body: (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds))
 - points to E6
deposit:
 - variable: amount
 - body: (set! balance (+ balance amount)) balance)
 - points to E6
dispatch:
 - variable: m
 - body: (cond ((eq? m 'withdraw) withdraw)
               ((eq? m 'deposit) deposit)
               (else (error "Unknown request: MAKE-ACCOUNT" m)))
 - points to E6


The local states for the two accounts are kept distinct as the local state for acc is kept
in E1 while the local state for acc2 is kept in E6. The part of the environment structure
shared between acc and acc2 is the global environment.
|#



