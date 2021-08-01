#lang sicp

#|
Exercise 3.38: Suppose that Peter, Paul, and Mary share a joint bank account that initially
contains $100. Concurrently, Peter deposits $10, Paul withdraws $20, and Mary withdraws
half the money in the account, by executing the following commands:

Peter: (set! balance (+ balance 10))
Paul: (set! balance (- balance 20))
Mary: (set! balance (- balance (/ balance 2)))

a. List all the different possible values for balance after these three transactions have
been completed, assuming that the banking system forces the three processes to run
sequentially in some order.

b. What are some other values that could be produced if the system allows the processes to
be interleaved? Draw timing diagrams like the one in Figure 3.29 to explain how these values
can occur.
|#

#|
a. Possible sequences:

- Peter, Paul, Mary: 45
- Peter, Mary, Paul: 35
- Paul, Peter, Mary: 45
- Paul, Mary, Peter: 50
- Mary, Peter, Paul: 40
- Mary, Paul, Peter: 50

b. Let there be 6 operations R1, R2, R3, S1, S2, S3, with R representing Read and S
representing Set.

Conditions:
- Each Set will set the value based on the value obtained for its respective Read e.g.
S1 sets balance based on the value obtained by R1
- Each Set operation can only come after its respective Read operation i.e. Sn cannot come
before Rn

Possible arrangements:

R R R S S S - Since all 3 Read operations obtain 100 as the value, the final balance value
              will only be determined by the final Set operation, hence the 3 possible values
              for this arrangement are 110, 80, and 50

R R S R S S - If the last S corresponds with either of the first two Rs, the final balance
              value is either 110, 80, or 50 for similar reasons as the previous arrangement.
              If it corresponds with the last R however, the final balance value is equivalent
              to running two of the three processes sequentially.

              (Peter, Paul): 90, (Peter, Mary): 55, (Paul, Peter): 90, (Paul, Mary): 40,
              (Mary, Peter): 60, (Mary, Paul): 30

              Hence the possible values are 30, 40, 55, 60, 90

R R S S R S - If the last S corresponds to either of the first two Rs, the final balance
              value is either 110, 80, or 50 similar to arrangement 1. However, if it
              corresponds to the last R, the value obtained by the last R is equivalent to
              the value set by the second S, whose R obtains a value of 100 i.e. the final
              balance value is equivalent to running two of the three processes sequentially.

              Therefore, the possible values are 30, 40, 55, 60, 90, in addition to 50, 80, 110.

R S R R S S - If the last S corresponds to the first R, the final balance value is either 110,
              80, or 50. However, if it corresponds to the second of third R, the final
              balance value is equivalent to running two of the three processes sequentially.

              Therefore, this arrangement produces the same values as arrangements 2 and 3.

R S R S R S - this is the same arrangement as described in (a), where there is no interleaving
              of processes, hence possible values are 35, 40, 45, and 50

From these 5 arrangements, the following possible values are produced: 30, 55, 60, 80, 90,
and 110.
|#

#|
Exercise 3.39: Which of the five possibilities in the parallel execution shown above remain
if we instead serialize execution as follows:

(define x 10)
(define s (make-serializer))
(parallel-execute
   (lambda () (set! x ((s (lambda () (* x x))))))
   (s (lambda () (set! x (+ x 1)))))
|#

#|
The processes are:
P1a: Access and compute (* x x)
P1b: Setting x to P1a
P2: Access and compute and setting x to (+ x 1)

Both P1a and P2 are serialised. However, since P1b is not serialised, it can interrupt P2,
which can be broken down into the following two operations:
P2a: Access and compute (+ x 1)
P2b: Setting x to P2a

The possibilities are:
P1a, P1b, P2a, P2b: 101
P2a, P2b, P1a, P1b: 121
P1a, P2a, P2b, P1b: 100
P1a, P2a, P1b, P2b: 11
|#

#|
Exercise 3.40: Give all possible values of x that can result from executing

(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

Which of these possibilities remain if we instead use serialized procedures:

(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))
|#

#|
For the first parallel-execute, let there be the following processes:

For (set! x (* x x)):
P1: Access x
P2: Access x
P3: Set x to P1 * P2

For (set! x (* x x x)):
P4: Access x
P5: Access x
P6: Access x
P7: Set x to P4 * P5 * P6

Possible orders:
P1, P2, P3, P4, P5, P6, P7: 1000000
P1, P2, P4, P5, P6, P3, P7: 1000 (including all valid permutations for P1, P2, P4, P5, P6
in the first 5 where P1 comes before P2, P4 comes before P5 and P6, P5 comes before P6)
P1, P2, P4, P5, P6, P7, P3: 100 (including all valid permutations for P1, P2, P4, P5, P6
in the first 5 where P1 comes before P2, P4 comes before P5 and P6, P5 comes before P6)
P1, P4, P5, P6, P7, P2, P3: 10000 (including all valid permutations for P1, P4, P5, P6 in
the first 4)
P1, P4, P2, P3, P5, P6, P7: 100000 (including all valid permutations for P1 and P4 in the
first 2)
P1, P4, P5, P2, P3, P6, P7: 10000 (including all valid permutations for P1, P4, P5 in the
first 3)

Thus the possible values of x are 100, 1000, 10000, 100000, 1000000.


For the second parallel-execute, the only possibility that remains is 1000000.
|#

#|
Exercise 3.41: Ben Bitdiddle worries that it would be better to implement the bank account
as follows (where the commented line has been changed):

(define (make-account balance)
   (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance
                 (- balance amount))
                 balance)
          "Insufficient funds"))
   (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
   (let ((protected (make-serializer)))
      (define (dispatch m)
         (cond ((eq? m 'withdraw) (protected withdraw))
               ((eq? m 'deposit) (protected deposit))
               ((eq? m 'balance)
                ((protected
                   (lambda () balance)))) ; serialized
               (else
                  (error "Unknown request: MAKE-ACCOUNT"
                         m))))
   dispatch))

because allowing unserialized access to the bank balance can result in anomalous behavior.
Do you agree? Is there any scenario that demonstrates Ben’s concern?
|#

#|
No, as unserialised access to the balance does not modify the state of the balance. Since
there is no intermediate value for the balance in either deposit or withdraw, either balance
returns the balance value before the other procedure is run, or the value after the other
procedure is run. Both are legal, thus this is not a concern.
|#

#|
Exercise 3.42: Ben Bitdiddle suggests that it’s a waste of time to create a new serialized
procedure in response to every withdraw and deposit message. He says that makeaccount could
be changed so that the calls to protected are done outside the dispatch procedure. That is,
an account would return the same serialized procedure (which was created at the same time
as the account) each time it is asked for a withdrawal procedure.

(define (make-account balance)
   (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
   (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
   (let ((protected (make-serializer)))
      (let ((protected-withdraw (protected withdraw))
            (protected-deposit (protected deposit)))
         (define (dispatch m)
            (cond ((eq? m 'withdraw) protected-withdraw)
                  ((eq? m 'deposit) protected-deposit)
                  ((eq? m 'balance) balance)
                  (else
                     (error "Unknown request: MAKE-ACCOUNT"
                            m))))
   dispatch)))

Is this a safe change to make? In particular, is there any difference in what concurrency
is allowed by these two versions of make-account?
|#

#|
Yes, it is a safe change to make. Instead of serialising the procedures when they are calledl
Ben's modification serialises the procedures when the account is created.
|#

#|
Exercise 3.43: Suppose that the balances in three accounts start out as $10, $20, and $30,
and that multiple processes run, exchanging the balances in the accounts. Argue that if
the processes are run sequentially, after any number of concurrent exchanges, the account
balances should be $10, $20, and $30 in some order. Draw a timing diagram like the one
in Figure 3.29 to show how this condition can be violated if the exchanges are implemented
using the first version of the account-exchange program in this section. On the other hand,
argue that even with this exchange program, the sum of the balances in the accounts will be
preserved. Draw a timing diagram to show how even this condition would be violated if we
did not serialize the transactions on individual accounts.
|#

#|
If the processes are run sequentially, i.e. R1, S1, R2, S2 ... Rn, Sn, each exchange (Rn, Sn)
will result in two of the three bank accounts exchanging balances. As such, after any number
of concurrent exchanges, the account balances should be $10, $20, $30 in some order since
there is no interleaving of processes.

However, if the exchanges are implemented using the first version of the account-exchange
program, one way that this condition can be violated is as follows:

Let a1 start with a balance of $10
Let a2 start with a balance of $20
Let a3 start with a balance of $30

Let E1 be the exchange operation between a1 and a2, and E2 be the exchange operation between
a2 and a3.
Let r1 be the balance read from a1, r2 be the balance read from a2, r3 be the
balance read from r3.
Let w1 be the withdrawal operation from a1, w2 be the withdrawal operation
from a2, w3 be the operation from a3.
Let d1 be the deposit operation from a1, d2 be the deposit
operation in a2, d3 be the deposit operation in a3.

E1 - r1: a1 = 10
E1 - r2: a2 = 20
E2 - r2: a2 = 20
E2 - r3: a3 = 30
E1 - difference: 10 - 20 = -10
E1 - w1: a1 = 10 - (-10) = 20
E1 - d2: a2 = 20 + (-10) = 10
E2 - difference: 20 - 30 = -10
E2 - w2: a2 = 10 - (-10) = 20
E2 - d3: a3 = 30 + (-10) = 20

Therefore, a1 = 20, a2 = 20, a3 = 20, thus the condition that the account balances are $10,
$20, $30 in some order has been violated.

However, even with this exchange program, the sum of the balances in the accounts is
preserved, as the amount of money withdrawn from an account within an exchange operation
is equivalent to the amount of money deposited into the other account within the same
exchange operation, as both differ by the difference variable calculated previously, thus the
net change in the sum of the balances is zero.

On the other hand, if the transactions on individual accounts are not serialised:

Since each withdrawal and deposit operation can be broken into two parts, namely the access
and computation part and the setting part,

Let w1a be the access and computation part for w1, and likewise w2a and w3a for w2 and w3
respectively.
Let w1b be the setting part for w1, and likewise w2b and w3b for w2 and w3 respectively.
Let d1a be the access and computation part for d1, and likewise d2a and d3a for d2 and d3
respectively.
Let d1b be the setting part for d1, and likewise d2b and d3b for d2 and d3 respectively.

E1 - r1: a1 = 10
E1 - r2: a2 = 20
E2 - r2: a2 = 20
E2 - r3: a3 = 30
E1 - difference: 10 - 20 = -10
E1 - w1a: a1 = 10; 10 - (-10) = 20
E1 - w1b: a1 = 20
E2 - difference: 20 - 30 = -10
E1 - d2a: a2 = 20; 20 + (-10) = 10
E2 - w2a: a2 = 20; 20 - (-10) = 30 (the withdrawal operation from a2 can run before the
                                    deposit operation has completed as the transactions
                                    are not serialised)
E2 - d2b: a2 = 10
E2 - w2b: a2 = 30
E2 - d3a: a3 = 30; 30 + (-10) = 20
E2 - d3b: a3 = 20

Therefore, a1 = 20, a2 = 30, a3 = 20, giving a sum of 70, which violates the condition that
the sum of the balances in the account are preserved
|#

#|
Exercise 3.44: Consider the problem of transferring an amount from one account to another.
Ben Bitdiddle claims that this can be accomplished with the following procedure, even if
there are multiple people concurrently transferring money among multiple accounts, using
any account mechanism that serializes deposit and withdrawal transactions, for example,
the version of make-account in the text above.

(define (transfer from-account to-account amount)
   ((from-account 'withdraw) amount)
   ((to-account 'deposit) amount))

Louis Reasoner claims that there is a problem here, and that we need to use a more
sophisticated method, such as the one required for dealing with the exchange problem. Is
Louis right? If not, what is the essential difference between the transfer problem and the
exchange problem? (You should assume that the balance in from-account is at least amount.)
|#

#|
Louis is not right. In the exchange problem, the exchange procedure has an unserialised
access in the form of the difference process, that can occur concurrently with another
exchange procedure. In the transfer problem, there is no such unserialised access.
|#

#|
Exercise 3.45: Louis Reasoner thinks our bank-account system is unnecessarily complex and
error-prone now that deposits and withdrawals aren’t automatically serialized. He suggests
that make-account-and-serializer should have exported the serializer (for use by such
procedures as serializedexchange) in addition to (rather than instead of) using it to
serialize accounts and deposits as make-account did. He proposes to redefine accounts as
follows:

(define (make-account-and-serializer balance)
   (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount)) balance)
          "Insufficient funds"))
   (define (deposit amount)
      (set! balance (+ balance amount)) balance)
   (let ((balance-serializer (make-serializer)))
      (define (dispatch m)
         (cond ((eq? m 'withdraw) (balance-serializer withdraw))
               ((eq? m 'deposit) (balance-serializer deposit))
               ((eq? m 'balance) balance)
               ((eq? m 'serializer) balance-serializer)
               (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))
Then deposits are handled as with the original make-account:

(define (deposit account amount)
   ((account 'deposit) amount))

Explain what is wrong with Louis’s reasoning. In particular, consider what happens when
serialized-exchange is called.
|#

#|
When serialised exchange is called, the exchange procedure is serialised first using the
serialiser from account2, then account1. Within the exchange procedure, the withdraw
procedure is serialised using the same serialiser from account1, which cannot run until the
exchange procedure is completed. However, the exchange procedure will not complete until
the withdraw procedure has run, thus serialised-exchange will never halt.|#

(define (make-serialiser)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialised-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialised-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true) false)))

#|
Exercise 3.46: Suppose that we implement test-and-set! using an ordinary procedure as shown
in the text, without attempting to make the operation atomic. Draw a timing diagram like the
one in Figure 3.29 to demonstrate how the mutex implementation can fail by allowing two
processes to acquire the mutex at the same time.
|#

#|
Suppose there are two concurrent processes calling test-and-set! at the same time, p1 and p2.
Let Rn be the access to (car cell) for process n, and Sn be the process of set-car! for
process n.

Since the test-and-set! procedure is not atomic, the following can occur:

R1: process 1 calls (car cell), which returns false
R2: process 2 calls (car cell), which returns false
S1: since (car cell) is false from R1, process 1 calls (set-car! cell true) and acquires the
mutex as the if-condition returns false
S2: since (car cell) is false from R2, process 2 calls (set-car! cell true) and acquires the
mutex as the if-condition returns false
|#

#|
Exercise 3.47: A semaphore (of sizen) is a generalization of a mutex. Like a mutex, a
semaphore supports acquire and release operations, but it is more general in that up to n
processes can acquire it concurrently. Additional processes that aempt to acquire the
semaphore must wait for release operations. Give implementations of semaphores
a. in terms of mutexes
b. in terms of atomic test-and-set! operations
|#

(define (make-semaphore-mutex n)
  (let ((lock (make-mutex))
        (available-count n))
    (define (semaphore command)
      (cond ((eq? command 'acquire) (lock 'acquire)
                                    (if (= available-count 0)
                                        (begin (lock 'release) (semaphore 'acquire))
                                        (begin (set! available-count (- available-count 1))
                                               (lock 'release))))
            ((eq? command 'release) (lock 'acquire)
                                    (if (= available-count n)
                                        (lock 'release)
                                        (begin (set! available-count (+ available-count 1))
                                               (lock 'release))))))
    semaphore))

(define (make-semaphore-atomic n)
  (let ((available (list false))
        (count n))
    (define (semaphore command)
      (cond ((eq? command 'acquire) (if (test-and-set! available)
                                        (semaphore 'acquire))
                                    (cond ((= count 0) (begin (clear! available)
                                                              (semaphore 'acquire)))
                                          (else (begin (set! count (- count 1))
                                                       (clear! available)))))
            ((eq? command 'release) (if (test-and-set! available)
                                        (semaphore 'release))
                                    (cond ((= count n) (clear! available))
                                          (else (begin (set! count (+ count 1))
                                                       (clear! available)))))))
    semaphore))

#|
Exercise 3.48: Explain in detail why the deadlock-avoidance method described above,
(i.e., the accounts are numbered, and each process aempts to acquire the smaller-numbered
account first) avoids deadlock in the exchange problem. Rewrite serialized-exchange to
incorporate this idea. (You will also need to modify make-account so that each account
is created with a number, which can be accessed by sending an appropriate message.)
|#

#|
This deadlock-avoidance method avoids deadlock by ensuring that each process acquires the
accounts in the exact same sequence i.e. if a1 has a smaller number than a2, when Peter
tries to exchange a1 and a2, the process acquires a1 first then a2. When Paul tries
to exchange a2 and a1, the process also acquires a1 first then a2. As long as the account
protected by the procedure that was before the current procedure is not the same as the
account protected by the procedure after the current procedure, no deadlocks will occur
since if there is any concurrent process, one of the processes would be waiting for the other,
but the other would not be waiting for the former.
|#

(define (generate-id)
  (let ((next-id 0))
    (define (new-id)
      (begin (set! next-id (+ next-id 1))
             next-id))
    ((make-serialiser) new-id)))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serialiser))
        (id (generate-id)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'account-number) id)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (define (exchange a1 a2)
      (let ((difference (- (a1 'balance)
                           (a2 'balance))))
        ((a1 'withdraw) difference)
        ((a2 'deposit) difference)))
    (if (< (account1 'account-number) (account2 'account-number))
        ((serializer1 (serializer2 exchange)) account1 account2)
        ((serializer2 (serializer1 exchange)) account2 account1))))

#|
Exercise 3.49: Give a scenario where the deadlock-avoidance mechanism described above does
not work. (Hint: In the exchange problem, each process knows in advance which accounts it
will need to get access to. Consider a situation where a process must get access to some
shared resources before it can know which additional shared resources it will require.)
|#

#|
The deadlock-avoidance mechanism will not work if the process needs to get access to some
shared resources before it can know which additional shared resources it will require.
For instance, suppose each account has in its local state a list of accounts that it has
to transact with, starting with the first account in the list all the way to the end of the
list. If account 1 has account 2 at the start of its list and vice versa, there will still
be deadlock.
|#