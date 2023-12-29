#lang sicp

#|
Exercise 3.12: e following procedure for appending lists was introduced in Section 2.2.1:

(define (append x y)
     (if (null? x)
          y
          (cons (car x) (append (cdr x) y))))

append forms a new list by successively consing the elements of x onto y. e procedure
append! is similar to append, but it is a mutator rather than a constructor. It appends
the lists by splicing them together, modifying the final pair of x so that its cdr is
now y. (It is an error to call append! with an empty x.)

(define (append! x y)
     (set-cdr! (last-pair x) y)
     x)

Here last-pair is a procedure that returns the last pair in its argument:

(define (last-pair x)
     (if (null? (cdr x)) x (last-pair (cdr x))))

Consider the interaction

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

z
(a b c d)

(cdr x)
⟨response⟩

(define w (append! x y))
w
(a b c d)

(cdr x)
⟨response⟩

What are the missing ⟨response⟩s? Draw box-and-pointer diagrams to explain your answer.
|#

#|
The first response is (list 'b).

x -> ['a][.] -> ['b][/]

y -> ['c][.] -> ['d][/]

z -> ['a][.] -> ['b][.] -> ['c][.] -> ['d][\]

Hence (cdr x) returns ['b][/], or (list 'b)


The second response is (list 'b 'c 'd).

After calling w,

x -> ['a][.] -> ['b][.] -> ['c][.] -> ['d][/]

y -> ['c][.] -> ['d][/]

w -> ['a][.] -> ['b][.] -> ['c][.] -> ['d][/]

Hence (cdr x) returns ['b][.] -> ['c][.] -> ['d][/], or (list 'b 'c 'd)
|#

#|
Exercise 3.13: Consider the following make-cycle procedure, which uses the last-pair
procedure defined in Exercise 3.12:

(define (make-cycle x)
     (set-cdr! (last-pair x) x)
     x)

Draw a box-and-pointer diagram that shows the structure
z created by

(define z (make-cycle (list 'a 'b 'c)))

What happens if we try to compute (last-pair z)?
|#

#|
z -> ['a][.] -> ['b][.] -> ['c][.]
      ^                         |
      |                         |
      ---------------------------

It results in an infinite loop.
|#

#|
Exercise 3.14: e following procedure is quite useful, although obscure:

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

loop uses the “temporary” variable temp to hold the old value of the cdr of x, since
the set-cdr! on the next line destroys the cdr. Explain what mystery does in general.
Suppose v is defined by (define v (list 'a 'b 'c 'd)). Draw the box-and-pointer diagram
that represents the list to which v is bound. Suppose that we now evaluate
(define w (mystery v)). Draw box-and-pointer digrams that show the structures v and w
aer evaluating this expression. What would be printed as the values of v and w?
|#

#|
It reverses the list.

v -> ['a][.] -> ['b][.] -> ['c][.] -> ['d][/]

(define w (mystery v))

w -> ['d][.] -> ['c][.] -> ['b][.] -> ['a][/]
v -> ['a][/]

v will be printed as (list 'a), while w will be printed as (list 'a 'b 'c 'd).
|#

#|
Exercise 3.15: Draw box-and-pointer diagrams to explain the effect of set-to-wow! on the
structures z1 and z2 above
|#

#|
(set-to-wow! z1)

z1 -> [.][.]
       |  |
x -> ['wow][.] -> ['b][/]

(set-to-wow! z1) affects (car z1), which is x. It changes the car of x to 'wow. Since both
car and cdr of z1 point to x, modifying x results in both of them being modified, resulting
in (list (list 'wow 'b) 'wow 'b).

(set-to-wow! z2)

z2 -> [.][.] -> ['a][.] -> ['b][/]
       |
      ['wow][.] -> ['b][/]

(set-to-wow! z2) afffects (car z2), which is (list 'a 'b). It changes that to (list 'wow 'b).
Since the car and cdr of z2 both point to different lists, modifying (car z2) does not affect
(cdr z2), resulting in (list (list 'wow 'b) 'a 'b).
|#

#|
Exercise 3.16: Ben Bitdiddle decides to write a procedure to count the number of pairs in
any list structure. “It’s easy,” he reasons. “e number of pairs in any structure is the
number in the car plus the number in the cdr plus one more to count the current pair.”
So Ben writes the following procedure:

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

Show that this procedure is not correct. In particular, draw box-and-pointer diagrams
representing list structures made up of exactly three pairs for which Ben’s procedure
would return 3; return 4; return 7; never return at all.
|#

#|
a -> [1][.] -> [2][.] -> [3][/]

b -> [.][.] -> [2][/]
      |         ^
      |         |
     [1][.] -----

c -> [.][.]
      |  |
     [.][.]
      |  |
     [3][/]


d -> [1][.] -> [2][.] -> [3][.]
      ^                      |
      |                      |
      ------------------------

(count-pairs a) will return 3.
(count-pairs b) will return 4.
(count-pairs c) will return 7.
(count-pairs d) will never return at all.

|#

#|
Exercise 3.17: Devise a correct version of the count-pairs procedure of Exercise 3.16
that returns the number of distinct pairs in any structure. (Hint: Traverse the structure,
maintaining an auxiliary data structure that is used to keep track of which pairs have
already been counted.)
|#

(define (count-pairs structure)
  (let ((memory '()))
    (define (in-memory? x mem)
      (cond ((null? mem) false)
            ((eq? (car mem) x) true)
            (else (in-memory? x (cdr mem)))))
    (define (iter struct)
      (cond ((not (pair? struct)) 0)
            ((in-memory? struct memory) 0)
            (else (begin (set! memory (cons struct memory)) (+ (iter (car struct)) (iter (cdr struct)) 1)))))
    (iter structure)))

#|Exercise 3.18: Write a procedure that examines a list and determines whether it contains
a cycle, that is, whether a program that tried to find the end of the list by taking
successive cdrs would go into an infinite loop. Exercise 3.13 constructed such lists.|#

(define (check-cycle ls)
  (define (seen? item parents)
    (cond ((null? parents) false)
          ((eq? item (car parents)) true)
          (else (seen? item (cdr parents)))))
  (define (iter xs parents)
    (cond ((not (pair? xs)) false)
          ((seen? xs parents) true)
          (else (or (iter (car xs) (cons xs parents)) (iter (cdr xs) (cons xs parents))))))
  (iter ls '()))

#|Exercise 3.19: Redo Exercise 3.18 using an algorithm that takes only a constant amount of
space. (is requires a very clever idea.)
|#

(define (cons-check-cycle ls)
  (define (can-cdr x)
    (if (pair? x)
        (cdr x)
        '()))
  (define (iter a b)
    (cond ((not (pair? a)) false)
          ((not (pair? b)) false)
          ((eq? a b) true)
          ((eq? a (can-cdr b)) true)
          (else (iter (can-cdr a) (can-cdr (can-cdr b))))))
  (iter (can-cdr ls) (can-cdr (can-cdr ls))))

#|
Exercise 3.20: Draw environment diagrams to illustrate the evaluation of the sequence of
expressions

(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)
(car x)
17

using the procedural implementation of pairs given above. (Compare Exercise 3.11.)
|#

#|
(define x (cons 1 2))

Global environment:
other variables
cons:
 - variables: x y
 - body: (define (set-x! v) (set! x v))
         (define (set-y! v) (set! y v))
         (define (dispatch m)
            (cond ((eq? m 'car) x)
                  ((eq? m 'cdr) y)
                  ((eq? m 'set-car!) set-x!)
                  ((eq? m 'set-cdr! set-y!)
                  (else (error "Undefined operation: CONS" m))))
          dispatch
 - points to global environment
car:
 - variables: z
 - body: (z 'car)
 - points to global environment
cdr:
 - variables: z
 - body: (z 'cdr)
 - points to global environment
set-car!
 - variables: z, new-value
 - body: ((z 'set-car!) new-value) z
 - points to global environment
set-cdr!
 - variables: z, new-value
 - body: ((z 'set-cdr!) new-value) z)
 - points to global environment
x
 - variables: x, y
 - body:
 - points to global environment


(define z (cons x x))

Global environment:
other variables
cons:
 - variables: x y
 - body: (define (set-x! v) (set! x v))
         (define (set-y! v) (set! y v))
         (define (dispatch m)
            (cond ((eq? m 'car) x)
                  ((eq? m 'cdr) y)
                  ((eq? m 'set-car!) set-x!)
                  ((eq? m 'set-cdr! set-y!)
                  (else (error "Undefined operation: CONS" m))))
          dispatch
 - points to global environment
car:
 - variables: z
 - body: (z 'car)
 - points to global environment
cdr:
 - variables: z
 - body: (z 'cdr)
 - points to global environment
set-car!
 - variables: z, new-value
 - body: ((z 'set-car!) new-value) z
 - points to global environment
set-cdr!
 - variables: z, new-value
 - body: ((z 'set-cdr!) new-value) z)
 - points to global environment
x
 - variables: x, y
 - body:
 - points to global environment
z
 - variables: x, y
 - body:
 - points to global environment



(set-car! (cdr z) 17)

Global environment:
other variables
cons:
 - variables: x y
 - body: (define (set-x! v) (set! x v))
         (define (set-y! v) (set! y v))
         (define (dispatch m)
            (cond ((eq? m 'car) x)
                  ((eq? m 'cdr) y)
                  ((eq? m 'set-car!) set-x!)
                  ((eq? m 'set-cdr! set-y!)
                  (else (error "Undefined operation: CONS" m))))
          dispatch
 - points to global environment
car:
 - variables: z
 - body: (z 'car)
 - points to global environment
cdr:
 - variables: z
 - body: (z 'cdr)
 - points to global environment
set-car!
 - variables: z, new-value
 - body: ((z 'set-car!) new-value) z
 - points to global environment
set-cdr!
 - variables: z, new-value
 - body: ((z 'set-cdr!) new-value) z)
 - points to global environment
x
 - variables: x, y
 - body:
 - points to E1
z
 - variables: x, y
 - body:
 - points to E2

E1 (x):
 - x: 1 (17 after set-x!)
 - y: 2
 - set-x!:
     - variable: v
     - body: (set! x v)
     - points to E1
 - set-y!:
     - variable: v
     - body: (set! y v)
     - points to E1
 - dispatch:
     - variable: m
     - body: (cond ((eq? m 'car) x)
                   ((eq? m 'cdr) y)
                   ((eq? m 'set-car!) set-x!)
                   ((eq? m 'set-cdr!) set-y!)
                   (else (error "Undefined operation: CONS" m))))
     - points to E1
 - points to global environment

E2 (z):
 - x: x (points to E1)
 - y: x (points to E1)
 - set-x!:
     - variable: v
     - body: (set! x v)
     - points to E2
 - set-y!:
     - variable: v
     - body: (set! y v)
     - points to E2
 - dispatch:
     - variable: m
     - body: (cond ((eq? m 'car) x)
                   ((eq? m 'cdr) y)
                   ((eq? m 'set-car!) set-x!)
                   ((eq? m 'set-cdr!) set-y!)
                   (else (error "Undefined operation: CONS" m))))
     - points to E2
 - points to global environment

E3 (set-car! (cdr z) 17)
 - z: (cdr z) - points to global environment
 - new-value: 17
((cdr z) 'set-car!) 17) (cdr z))

E4 (cdr z)
 - z: points to E2
 - points to global environment
(z 'cdr)

E5 (z 'cdr)
 - m: 'cdr
 - points to E2
x

E6 (x 'set-car!)
 - m: 'set-car!
 - points to E1
set-x!

E7 (set-x! 17)
 - v: 17
 - points to E1
sets x in E1 to 17


(car x)

Global environment:
other variables
cons:
 - variables: x y
 - body: (define (set-x! v) (set! x v))
         (define (set-y! v) (set! y v))
         (define (dispatch m)
            (cond ((eq? m 'car) x)
                  ((eq? m 'cdr) y)
                  ((eq? m 'set-car!) set-x!)
                  ((eq? m 'set-cdr! set-y!)
                  (else (error "Undefined operation: CONS" m))))
          dispatch
 - points to global environment
car:
 - variables: z
 - body: (z 'car)
 - points to global environment
cdr:
 - variables: z
 - body: (z 'cdr)
 - points to global environment
set-car!
 - variables: z, new-value
 - body: ((z 'set-car!) new-value) z
 - points to global environment
set-cdr!
 - variables: z, new-value
 - body: ((z 'set-cdr!) new-value) z)
 - points to global environment
x
 - variables: x, y
 - body:
 - points to E1
z
 - variables: x, y
 - body:
 - points to E2

E1 (x):
 - x: 17
 - y: 2
 - set-x!:
     - variable: v
     - body: (set! x v)
     - points to E1
 - set-y!:
     - variable: v
     - body: (set! y v)
     - points to E1
 - dispatch:
     - variable: m
     - body: (cond ((eq? m 'car) x)
                   ((eq? m 'cdr) y)
                   ((eq? m 'set-car!) set-x!)
                   ((eq? m 'set-cdr!) set-y!)
                   (else (error "Undefined operation: CONS" m))))
     - points to E1
 - points to global environment

E2 (z):
 - x: x (points to E1)
 - y: x (points to E1)
 - set-x!:
     - variable: v
     - body: (set! x v)
     - points to E2
 - set-y!:
     - variable: v
     - body: (set! y v)
     - points to E2
 - dispatch:
     - variable: m
     - body: (cond ((eq? m 'car) x)
                   ((eq? m 'cdr) y)
                   ((eq? m 'set-car!) set-x!)
                   ((eq? m 'set-cdr!) set-y!)
                   (else (error "Undefined operation: CONS" m))))
     - points to E2
 - points to global environment

E3 (set-car! (cdr z) 17)
 - z: (cdr z) - points to global environment
 - new-value: 17
((cdr z) 'set-car!) 17) (cdr z))

E4 (cdr z)
 - z: points to E2
 - points to global environment
(z 'cdr)

E5 (z 'cdr)
 - m: 'cdr
 - points to E2
x

E6 (x 'set-car!)
 - m: 'set-car!
 - points to E1
set-x!

E7 (set-x! 17)
 - v: 17
 - points to E1
sets x in E1 to 17

E8 (car x)
 - z: points to E1
 - points to global environment
(x 'car)

E9 (x 'car)
 - m: 'car
 - points to E1
17
|#

#|
Exercise 3.21: Ben Bitdiddle decides to test the queue implementation described above.
He types in the procedures to the Lisp interpreter and proceeds to try them out:

(define q1 (make-queue))
(insert-queue! q1 'a)
((a) a)
(insert-queue! q1 'b)
((a b) b)
(delete-queue! q1)
((b) b)
(delete-queue! q1)
(() b)

“It’s all wrong!” he complains. “The interpreter’s response shows that the last item is
inserted into the queue twice. And when I delete both items, the second b is still there,
so the queue isn’t empty, even though it’s supposed to be.” Eva Lu Ator suggests that Ben
has misunderstood what is happening. “It’s not that the items are going into the queue
twice,” she explains. “It’s just that the standard Lisp printer doesn’t know how to make
sense of the queue representation. If you want to see the queue printed correctly, you’ll
have to define your own print procedure for queues.” Explain what Eva Lu is talking about.
In particular, show why Ben’s examples produce the printed results that they do.
Define a procedure print-queue that takes a queue as input and prints the sequence of
items in the queue.
|#


(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (make-queue) (cons '() '()))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue) (set-front-ptr! queue new-pair) (set-rear-ptr! queue new-pair) queue)
          (else (set-cdr! (rear-ptr queue) new-pair) (set-rear-ptr! queue new-pair) queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue) (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue))) queue)))

#|
The standard Lisp printer prints a queue as a cons of the front-ptr, which is displayed as
the list of ordered items in the queue since the queue is displayed as a list, and the
rear-ptr, which is a pointer to the last item in the queue. As such, when insert-queue is
called, the interpreter will display the queue as if the last item entered the queue twice,
whereas it only entered the queue once - in the list itself. delete-queue! does not update
the rear-ptr thus when both items are deleted in the list, the rear-ptr still points to 'b,
even though it no longer exists in the queue.
|#

(define (print-queue queue) (display (front-ptr queue)))

#|
Exercise 3.22: Instead of representing a queue as a pair of pointers, we can build a queue
as a procedure with local state. e local state will consist of pointers to the beginning
and the end of an ordinary list. us, the make-queue procedure will have the form

(define (make-queue)
(let ((front-ptr . . . )
(rear-ptr . . . ))
⟨definitions of internal procedures⟩
(define (dispatch m) . . .)
dispatch))

Complete the definition of make-queue and provide implementations of the queue operations
using this representation.
|#

(define (make-queue-ls)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue-ls?) (null? front-ptr))
    (define (set-front-ptr-ls! item) (set! front-ptr item))
    (define (set-rear-ptr-ls! item) (set! rear-ptr item))
    (define (front-queue-ls) (if (empty-queue-ls?)
                            (error "FRONT called with an empty queue")
                            (car front-ptr)))
    (define (insert-queue-ls! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue-ls?) (set-front-ptr-ls! new-pair) (set-rear-ptr-ls! new-pair))
              (else (set-cdr! rear-ptr new-pair) (set-rear-ptr-ls! new-pair)))))
    (define (delete-queue-ls!)
      (cond ((empty-queue-ls?) (error "DELETE! called with an empty queue"))
            (else (set-front-ptr-ls! (cdr front-ptr)))))
    (define (print-queue-ls) front-ptr)
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue-ls?))
            ((eq? m 'front-queue) (front-queue-ls))
            ((eq? m 'insert-queue!) insert-queue-ls!)
            ((eq? m 'delete-queue!) (delete-queue-ls!))
            ((eq? m 'print-queue) (print-queue-ls))
            (else (error "Undefined operation -- QUEUE" m))))
    dispatch))

#|
Exercise 3.23:Adeque (“double-ended queue”) is a sequence in which items can be inserted
and deleted at either the front or the rear. Operations on deques are the constructor
make-deque, the predicate empty-deque?, selectors frontdeque and rear-deque, mutators
front-insert-deque!, rear-insert-deque!, front-delete-deque!, and rear-deletedeque!.
Show how to represent deques using pairs, and give implementations of the operations.
All operations should be accomplished in Θ(1) steps.
|#
(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-deque?) (or (null? front-ptr) (null? rear-ptr)))
    (define (set-front-ptr-dq! new-pair) (set! front-ptr new-pair))
    (define (set-rear-ptr-dq! new-pair) (set! rear-ptr new-pair))
    (define (front-deque) (if (null? front-ptr)
                              (error "No item in deque")
                              (caar front-ptr)))
    (define (rear-deque) (if (null? rear-ptr)
                             (error "No item in deque")
                             (caar rear-ptr)))
    (define (front-insert-deque! item) (let ((new-pair (cons (cons item '()) '())))
                                         (cond ((empty-deque?) (set-front-ptr-dq! new-pair) (set-rear-ptr-dq! new-pair))
                                               (else (set-cdr! new-pair front-ptr) (set-cdr! (car front-ptr) new-pair) (set-front-ptr-dq! new-pair)))))
    (define (rear-insert-deque! item) (let ((new-pair (cons (cons item '()) '())))
                                        (cond ((empty-deque?) (set-front-ptr-dq! new-pair) (set-rear-ptr-dq! new-pair))
                                              (else (set-cdr! (car new-pair) rear-ptr) (set-cdr! rear-ptr new-pair) (set-rear-ptr-dq! new-pair)))))
    (define (front-delete-deque!) (cond ((empty-deque?) (error "Cannot delete from empty deque"))
                                        ((null? (cdr front-ptr)) (set-front-ptr-dq! '()) (set-rear-ptr-dq! '()))
                                        (else (set-front-ptr-dq! (cdr front-ptr)) (set-cdr! (car front-ptr) '()))))
    (define (rear-delete-deque!) (cond ((empty-deque?) (error "Cannot delete from empty deque"))
                                       ((null? (cdar rear-ptr)) (set-front-ptr-dq! '()) (set-rear-ptr-dq! '()))
                                       (else (set-rear-ptr-dq! (cdar rear-ptr)) (set-cdr! rear-ptr '()))))
    (define (print-deque)
      (define (iter-print deque)
        (if (null? deque)
            '()
            (cons (caar deque) (iter-print (cdr deque)))))
      (iter-print front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'empty-deque?) (empty-deque?))
            ((eq? m 'front-deque) (front-deque))
            ((eq? m 'rear-deque) (rear-deque))
            ((eq? m 'front-insert-deque!) front-insert-deque!)
            ((eq? m 'rear-insert-deque!) rear-insert-deque!)
            ((eq? m 'front-delete-deque!) (front-delete-deque!))
            ((eq? m 'rear-delete-deque!) (rear-delete-deque!))
            ((eq? m 'print-deque) (print-deque))
            (else (error "Unknown operation -- DEQUE" m))))
    dispatch))

#|
Exercise 3.24: In the table implementations above, the keys are tested for equality using
equal? (called by assoc). is is not always the appropriate test. For instance, we might
have a table with numeric keys in which we don’t need an exact match to the number we’re
looking up, but only a number within some tolerance of it. Design a table constructor
make-table that takes as an argument a same-key? procedure that will be used to test
“equality” of keys. maketable should return a dispatch procedure that can be used
to access appropriate lookup and insert! procedures for a local table
|#

(define (make-tolerance-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable (cons (cons key-2 value)
                                           (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

#|
Exercise 3.25: Generalizing one- and two-dimensional tables, show how to implement a table
in which values are stored under an arbitrary number of keys and different values may be
stored under different numbers of keys. e lookup and insert! procedures should take as
input a list of keys used to access the table.
|#

(define (make-multi-table)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup table key . subkeys)
      (let ((subtable (assoc key (cdr table))))
        (if subtable
            (cond ((null? subkeys) (cdr subtable))
                  (else (lookup subtable subkeys)))            
            false)))
    (define (insert! table value key . subkeys)
      (let ((subtable (assoc key (cdr table))))
        (if subtable
            (cond ((null? subkeys) (set-cdr! subtable value))
                  (else (insert! subtable value subkeys)))
            (cond ((null? subkeys) (set-cdr! table (cons (cons key value) (cdr table))))
                  (else (set-cdr! table (cons (list key) (cdr table)))
                        (insert! (cadr table) value subkeys)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lambda (key . subkeys) (lookup local-table key subkeys)))
            ((eq? m 'insert-proc) (lambda (value key . subkeys) (insert! local-table value key subkeys)))
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

#|
Exercise 3.26: To search a table as implemented above, one needs to scan through the list
of records. This is basically the unordered list representation of Section 2.3.3. For large
tables, it may be more efficient to structure the table in a different manner. Describe a
table implementation where the (key, value) records are organized using a binary tree,
assuming that keys can be ordered in some way (e.g., numerically or alphabetically).
(Compare Exercise 2.66 of Chapter 2.)
|#

(define (make-tree-table)
  (define (make-tree entry left right) (list entry left right))
  (define (get-entry tree) (car tree))
  (define (get-left-branch tree) (cadr tree))
  (define (get-right-branch tree) (caddr tree))
  (define (get-key record) (car record))
  (define (get-value record) (cdr record))
  (define (make-record key value) (cons key value))
  (define (set-value! record value)
    (set-cdr! record value))
  (define (set-left! tree new-left) (set-car! (cdr tree) new-left))
  (define (set-right! tree new-right) (set-car! (cddr tree) new-right))
  (let ((local-tree (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (get-key (get-entry records))) (get-entry records))
            ((< key (get-key (get-entry records))) (assoc key (get-left-branch records)))
            (else (assoc key (get-right-branch records)))))
    (define (lookup tree key . subkeys)
      (let ((subtree (assoc key (cdr tree))))
        (if subtree
            (cond ((null? subkeys) (get-value subtree))
                  (else (lookup subtree subkeys)))
            false)))
    (define (add-record! tree key value)
        (define (iter record parent set-action)
          (cond ((null? record) (let ((new-entry (make-tree (make-record key value) '() '())))
                                  (begin (set-action parent new-entry) new-entry)))
                ((equal? (get-key (get-entry record)) key) (begin (set-value! (get-entry record) value) (get-entry record)))
                ((< key (get-key (get-entry record))) (iter (get-left-branch record) record set-left!))
                (else (iter (get-right-branch record) record set-right!))))
      (iter (cdr tree) tree set-cdr!))
    (define (insert! tree value key . subkeys)
      (if (null? subkeys)
          (add-record! tree key value)
          (let ((new-subtree (add-record! tree key nil)))
            (insert! new-subtree value subkeys))))
    (define (print) (display local-tree) (newline))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lambda (key . subkeys) (if (null? subkeys)
                                                              (lookup local-tree key)
                                                              (lookup local-tree key subkeys))))
            ((eq? m 'insert-proc) (lambda (value key . subkeys) (if (null? subkeys)
                                                                    (insert! local-tree value key)
                                                                    (insert! local-tree value key subkeys))))
            ((eq? m 'print) (print))
            (else (error "Unknown operation: TREE" m))))
    dispatch))

#|
Exercise 3.27: Memoization (also called tabulation) is a technique that enables a procedure
to record, in a local table, values that have previously been computed. is technique can
make a vast difference in the performance of a program. A memoized procedure maintains a
table in which values of previous calls are stored using as keys the arguments that produced
the values. When the memoized procedure is asked to compute a value, it first checks the
table to see if the value is already there and, if so, just returns that value. Otherwise,
it computes the new value in the ordinary way and stores this in the table. As an example of
memoization, recall from Section 1.2.2 the exponential process for computing Fibonacci
numbers:

(define (fib n)
   (cond ((= n 0) 0)
         ((= n 1) 1)
         (else (+ (fib (- n 1)) (fib (- n 2))))))

The memoized version of the same procedure is

(define memo-fib
   (memoize
      (lambda (n)
         (cond ((= n 0) 0)
               ((= n 1) 1)
               (else (+ (memo-fib (- n 1))
                        (memo-fib (- n 2))))))))

where the memoizer is defined as

(define (memoize f)
   (let ((table (make-table)))
      (lambda (x)
         (let ((previously-computed-result
                  (lookup x table)))
            (or previously-computed-result
                (let ((result (f x)))
                   (insert! x result table)
                   result))))))

Draw an environment diagram to analyze the computation of (memo-fib 3). Explain why memo-fib
computes the nth Fibonacci number in a number of steps proportional to n. Would the scheme
still work if we had simply defined memo-fib to be (memoize fib)?
|#

#|
memo-fib never recomputes the same result twice, as any previously computed result is
obtained from the table to be used for subsequent computations, hence memo-fib computes in
O(n) steps. The scheme will not work if memo-fib was simply defined to be (memoize fib),
since fib will call itself recursively instead of calling memo-fib.
|#

#|
Exercise 3.28: Define an or-gate as a primitive function box. Your or-gate constructor
should be similar to andgate.
|#

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure) ((wire 'add-action!) action-procedure))

(define (make-time-segment time queue) (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time) (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments) (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments) (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments)) action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments (cons (make-new-time-segment time action) (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda (cons (make-new-time-segment time action) segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))
  

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))


(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and a b)
  (cond ((and (= a 1) (= b 1)) 1)
        ((or (= a 0) (= b 0)) 0)
        (else (error "Invalid signal(s)" a b))))

(define (logical-or a b)
  (cond ((or (= a 1) (= b 1)) 1)
        ((and (= a 0) (= b 0)) 0)
        (else (error "Invalid signal(s)" a b))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

#|
Exercise 3.29: Another way to construct an or-gate is as a compound digital logic device,
built from and-gates and inverters. Define a procedure or-gate that accomplishes this.
What is the delay time of the or-gate in terms of andgate-delay and inverter-delay?
|#

(define (alternate-or-gate a1 a2 output)
  (let ((d (make-wire))
        (e (make-wire))
        (f (make-wire)))
    (inverter a1 d)
    (inverter a2 e)
    (and-gate d e f)
    (inverter f output)
    'ok))

#|The delay time of the or-gate is and-gate-delay + 2 * inverter-delay.|#

#|
Exercise 3.30: Figure 3.27 shows a ripple-carry adder formed by stringing together n
full-adders.is is the simplest form of parallel adder for adding two n-bit binary numbers.
The inputs A1, A2, A3, . . ., An and B1, B2, B3, . . ., Bn are the two binary numbers to be
added (each Ak and Bk is a 0 or a 1). e circuit generates S1, S2, S3, . . ., Sn, the n
bits of the sum, and C, the carry from the addition. Write a procedure ripple-carry-adder
that generates this circuit. e procedure should take as arguments three lists of n wires
each — the Ak , the Bk , and the Sk—and also another wire C. The major drawback of the
ripple-carry adder is the need to wait for the carry signals to propagate. What is the delay
needed to obtain the complete output from an n-bit ripplecarry adder, expressed in terms of the delays for and-gates,
or-gates, and inverters?
|#

(define (half-adder a1 a2 s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a1 a2 d)
    (and-gate a1 a2 c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder a-list b-list s-list c)
  (define (ripple-carry-adder-iter as bs ss c-in c-out)
    (if (null? as)
        'ok
        (begin (full-adder (car as) (car bs) c-in (car ss) (if (null? (cdr as))
                                                               c
                                                               c-out))
               (ripple-carry-adder-iter (cdr as) (cdr bs) (cdr ss) c-out (make-wire)))))
  (if (and (= (length a-list) (length b-list))
           (= (length b-list) (length s-list)))
      (let ((init-c-in (make-wire))
            (init-c-out (make-wire)))
        (ripple-carry-adder-iter a-list b-list s-list init-c-in init-c-out))
      (error "Input lengths must be the same size -- RIPPLE-CARRY-ADDER" (length a-list) (length b-list) (length s-list))))

#|
The delay taken in a half-adder is
and-gate-delay + max(or-gate-delay, and-gate-delay + inverter-delay).

The delay taken in a full adder is 2 * half-adder-delay + or-gate-delay.

Therefore, the delay needed to obtain the complete output is
n * full-adder-delay = n * (2 * half-adder-delay + or-gate-delay)
                     = n * (2 * (and-gate-delay + max (or-gate-delay, and-gate-delay + inverter-delay)) + or-gate-delay)
|#

#|Exercise 3.31: The internal procedure accept-action-procedure! defined in make-wire
specifies that when a new action procedure is added to a wire, the procedure is immediately
run. Explain why this initialization is necessary. In particular, trace through the
half-adder example in the paragraphs above and say how the system’s response would differ
if we had defined accept-action-procedure! as

(define (accept-action-procedure! proc)
   (set! action-procedures
   (cons proc action-procedures)))|#


(define (probe-circuit name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

#|
This initialisation is necessary as some wires will not have a signal value of 0 at time 0.
For instance, the output wire of an inverter should start with a signal value of 1, assuming
that the input wire starts with a signal value of 0. Likewise, any devices connected to the
output wire will also be affected as a result. If there is no initialisation, all wires in
the set up will start with a signal value of 0 at time 0, thus any change in signal value
may not propagate down the set up e.g. input wire of inverter changes its signal value to a 1,
output wire will not update as the new value of its signal is 0, and it started with a signal
value of 0, thus any devices connected to the output wire will not update as well.

In the case of the half-adder, with wires A, B, C, D, E, S all with a signal value of 0 at
time 0:

If A changes to 1, while B remains 0:
or-gate with A and B as inputs produces 1 as an output value, thus updating D to a signal
value of 1
and-gate with A and B as inputs produces 0 as an output value, C does not update as new
value of 0 is the same as the previous value of 0
inverter with C as input does not update as C does not update, E remains at 0 as a result
instead of 1 had it been initialised
and-gate with D and E as inputs produces 0 as an output value, thus S has an output value
of 0 instead of 1
|#

#|
Exercise 3.32: The procedures to be run during each time segment of the agenda are kept in
a queue. Thus, the procedures for each segment are called in the order in which they were
added to the agenda (first in, first out). Explain why this order must be used. In
particular, trace the behavior of an and-gate whose inputs change from 0, 1 to 1, 0.
in the same segment and say how the behavior would differ if we stored a segment’s
procedures in an ordinary list, adding and removing procedures only at the front (last in,
first out).
|#

#|
Let the inputs be a1 and a2. Assume that LIFO is used instead of FIFO.

At t = 0, the queue is empty.
a1 changes to 1:
and-action-procedure is called for a1. Since at this point, a1 and a2 are both 1, the
new-value of the output is 1. As such, (after-delay and-gate-delay (lambda ()
(set-signal! output 1))) is called. This adds (set-signal! output 1) to the agenda
at time segment t = 3.

a2 changes to 0:
and-action-procedure is called for a2. Since at this point, a1 is 1 and a2 is 0, the
new-value of the output is 0. As such, (after-delay and-gate-delay (lambda ()
(set-signal! output 0))) is called. This adds (set-signal! output 0) to the agenda
at time segment t = 3.

At t = 3, assuming LIFO instead of FIFO,
(set-signal! output 0) will be called first. Since the signal value of the output was 0 at
first, there is no change. Next, (set-signal! output 1) will be called. Since the signal value
of the output was 0, it is now changed to 1. Thus the signal value of the output at time
t = 3 is 1 when the inputs are (1, 0), which is incorrect. This is a problem as the
output-events that were based on old input-events are executed, without reflecting the
changed input-events. On the other hand, in a FIFO system, output events are executed based
on the order of the input-events, thus the latest output-event will reflect the latest input-
event.
|#

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2)) (set-value! sum (+ (get-value a1) (get-value a2)) me))
          ((and (has-value? a1) (has-value? sum)) (set-value! a2 (- (get-value sum) (get-value a1)) me))
          ((and (has-value? a2) (has-value? sum)) (set-value! a1 (- (get-value sum) (get-value a2)) me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product (* (get-value m1) (get-value m2)) me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2 (/ (get-value product) (get-value m1)) me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1 (/ (get-value product) (get-value m2)) me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe-connector name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false)
        (informant false)
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter inform-about-value constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor inform-about-no-value constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?) (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: CONNECTOR" request))))
    me))

#|
Exercise 3.33: Using primitive multiplier, adder, and constant constraints, define a
procedure averager that takes three connectors a, b, and c as inputs and establishes the
constraint that the value of c is the average of the values of a and b.
|#

(define (averager a b c)
  (let ((total (make-connector))
        (const (make-connector)))
    (adder a b total)
    (multiplier c const total)
    (constant 2 const)
    'ok))

#|
Exercise 3.34: Louis Reasoner wants to build a squarer, a constraint device with two
terminals such that the value of connector b on the second terminal will always be the
square of the value a on the first terminal. He proposes the following simple device made
from a multiplier:

(define (squarer a b)
(multiplier a a b))

There is a serious flaw in this idea. Explain.
|#

#|
If b is set, a cannot be set, as multiplier requires one of the multiplicands to already
have a value along with the product in order to calculate the other multiplicand i.e.
multiplier is not aware that the two multiplicand inputs are the same. In addition, assuming
there is some way to set only m1 or m2, then squarer will calculate the other multiplicand
wrongly, as it is calculated as b/m1 in the case of m2 or b/m2 in the case of m1, instead
of assigning the same value as the other multiplicand. This will give the wrong result if
either the value of a or b is changed.
|#

#|
Exercise 3.35: Ben Bitdiddle tells Louis that one way to avoid the trouble in Exercise 3.34
is to define a squarer as a new primitive constraint. Fill in the missing portions in Ben’s
outline for a procedure to implement such a constraint:
|#

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (let ((square (lambda (x) (* x x))))
              (set-value! b (square (get-value a)) me)))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: MULTIPLIER" request))))
  (connect a me)
  (connect b me)
  me)

#|
Exercise 3.36: Suppose we evaluate the following sequence of expressions in the global
environment:

(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)

At some time during evaluation of the set-value!, the following expression from the
connector’s local procedure is evaluated:

(for-each-except
   setter inform-about-value constraints)

Draw an environment diagram showing the environment in which the above expression is
evaluated.
|#

#|
(define a (make-connector))
(define b (make-connector))

Global environment:
other variables

make-connector
- body: (let ((value .... me)
- points to global environment

has-value?
- variable: connector
- body: (connector 'has-value?)
- points to global environment

get-value?
- variable: connector
- body: (connector 'value)
- points to global environment

set-value!
- variables: connector, new-value, informant
- body: ((connector 'set-value!) new-value informant)
- points to global environment

forget-value!
- variables: connector, retractor
- body: ((connector 'forget) retractor)
- points to global environment

connect
- variables: connector, new-constraint
- body: ((connector 'connect) new-constraint)
- points to global environment

a
- (let ((value ... me)
- points to E1 (me)

b
- (let ((value ... me)
- points to E2 (me)



E1 (define a (make-connector)):
value: false
informant: false
constraints: '()
set-my-value:

- variables: newval, setter
- body: (cond ((not (has-value? ... (else 'ignored))
- points to E1

forget-my-value:
- variable: retractor
- body: (if (eq? retractor ... ignored)
- points to E1

connect:
- variable: new-constraint
- body: (if (not (memq ... 'done)
- points to E1

me:
- variable: request
- body: (cond ((eq? request ... (error "Unknown operation: CONNECTOR" request)))
- points to E1

points to global environment



E2 (define b (make-connector)):
value: false
informant: false
constraints: '()
set-my-value:

- variables: newval, setter
- body: (cond ((not (has-value? ... (else 'ignored))
- points to E2

forget-my-value:
- variable: retractor
- body: (if (eq? retractor ... ignored)
- points to E2

connect:
- variable: new-constraint
- body: (if (not (memq ... 'done)
- points to E2

me:
- variable: request
- body: (cond ((eq? request ... (error "Unknown operation: CONNECTOR" request)))
- points to E2

points to global environment



(set-value! a 10 'user)

Global environment:
other variables

make-connector
- body: (let ((value .... me)
- points to global environment

has-value?
- variable: connector
- body: (connector 'has-value?)
- points to global environment

get-value?
- variable: connector
- body: (connector 'value)
- points to global environment

set-value!
- variables: connector, new-value, informant
- body: ((connector 'set-value!) new-value informant)
- points to global environment

forget-value!
- variables: connector, retractor
- body: ((connector 'forget) retractor)
- points to global environment

connect
- variables: connector, new-constraint
- body: ((connector 'connect) new-constraint)
- points to global environment

a
- (let ((value ... me)
- points to E1 (me)

b
- (let ((value ... me)
- points to E2 (me)



E1 (define a (make-connector)):
value: 10 (after calling (set! value 10))
informant: 'user (after calling (set! informant 'user))
constraints: '()
set-my-value:

- variables: newval, setter
- body: (cond ((not (has-value? ... (else 'ignored))
- points to E1

forget-my-value:
- variable: retractor
- body: (if (eq? retractor ... ignored)
- points to E1

connect:
- variable: new-constraint
- body: (if (not (memq ... 'done)
- points to E1

me:
- variable: request
- body: (cond ((eq? request ... (error "Unknown operation: CONNECTOR" request)))
- points to E1

points to global environment



E2 (define b (make-connector)):
value: false
informant: false
constraints: '()
set-my-value:

- variables: newval, setter
- body: (cond ((not (has-value? ... (else 'ignored))
- points to E2

forget-my-value:
- variable: retractor
- body: (if (eq? retractor ... ignored)
- points to E2

connect:
- variable: new-constraint
- body: (if (not (memq ... 'done)
- points to E2

me:
- variable: request
- body: (cond ((eq? request ... (error "Unknown operation: CONNECTOR" request)))
- points to E2

points to global environment

E3 (set-value! a 10 'user):
- connector: a
- new-value: 10
- informant: 'user
- points to global environment
((a 'set-value!) 10 'user)

E4 (a 'set-value!):
- request: 'set-value!
- points to E1
set-my-value!

E5 (set-my-value! 10 'user)
- newval: 10
- setter: 'user
- points to E1
(cond ((not (has-value? me)) ... (else 'ignored))

E6 (has-value? me)
- connector: me
- points to global environment
(me 'has-value?)

E7 (me 'has-value?)
- request: 'has-value?
- points to E1
(if informant true false)

E8 (for-each-except 'user inform-about-value '())
- exception: 'user
- procedure: inform-about-value
- list: '()
- points to global environment
(define (loop items) ... (loop list)
|#

#|
Exercise 3.37: e celsius-fahrenheit-converter procedure is cumbersome when compared with
a more expressionoriented style of definition, such as

(define (celsius-fahrenheit-converter x)
   (c+ (c* (c/ (cv 9) (cv 5))
             x)
        (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

Here c+, c*, etc. are the “constraint” versions of the arithmetic operations. For example,
c+ takes two connectors as arguments and returns a connector that is related to these
by an adder constraint:
|#

(define (c+ x y)
   (let ((z (make-connector)))
      (adder x y z)
      z))

#|
Define analogous procedures c-, c*, c/, and cv (constant value) that enable us to define
compound constraints as in the converter example above.
|#

(define (c- z x)
  (let ((y (make-connector)))
    (adder x y z)
    y))

(define (c* m1 m2)
  (let ((product (make-connector)))
    (multiplier m1 m2 product)
    product))

(define (c/ product m1)
  (let ((m2 (make-connector)))
    (multiplier m1 m2 product)
    m2))

(define (cv value)
  (let ((x (make-connector)))
    (constant value x)
    x))