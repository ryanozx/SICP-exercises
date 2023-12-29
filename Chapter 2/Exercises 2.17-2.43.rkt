#lang sicp

#|Exercise 2.17: Define a procedure last-pair that returns
the list that contains only the last element of a given (nonempty)
list|#

(define (last-pair ls)
  (cond ((or (not (list? ls)) (null? ls)) (error "Non-empty list required as argument"))
        ((null? (cdr ls)) ls)
        (else (last-pair (cdr ls)))))

#|Exercise 2.18: Define a procedure reverse that takes a list
as argument and returns a list of the same elements in reverse order:|#

(define (reverse ls)
  (define (helper acc xs)
    (if (or (null? xs) (not (list? xs)))
        acc
        (helper (cons (car xs) acc) (cdr xs))))
  (helper nil ls))

#|Exercise 2.19: Consider the change-counting program of
Section 1.2.2. It would be nice to be able to easily change the
currency used by the program, so that we could compute
the number of ways to change a British pound, for example.
As the program is written, the knowledge of the currency is
distributed partly into the procedure first-denomination
and partly into the procedure count-change (which knows
that there are five kinds of U.S. coins). It would be nicer
to be able to supply a list of coins to be used for making
change.

We want to rewrite the procedure cc so that its second argument
is a list of the values of the coins to use rather than
an integer specifying which coins to use. We could then
have lists that defined each kind of currency:|#

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

#|We could then call cc as follows:

(cc 100 us-coins)
292

To do this will require changing the program cc somewhat.
It will still have the same form, but it will access its second
argument differently, as follows:|#

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

#|Define the procedures first-denomination, except-firstdenomination,
and no-more? in terms of primitive operations on list structures. Does
the order of the list coinvalues affect the answer produced by cc? Why
or why not?
|#

(define (first-denomination coin-values) (car coin-values))
(define (except-first-denomination coin-values) (cdr coin-values))
(define (no-more? coin-values) (null? coin-values))

#|The order of the list does not affect the result. All combinations
of coins are calculated.|#

#|Exercise 2.20: The procedures +, *, and list take arbitrary
numbers of arguments. One way to define such procedures
is to use define with dotted-tail notation. In a procedure
definition, a parameter list that has a dot before the last
parameter name indicates that, when the procedure is called,
the initial parameters (if any) will have as values the initial
arguments, as usual, but the final parameter’s value will be
a list of any remaining arguments. For instance, given the
definition

(define (f x y . z) ⟨body⟩)

the procedure f can be called with two or more arguments.
If we evaluate

(f 1 2 3 4 5 6)

then in the body of f, x will be 1, y will be 2, and z will be
the list (3 4 5 6). Given the definition

(define (g . w) ⟨body⟩)

the procedure g can be called with zero or more arguments.
If we evaluate

(g 1 2 3 4 5 6)

then in the body of g, w will be the list (1 2 3 4 5 6).

Use this notation to write a procedure same-parity that
takes one or more integers and returns a list of all the
arguments that have the same even-odd parity as the first
argument. For example,

(same-parity 1 2 3 4 5 6 7)
(1 3 5 7)
(same-parity 2 3 4 5 6 7)
(2 4 6)
|#

(define (same-parity first . ls)
  (let ((parity (remainder first 2)))
    (define (parity-rec xs)
      (cond ((null? xs) nil)
            ((= (remainder (car xs) 2) parity) (cons (car xs) (parity-rec (cdr xs))))
            (else (parity-rec (cdr xs)))))
    (cons first (parity-rec ls))))

#|Exercise 2.21: e procedure square-list takes a list of
numbers as argument and returns a list of the squares of
those numbers.

(square-list (list 1 2 3 4))
(1 4 9 16)

Here are two different definitions of square-list. Complete
both of them by filling in the missing expressions:

(define (square-list items)
(if (null? items)
nil
(cons ⟨??⟩ ⟨??⟩)))
(define (square-list items)
(map ⟨??⟩ ⟨??⟩))|#

(define (square x) (* x x))

(define (square-list-1 items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))

#|Exercise 2.22: Louis Reasoner tries to rewrite the first square
list procedure of Exercise 2.21 so that it evolves an iterative process:

(define (square-list items)
(define (iter things answer)
(if (null? things)
answer
(iter (cdr things)
(cons (square (car things))
answer))))
(iter items nil))

Unfortunately, defining square-list this way produces the
answer list in the reverse order of the one desired. Why?

Louis then tries to fix his bug by interchanging the arguments to cons:
(define (square-list items)
(define (iter things answer)
(if (null? things)
answer
(iter (cdr things)
(cons answer
(square (car things))))))
(iter items nil))

This doesn’t work either. Explain.|#

#|
a) As the procedure iterates, each item is added to the front of the list,
thus the first item ends up at the back of the list and the last item ends
up at the front of the list.

b) By interchanging the argument, nested pairs are returned instead of a list,
since every time cons is called, a new pair is formed with the former pair being
the item in the pair and the new item being the second. This does not adhere to the
structure of a list, whereby the first item of a pair is a list and the second item
is a pair, nesting all the way down until the innermost pair, where the last item is
nil.|#

#|Exercise 2.23: The procedure for-each is similar to map. It
takes as arguments a procedure and a list of elements. However,
rather than forming a list of the results, for-each just
applies the procedure to each of the elements in turn, from
left to right. The values returned by applying the procedure
to the elements are not used at all—for-each is used with
procedures that perform an action, such as printing. For example,

(for-each (lambda (x)
(newline)
(display x))
(list 57 321 88))
57
321
88

The value returned by the call to for-each (not illustrated
above) can be something arbitrary, such as true. Give an
implementation of for-each|#

(define (for-each f ls)
  (cond ((null? ls) #t)
        (else (f (car ls))
              (for-each f (cdr ls)))))

#|Exercise 2.24: Suppose we evaluate the expression (list
1 (list 2 (list 3 4))). Give the result printed by the
interpreter, the corresponding box-and-pointer structure,
and the interpretation of this as a tree (as in Figure 2.6).|#

#|
Printed: (1 (2 (3 4)))

Box-and-pointer:
(list 1 (list 2 (list 3 4)))
   |
   |                   (list (list 2 (list 3 4)))
   |                      |
[ ] [ ] -------------> [ ] [/]
 |                      |
 1                      |         (list (list 3 4))
                        |            |
(list 2 (list 3 4) --> [ ] [ ] -> [ ] [/]
                        |          |
                        2          |          (list 4)
                                   |             |
                   (list 3 4) --> [ ] [ ] --> [ ] [/]
                                   |           |
                                   3           4


Tree:
(1 (2 (3 4)))
      |
  ---------
  |       |
  1   (2 (3 4))
          |
      ---------
      |       |
      2     (3 4)
              |
          ---------
          |       |
          3       4
|#

#|Exercise 2.25: Give combinations of cars and cdrs that
will pick 7 from each of the following lists:
(1 3 (5 7) 9)
((7))
(1 (2 (3 (4 (5 (6 7))))))
|#

(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (car (list (list 7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))

#|Exercise 2.26: Suppose we define x and y to be two lists:
(define x (list 1 2 3))
(define y (list 4 5 6))
What result is printed by the interpreter in response to evaluating
each of the following expressions:
(append x y)
(cons x y)
(list x y)|#

#|
(append x y) --> (1 2 3 4 5 6)
(cons x y) --> ((1 2 3) 4 5 6)
(list x y) --> ((1 2 3) (4 5 6))
|#

#|Exercise 2.27: Modify your reverse procedure of Exercise
2.18 to produce a deep-reverse procedure that takes a list
as argument and returns as its value the list with its elements
reversed and with all sublists deep-reversed as well.
For example,

(define x (list (list 1 2) (list 3 4)))
x
((1 2) (3 4))
(reverse x)
((3 4) (1 2))
(deep-reverse x)
((4 3) (2 1))
|#

(define (deep-reverse ls)
  (define (helper acc xs)
    (cond ((null? xs) acc)
          ((pair? (car xs)) (helper (cons (helper nil (car xs)) acc) (cdr xs)))
          (else (helper (cons (car xs) acc) (cdr xs)))))
  (helper nil ls))

#|Exercise 2.28: Write a procedure fringe that takes as argument a tree
(represented as a list) and returns a list whose elements are all the
leaves of the tree arranged in left-toright order. For example,

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(1 2 3 4)
(fringe (list x x))
(1 2 3 4 1 2 3 4)
|#

(define (fringe tree)
  (define (arrange proc stack)
    (cond ((and (null? proc) (null? stack)) nil)
          ((null? proc) (arrange (car stack) (cdr stack)))
          ((list? (car proc)) (arrange (car proc) (cons (cdr proc) stack)))
          (else (cons (car proc) (arrange (cdr proc) stack)))))
  (arrange tree nil))

#|Exercise 2.29: A binary mobile consists of two branches,
a left branch and a right branch. Each branch is a rod of
a certain length, from which hangs either a weight or an
other binary mobile. We can represent a binary mobile using
compound data by constructing it from two branches
(for example, using list):|#

(define (make-mobile left right) (list left right))

#|A branch is constructed from a length (which must be a
number) together with a structure, which may be either a
number (representing a simple weight) or another mobile:|#

(define (make-branch length structure) (list length structure))

#|a. Write the corresponding selectors left-branch and
right-branch, which return the branches of a mobile,
and branch-length and branch-structure, which return the
components of a branch.
b. Using your selectors, define a procedure total-weight
that returns the total weight of a mobile.
c. A mobile is said to be balanced if the torque applied by
its top-left branch is equal to that applied by its top-right
branch (that is, if the length of the left rod multiplied by
the weight hanging from that rod is equal to the corresponding
product for the right side) and if each of the submobiles
hanging off its branches is balanced. Design a predicate that
tests whether a binary mobile is balanced.
d. Suppose we change the representation of mobiles so
that the constructors are
(define (make-mobile left right) (cons left right))
(define (make-branch length structure)
(cons length structure))
How much do you need to change your programs to
convert to the new representation?|#

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))

(define (total-weight mobile)
  (if (list? mobile)
      (+ (total-weight (branch-structure (left-branch mobile))) (total-weight (branch-structure (right-branch mobile))))
      mobile))

(define (balanced? mobile)
  (define (balanced-traversal structure)
    (if (not (list? structure))
        structure
        (let ((left-bal (balanced-traversal (branch-structure (left-branch structure))))
              (right-bal (balanced-traversal (branch-structure (right-branch structure))))
              (left-length (branch-length (left-branch structure)))
              (right-length (branch-length (right-branch structure))))
          (cond ((not (or left-bal right-bal)) #f)
                (else (if (= (* left-bal left-length) (* right-bal right-length))
                          (+ left-bal right-bal)
                          #f))))))
  (if (not (balanced-traversal mobile))
      #f
      #t))

(define (new-left-branch mobile) (car mobile))
(define (new-right-branch mobile) (cdr mobile))
(define (new-branch-length branch) (car branch))
(define (new-branch-structure branch) (cdr branch))

(define (new-total-weight mobile)
  (if (pair? mobile)
      (+ (new-total-weight (new-branch-structure (new-left-branch mobile))) (new-total-weight (new-branch-structure (new-right-branch mobile))))
      mobile))

(define (new-balanced? mobile)
  (define (balanced-traversal structure)
    (if (not (pair? structure))
        structure
        (let ((left-bal (balanced-traversal (new-branch-structure (new-left-branch structure))))
              (right-bal (balanced-traversal (new-branch-structure (new-right-branch structure))))
              (left-length (new-branch-length (new-left-branch structure)))
              (right-length (new-branch-length (new-right-branch structure))))
          (cond ((not (or left-bal right-bal)) #f)
                (else (if (= (* left-bal left-length) (* right-bal right-length))
                          (+ left-bal right-bal)
                          #f))))))
  (if (not (balanced-traversal mobile))
      #f
      #t))

#|Only the selectors need to be changed - the programs will work just as fine without
any changes required, aside from replacing each 'list?' with 'pair?'|#

#|Exercise 2.30: Define a procedure square-tree analogous
to the square-list procedure of Exercise 2.21. That is, square
tree should behave as follows:

(square-tree
(list 1
(list 2 (list 3 4) 5)
(list 6 7)))
(1 (4 (9 16) 25) (36 49))

Define square-tree both directly (i.e., without using any
higher-order procedures) and also by using map and recursion.
|#

(define (square-tree-direct tree)
  (cond ((null? tree) nil)
        ((list? (car tree)) (cons (square-tree-direct (car tree)) (square-tree-direct (cdr tree))))
        (else (cons (square (car tree)) (square-tree-direct (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (list? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))

#|Exercise 2.31: Abstract your answer to Exercise 2.30 to
produce a procedure tree-map with the property that square
tree could be defined as

(define (square-tree tree) (tree-map square tree))|#

(define (tree-map f tree)
  (cond ((null? tree) nil)
        ((list? (car tree)) (cons (tree-map f (car tree)) (tree-map f (cdr tree))))
        (else (cons (f (car tree)) (tree-map f (cdr tree))))))

#|Exercise 2.32: We can represent a set as a list of distinct
elements, and we can represent the set of all subsets of the
set as a list of lists. For example, if the set is (1 2 3), then
the set of all subsets is (() (3) (2) (2 3) (1) (1 3)
(1 2) (1 2 3)). Complete the following definition of a
procedure that generates the set of subsets of a set and give
a clear explanation of why it works:

(define (subsets s)
(if (null? s)
(list nil)
(let ((rest (subsets (cdr s))))
(append rest (map ⟨??⟩ rest)))))
|#

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

#|This subsets procedure works similarly to the procedure to find
all possible combinations of coins. It calls itself recursively,
splitting the set of all subsets into two groups - subsets that
do not have a particular element in them (which it calls itself
recursively on), and subsets that have that particular element in them.
The latter is merely the former with the element appended to them.
As such, it is able to generate the set of subsets of the set.|#

#|Exercise 2.33: Fill in the missing expressions to complete
the following definitions of some basic list-manipulation
operations as accumulations:

(define (map p sequence) (accumulate (lambda (x y) ⟨??⟩) nil sequence))
(define (append seq1 seq2) (accumulate cons ⟨??⟩ ⟨??⟩))
(define (length sequence) (accumulate ⟨??⟩ 0 sequence))|#

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (my-map p sequence) (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (my-append seq1 seq2) (accumulate cons seq2 seq1))
(define (my-length sequence) (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

#|Exercise 2.34: Evaluating a polynomial in x at a given value
of x can be formulated as an accumulation. We evaluate the
polynomial

a_n * x^n + a_(n-1) * x^(n-1) + · · · + a_1 * x + a_0

using a well-known algorithm called Horner’s rule, which
structures the computation as

(. . . (a_n * x + a_(n-1))x + · · · + a1)x + a0.

In other words, we start with a_n, multiply by x, add a_(n-1),
multiply by x, and so on, until we reach a_0.

Fill in the following template to produce a procedure that
evaluates a polynomial using Horner’s rule. Assume that
the coefficients of the polynomial are arranged in a sequence,
from a_0 through a_n.

(define (horner-eval x coefficient-sequence)
(accumulate (lambda (this-coeff higher-terms) ⟨??⟩)
0
coefficient-sequence))|#

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms))) 0 coefficient-sequence))

#|Exercise 2.35: Redefine count-leaves from Section 2.2.2
as an accumulation:

(define (count-leaves t)
(accumulate ⟨??⟩ ⟨??⟩ (map ⟨??⟩ ⟨??⟩)))|#

(define (count-leaves t) (accumulate + 0 (map (lambda (node) (cond ((null? node) 0)
                                                                   ((pair? node) (count-leaves node))
                                                                   (else 1))) t)))

#|Exercise 2.36: The procedure accumulate-n is similar to
accumulate except that it takes as its third argument a
sequence of sequences, which are all assumed to have the
same number of elements. It applies the designated accumulation
procedure to combine all the first elements of the
sequences, all the second elements of the sequences, and so
on, and returns a sequence of the results. For instance, if s
is a sequence containing four sequences, ((1 2 3) (4 5 6)
(7 8 9) (10 11 12)), then the value of (accumulate-n +
0 s) should be the sequence (22 26 30). Fill in the missing
expressions in the following definition of accumulate-n:

(define (accumulate-n op init seqs)
(if (null? (car seqs))
nil
(cons (accumulate op init ⟨??⟩)
(accumulate-n op init ⟨??⟩))))|#

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

#|Exercise 2.37: Suppose we represent vectors v = (v_i) as
sequences of numbers, and matrices m = (m_ij) as sequences
of vectors (the rows of the matrix). For example, the matrix

(1 2 3 4)
(4 5 6 6)
(6 7 8 9)

is represented as the sequence ((1 2 3 4) (4 5 6 6)
(6 7 8 9)). With this representation, we can use sequence
operations to concisely express the basic matrix and vector
operations. These operations (which are described in any
book on matrix algebra) are the following:

(dot-product v w) returns the sum (sum_i v_i * w_i);

(matrix-*-vector m v) returns the vector t,
where t_i = (sum_j m_ij * v_j);

(matrix-*-matrix m n) returns the matrix p,
where p_ij = (sum_k m_ik * n_kj);

(transpose m) returns the matrix n,
where n_ij = m_ji .

We can define the dot product as|#

(define (dot-product v w) (accumulate + 0 (map * v w)))

#|Fill in the missing expressions in the following procedures
for computing the other matrix operations. (e procedure
accumulate-n is defined in Exercise 2.36.)
(define (matrix-*-vector m v)
(map ⟨??⟩ m))
(define (transpose mat)
(accumulate-n ⟨??⟩ ⟨??⟩ mat))
(define (matrix-*-matrix m n)
(let ((cols (transpose n)))
(map ⟨??⟩ m)))
|#

(define (matrix-*-vector m v) (map (lambda (r) (dot-product r v)) m))
(define (transpose mat) (accumulate-n cons nil mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (matrix-*-vector cols r)) m)))

#|Exercise 2.38: The accumulate procedure is also known as
fold-right, because it combines the first element of the
sequence with the result of combining all the elements to the
right. There is also a fold-left, which is similar to foldright,
except that it combines elements working in the opposite direction:|#

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

#|What are the values of
(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))
Give a property that op should satisfy to guarantee that
fold-right and fold-left will produce the same values
for any sequence.
|#

#|
1/6
1/6
(1 (2 (3 ())))
(((() 1) 2) 3)

op needs to satisfy commutativity.
|#

#|Exercise 2.39: Complete the following definitions of reverse
(Exercise 2.18) in terms of fold-right and fold-left from
Exercise 2.38:

(define (reverse sequence)
(fold-right (lambda (x y) ⟨??⟩) nil sequence))
(define (reverse sequence)
(fold-left (lambda (x y) ⟨??⟩) nil sequence))|#

(define (fold-right-reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (fold-left-reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

#|Exercise 2.40: Define a procedure unique-pairs that, given
an integer n, generates the sequence of pairs (i, j) with 1 ≤
j < i ≤ n. Use unique-pairs to simplify the definition of
prime-sum-pairs given above.|#

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq)) (cons (car seq) (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))

(define (prime? x)
  (define (test divisor)
    (cond ((> (square divisor) x) true)
          ((= 0 (remainder x divisor)) false)
          (else (test (+ divisor 1)))))
  (test 2))
(define (prime-sum? pair) (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair) (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

#|Exercise 2.41: Write a procedure to find all ordered triples
of distinct positive integers i, j, and k less than or equal to
a given integer n that sum to a given integer s.
|#
(define (ordered-triples n s)
  (flatmap (lambda (pair) (let ((i (cadr pair))
                                (j (car pair)))
                            (map (lambda (k) (list i j k)) (filter (lambda (k) (and (<= k n) (> k i) (> k j))) (list (- s i j))))))
           (unique-pairs (- n 1))))

#|Exercise 2.42: The "eight-queens puzzle" asks how to place
eight queens on a chessboard so that no queen is in check
from any other (i.e., no two queens are in the same row, column,
or diagonal). One possible solution is shown in Figure
2.8. One way to solve the puzzle is to work across the board,
placing a queen in each column. Once we have placed k -1
queens, we must place the k_th queen in a position where it
does not check any of the queens already on the board. We
can formulate this approach recursively: Assume that we
have already generated the sequence of all possible ways
to place k -1 queens in the first k -1 columns of the board.
For each of these ways, generate an extended set of positions
by placing a queen in each row of the kth column.
Now filter these, keeping only the positions for which the
queen in the kth column is safe with respect to the other
queens. This produces the sequence of all ways to place k
queens in the first k columns. By continuing this process,
we will produce not only one solution, but all solutions to
the puzzle. We implement this solution as a procedure queens, which
returns a sequence of all solutions to the problem of placing
n queens on an n × n chessboard. queens has an internal
procedure queen-cols that returns the sequence of all
ways to place queens in the first k columns of the board.|#

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

#|In this procedure rest-of-queens is a way to place k - 1
queens in the first k-1 columns, and new-row is a proposed
row in which to place the queen for the kth column. Complete
the program by implementing the representation for
sets of board positions, including the procedure adjoinposition,
which adjoins a new row-column position to a set of positions,
and empty-board, which represents an empty set of positions.
You must also write the procedure safe?, which determines
for a set of positions, whether the queen in the k
th column is safe with respect to the others. (Note
that we need only check whether the new queen is safe—
the other queens are already guaranteed safe with respect
to each other.)|#

(define (adjoin-position new-row k rest-of-queens) (cons (list new-row k) rest-of-queens)) 
(define empty-board nil)
(define (safe? k positions)
  (let ((new-row (car (car positions)))
        (new-col k))
    (accumulate (lambda (coordinate rest)
                  (let ((check-row (car coordinate))
                        (check-col (cadr coordinate)))
                    (and rest (not (or (= new-row check-row)
                                       (= (abs (- new-row check-row))
                                          (abs (- new-col check-col))))))))
                #t (cdr positions))))
                                     
#|Exercise 2.43: Louis Reasoner is having a terrible time doing
Exercise 2.42.His queens procedure seems to work, but it runs
extremely slowly. (Louis never does manage to wait long enough
for it to solve even the 6 × 6 case.) When Louis asks Eva Lu Ator
for help, she points out that he has interchanged the order of the
nested mappings in the flatmap, writing it as

(flatmap
(lambda (new-row)
(map (lambda (rest-of-queens)
(adjoin-position new-row k rest-of-queens))
(queen-cols (- k 1))))
(enumerate-interval 1 board-size))

Explain why this interchange makes the program run slowly.
Estimate how long it will take Louis’s program to solve the
eight-queens puzzle, assuming that the program in Exercise
2.42 solves the puzzle in time T |#

#|This interchange makes the program run slower, as every time
a queen is added to the board, the positions of the queens already
on the board is repeatedly generated (board-size) times, thus the number of
calculations taken is exponential as there is tree recursion.

In the original program, (queen-cols k) is called once, which then
calls (queen-cols (k-1)), which then calls (queen-cols (k-2)), and so on
until (queen-cols 0). Within each (queen-cols j) call, assuming the length
of the list returned from the preceding queen-cols call (queen-cols (j-1))
is L_(j-1) and the board size is i, map runs through the list i times,
resulting in iL_(j-1) steps as iL_(j-1) positions are created.

Next, safe? takes (j-1) steps as it runs through the positions of previous
queens for each position, resulting in filter taking a total of i(j-1)L_(j-1) steps.

As such, the number of steps within each queen-cols call, excluding those
taken by preceding queen-cols call, is iL_(j-1) + i(j-1)L_(j-1) = ijL_(j-1).

The upper bound of L_j can be taken as iL_(j-1) i.e. i^j - naively assuming that
all positions are accepted.

T_j = (i^j)j + T_(j-1)

T_8 = 8(i^8) + 7(i^7) + ... + i

With i = 8, T_8 = 150652552 (actual value is much lower, as L_j will certainly
be lower than iL_(j-1).

In Louis' program, within each queen-cols call, the preceding queen-cols is called
i times, resulting in iT_(j-1) steps taken. Next, safe? takes i(j-1)T_(j-1) steps.
Adding them together, each queen-cols call takes ijT_(j-1) steps.

T_0 --> 1 step
T_1 --> 1 * 8 * 1 = 1! * 8 = 8 steps
T_2 --> 2 * 8 * T_1 = 2 * 1 * 8 * 8 = 2! * 8^2 = 128 steps
T_3 --> 3 * 8 * T_2 = 3 * 2 * 1 * 8 * 8 * 8 = 3! * 8^3 = 3072 steps
...
T_j = (j!) * i^j

As such, T_8 takes 8! * 8^8 = 676457349120 steps, which is approximately 5000T (higher
than this since it is guaranteed that T_8 will take this number of steps in Louis' program,
whereas the number of steps taken in the original program will be lower than what was calculated.
|#