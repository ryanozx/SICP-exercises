#lang sicp

#|Exercise 2.53: What would the interpreter print in response
to evaluating each of the following expressions?

(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))
|#

#|
(a b c)
((george))
((y1 y2))
(y1 y2)
#f
#f
(red shoes blue socks)
|#

#|Exercise 2.54: Two lists are said to be equal? if they contain
equal elements arranged in the same order. For example,

(equal? '(this is a list) '(this is a list))

is true, but

(equal? '(this is a list) '(this (is a) list))

is false. To be more precise, we can define equal? recursively in
terms of the basic eq? equality of symbols by saying that a and b
are equal? if they are both symbols and the symbols are eq?, or if
they are both lists such that (car a) is equal? to (car b) and
(cdr a) is equal? to (cdr b). Using this idea, implement equal?
as a procedure.|#

(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((or (null? a) (null? b)) #f)
        ((and (number? a) (number? b)) (= a b))
        ((and (list? a) (list? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        (#t (eq? a b))))

#|Exercise 2.55: Eva Lu Ator types to the interpreter the expression

(car ''abracadabra)

To her surprise, the interpreter prints back quote. Explain.
|#

#|
The interpreter interprets the expression as (car (quote (quote abracadabra))).
which is equivalent to (car (list 'quote (list 'quote 'abracadabra))). As such.
the interpreter returns quote.
|#

(define (variable? e) (symbol? e))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
         
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (new-augend exp) var)))
        ((product? exp) (make-sum
                         (make-product (multiplier exp)
                                       (deriv (new-multiplicand exp) var))
                         (make-product (new-multiplicand exp)
                                       (deriv (multiplier exp) var))))
        ((exponentiation? exp) (make-product
                                (exponent exp)
                                (make-product
                                 (make-exponentiation (base exp)
                                                      (make-sum (exponent exp) -1))
                                 (deriv (base exp) var))))
        (else (error "unknown expression type: DERIV" exp))))

#|Exercise 2.56: Show how to extend the basic differentiator
to handle more kinds of expressions. For instance, implement
the differentiation rule

d(u^n)/dx = n*u^(n-1) du/dx

by adding a new clause to the deriv program and defining
appropriate procedures exponentiation?, base, exponent,
and make-exponentiation. (You may use the symbol **
to denote exponentiation.) Build in the rules that anything
raised to the power 0 is 1 and anything raised to the power
1 is the thing itself.
|#

(define (square x) (* x x))
(define (pow base exp)
  (define (helper b e acc)
    (cond ((= e 0) acc)
          ((even? e) (helper (square b) (/ e 2) acc))
          (else (helper b (- e 1) (* acc b)))))
  (helper base exp 1))

(define (exponentiation? e) (and (pair? e) (eq? (car e) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((=number? base 1) 1)
        ((and (number? base) (number? exp)) (pow base exp))
        (else (list '** base exp))))

#|Exercise 2.57: Extend the differentiation program to handle sums
and products of arbitrary numbers of (two or more)
terms. Then the last example above could be expressed as

(deriv '(* x y (+ x 3)) 'x)

Try to do this by changing only the representation for sums
and products, without changing the deriv procedure at all.
For example, the addend of a sum would be the first term,
and the augend would be the sum of the rest of the terms.|#

(define (new-augend s) (if (null? (cdddr s))
                           (caddr s)
                           (cons '+ (cddr s))))

(define (new-multiplicand p) (if (null? (cdddr p))
                                 (caddr p)
                                 (cons '* (cddr p))))

#|Exercise 2.58: Suppose we want to modify the differentiation
program so that it works with ordinary mathematical
notation, in which + and * are infix rather than prefix operators.
Since the differentiation program is defined in terms of
abstract data, we can modify it to work with different
representations of expressions solely by changing the predicates,
selectors, and constructors that define the representation of
the algebraic expressions on which the differentiator is to
operate.

a. Show how to do this in order to differentiate algebraic
expressions presented in infix form, such as (x + (3
* (x + (y + 2)))). To simplify the task, assume that
+ and * always take two arguments and that expressions are
fully parenthesized.|#

(define (prefix-make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
(define (prefix-make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
(define (prefix-make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((=number? base 1) 1)
        ((and (number? base) (number? exp)) (pow base exp))
        (else (list base '** exp))))
(define (prefix-sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (prefix-addend s) (car s))
(define (prefix-augend s) (caddr s))
(define (prefix-product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (prefix-multiplier p) (car p))
(define (prefix-multiplicand p) (caddr p))
(define (prefix-exponentiation? e) (and (pair? e) (eq? (cadr e) '**)))
(define (prefix-base e) (car e))
(define (prefix-exponent e) (caddr e))
         
(define (prefix-deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((prefix-sum? exp) (prefix-make-sum (prefix-deriv (prefix-addend exp) var)
                                            (prefix-deriv (prefix-augend exp) var)))
        ((prefix-product? exp) (prefix-make-sum
                                (prefix-make-product (prefix-multiplier exp)
                                                     (prefix-deriv (prefix-multiplicand exp) var))
                                (prefix-make-product (prefix-multiplicand exp)
                                                     (prefix-deriv (prefix-multiplier exp) var))))
        ((prefix-exponentiation? exp) (prefix-make-product
                                       (prefix-exponent exp)
                                       (prefix-make-product
                                        (prefix-make-exponentiation (prefix-base exp)
                                                                    (prefix-make-sum (prefix-exponent exp) -1))
                                        (prefix-deriv (prefix-base exp) var))))
        (else (error "unknown expression type: DERIV" exp))))

#|b. The problem becomes substantially harder if we allow
standard algebraic notation, such as (x + 3 * (x +
y + 2)), which drops unnecessary parentheses and
assumes that multiplication is done before addition.
Can you design appropriate predicates, selectors, and
constructors for this notation such that our derivative
program still works?|#

(define (op-value op)
  (cond ((eq? op '+) 1)
        ((eq? op '*) 2)
        ((eq? op '**) 3)
        (else 0)))

(define (is-op? op)
  (or (eq? op '+) (eq? op '*) (eq? op '**)))

(define (commutative? op)
  (cond ((eq? op '+) #t)
        ((eq? op '*) #t)
        ((eq? op '**) #f)
        (else #f)))
  
(define (preproc exp)
  (define (fill-res op-queue elem-queue e res)
    (if (null? res)
        (shunting-yard (cdr op-queue) (cddr elem-queue) e
                       (list (shunting-yard nil nil (cadr elem-queue) nil)
                             (car op-queue)
                             (shunting-yard nil nil (car elem-queue) nil)))
        (shunting-yard (cdr op-queue) (cdr elem-queue) e
                       (list (shunting-yard nil nil (car elem-queue) nil)
                             (car op-queue)
                             res))))        
  (define (shunting-yard op-queue elem-queue e res)
    (cond ((and (null? op-queue) (null? elem-queue) (null? e)) res)
          ((and (null? op-queue) (null? elem-queue) (not (pair? e))) e)
          ((null? e) (fill-res op-queue elem-queue e res))
          ((is-op? (car e)) (let ((new-op (car e)))
                                (cond ((or (null? op-queue) (> (op-value new-op) (op-value (car op-queue))))
                                       (shunting-yard (cons new-op op-queue) elem-queue (cdr e) res))
                                      ((< (op-value new-op) (op-value (car op-queue)))
                                       (fill-res op-queue elem-queue e res))
                                      (else (shunting-yard (cons new-op op-queue) elem-queue (cdr e) res)))))
          (else (shunting-yard op-queue (cons (car e) elem-queue) (cdr e) res))))
  (shunting-yard nil nil exp nil))

(define (hard-deriv exp var)
  (prefix-deriv (preproc exp) var))

#|Exercise 2.59: Implement the union-set operation for the
unordered-list representation of sets.|#

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
  (cond ((null? set2) set1)
        ((null? set1) set2)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

#|Exercise 2.60: We specified that a set would be represented
as a list with no duplicates. Now suppose we allow duplicates.
For instance, the set {1, 2, 3} could be represented as
the list (2 3 2 1 3 2 2). Design procedures elementof-set?,
adjoin-set, union-set, and intersection-set that operate on this
representation. How does the efficiency of each compare with the
corresponding procedure for the non-duplicate representation?
Are there applications for which you would use this representation
in preference to the nonduplicate one?
|#

(define (dupl-element-of-set? set x)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (dupl-element-of-set? (cdr set) x))))
(define (dupl-adjoin-set set x) (cons x set))
(define (dupl-union-set set1 set2) (append set1 set2))
(define (dupl-intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) nil)
        ((dupl-element-of-set? (car set1) set2) (cons (car set1) (dupl-intersection-set (cdr set1) set2)))
        (else (dupl-intersection-set (cdr set1) (set2)))))

#|adjoin-set is now a O(1) operation instead of an O(n) operation,
while union-set is now an O(n) operation instead of a O(n^2) operation.
On the other hand, element-of-set remains an O(n) operation while
intersection-set remains an O(n^2) operation. This representation
might be preferred in cases where it is only needed to know if
a particular element exists in a set, and not how many of them
exist in the set.|#

#|Exercise 2.61: Give an implementation of adjoin-set using the
ordered representation. By analogy with elementof-set? show how
to take advantage of the ordering to produce a procedure that
requires on the average about half as many steps as with the
unordered representation.
|#

(define (ordered-adjoin-set set x)
  (cond ((null? set) (list x))
        ((< (car set) x) (cons (car set) (ordered-adjoin-set (cdr set) x)))
        ((= (car set) x) set)
        ((> (car set) x) (cons x set))))

#|In the worst case, x is larger than all other items in the set, so
the number of steps is the same as for unordered representation. On
the other hand, in the best case scenario, x is smaller than all other
items in the set, so the minimum number of steps is 1. Using this logic,
we should expect on average to iterate through half the items in the set,
thus the procedure will require on the average about half as many steps as
with the unordered representation.|#

#|Exercise 2.62: Give a Θ(n) implementation of union-set
for sets represented as ordered lists.
|#

(define (ordered-union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) nil)
        ((null? set1) (list set2))
        ((null? set2) (list set1))
        ((< (car set1) (car set2)) (cons (car set1) (ordered-union-set (cdr set1) set2)))
        ((< (car set2) (car set1)) (cons (car set2) (ordered-union-set set1 (cdr set2))))
        (else (cons (car set1) (ordered-union-set (cdr set1) (cdr set2))))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

#|Exercise 2.63: Each of the following two procedures converts a
binary tree to a list.|#

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

#|a. Do the two procedures produce the same result for
every tree? If not, how do the results differ? What lists
do the two procedures produce for the trees in Figure
2.16?
b. Do the two procedures have the same order of growth
in the number of steps required to convert a balanced
tree with n elements to a list? If not, which one grows
more slowly?
|#

#|
a. Both procedures produce the same result for every tree.

They all produce (list 1 3 5 7 9 11)

b. The two procedures have different orders of growth in the
number of steps required to convert a balanced tree with n
elements to a list. tree->list-1 grows slower as it uses append.
which results in a O(n log n) operation - log n due to the tree
traversal, and n as append is a linear operation. On the other
hand, tree->list-2 uses cons, which is a constant time operation,
thus it is a O(n) operation.
|#

#|
Exercise 2.64: The following procedure list->tree converts an ordered list
to a balanced binary tree. The helper procedure partial-tree takes as
arguments an integer n and list of at least n elements and constructs a balanced
tree containing the first n elements of the list. The result returned
by partial-tree is a pair (formed with cons) whose car is the constructed
tree and whose cdr is the list of elements not included in the tree.|#

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

#|a. Write a short paragraph explaining as clearly as you
can how partial-tree works. Draw the tree produced
by list->tree for the list (1 3 5 7 9 11).
b. What is the order of growth in the number of steps
required by list->tree to convert a list of n elements?
|#

#|
a. partial-tree takes in a list of elements, as well as
the number of elements that are to form the tree, and returns
a pair, which has a car that is the constructed tree and a cdr
that is a list of elements that are not part of the tree. When n
is 0, it returns a pair consisting of the empty list and all elements
that it was called with. If n is not 0, it recursively calls partial-tree
on the first (quotient of (n - 1) / 2) elements to form the left tree, then
using the ((quotient of (n - 1) / 2) + 1) element as the node of the tree,
then recursively calling partial-tree on the next (n - left-size - 1) elements
to form the right tree, with any remaining elements going into the remaining-elts list.
partial-tree then forms a tree by calling make-tree with left-tree, this-entry, and
right-tree, then returns a pair consisting of this new tree and remaining-elts.

The tree produced is

        5
        |
  -------------
  |           |
  1           9
  |           |    
  ----     --------
     |     |      |
     3     7      11

b. The order of growth is O(n).
|#

#|Exercise 2.65: Use the results of Exercise 2.63 and Exercise 2.64
to give Θ(n) implementations of union-set and intersection-set for
sets implemented as (balanced) binary trees.|#

(define (binary-union-set set1 set2)
  (list->tree (ordered-union-set (tree->list-2 set1) (tree->list-2 set2))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (binary-intersection-set set1 set2)
  (list->tree (intersection-set (tree->list-2 set1) (tree->list-2 set2))))

#|Exercise 2.66: Implement the lookup procedure for the case
where the set of records is structured as a binary tree, ordered by the
numerical values of the keys.|#

(define (lookup given-key record-tree)
  (if (null? record-tree)
      false
      (let ((cur-entry (entry record-tree)))
        (cond ((equal? given-key entry) entry)
              ((< given-key entry) (lookup given-key (left-branch record-tree)))
              ((> given-key entry) (lookup given-key (right-branch record-tree)))))))



(define (hm-make-leaf symbol weight) (list 'leaf symbol weight))
(define (hm-leaf? object) (eq? (car object) 'leaf))
(define (hm-symbol-leaf x) (cadr x))
(define (hm-weight-leaf x) (caddr x))

(define (hm-make-code-tree left right)
  (list left
        right
        (append (hm-symbols left) (hm-symbols right))
        (+ (hm-weight left) (hm-weight right))))

(define (hm-left-branch tree) (car tree))
(define (hm-right-branch tree) (cadr tree))
(define (hm-symbols tree)
  (if (hm-leaf? tree)
      (list (hm-symbol-leaf tree))
      (caddr tree)))
(define (hm-weight tree)
  (if (hm-leaf? tree)
      (hm-weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (hm-leaf? next-branch)
              (cons (hm-symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (hm-left-branch branch))
        ((= bit 1) (hm-right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (hm-adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (hm-weight x) (hm-weight (car set))) (cons x set))
        (else (cons (car set)
                    (hm-adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (hm-adjoin-set (hm-make-leaf (car pair)
                                     (cadr pair))
                       (make-leaf-set (cdr pairs))))))

#|Exercise 2.67: Define an encoding tree and a sample message:|#

(define sample-tree
  (hm-make-code-tree (hm-make-leaf 'A 4)
                     (hm-make-code-tree
                      (hm-make-leaf 'B 2)
                      (hm-make-code-tree
                       (hm-make-leaf 'D 1)
                       (hm-make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

#|Use the decode procedure to decode the message, and give
the result.|#

(decode sample-message sample-tree) ; (A D A B B C A)

#|Exercise 2.68: The encode procedure takes as arguments a
message and a tree and produces the list of bits that gives
the encoded message.|#

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

#|encode-symbol is a procedure, which you must write, that
returns the list of bits that encodes a given symbol according to
a given tree. You should design encode-symbol so
that it signals an error if the symbol is not in the tree at all.
Test your procedure by encoding the result you obtained in
Exercise 2.67 with the sample tree and seeing whether it is
the same as the original sample message.
|#

(define (encode-symbol symbol tree)
  (cond ((hm-leaf? tree)
         nil)
        ((element-of-set? symbol (hm-symbols (hm-left-branch tree)))
         (cons 0 (encode-symbol symbol (hm-left-branch tree))))
        ((element-of-set? symbol (hm-symbols (hm-right-branch tree)))
         (cons 1 (encode-symbol symbol (hm-right-branch tree))))
        (else (error "Symbol not in tree: " symbol))))

#|Exercise 2.69: The following procedure takes as its argument
a list of symbol-frequency pairs (where no symbol
appears in more than one pair) and generates a Huffman
encoding tree according to the Huffman algorithm.|#

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

#|make-leaf-set is the procedure given above that transforms the
list of pairs into an ordered set of leaves. successivemerge is
the procedure you must write, using make-codetree to successively
merge the smallest-weight elements of the set until there is only
one element left, which is the desired Huffman tree. (This procedure
is slightly tricky, but not really complicated. If you find yourself
designing a complex procedure, then you are almost certainly doing some
thing wrong. You can take significant advantage of the fact
that we are using an ordered set representation.)|#

(define (successive-merge set)
  (cond ((null? (cdr set)) (car set))
        (else (successive-merge (hm-adjoin-set (hm-make-code-tree (car set) (cadr set)) (cddr set))))))

#|
Exercise 2.70: The following eight-symbol alphabet with
associated relative frequencies was designed to efficiently
encode the lyrics of 1950s rock songs. (Note that the “symbols”
of an “alphabet” need not be individual letters.)

A 2 GET 2 SHA 3 WAH 1
BOOM 1 JOB 2 NA 16 YIP 9

Use generate-huffman-tree (Exercise 2.69) to generate a
corresponding Huffman tree, and use encode (Exercise 2.68)
to encode the following message:

Get a job
Sha na na na na na na na na
Get a job
Sha na na na na na na na na
Wah yip yip yip yip yip yip yip yip yip
Sha boom

How many bits are required for the encoding? What is the
smallest number of bits that would be needed to encode this
song if we used a fixed-length code for the eight-symbol
alphabet?|#

(define song-tree (generate-huffman-tree (list (list 'A 2)
                                               (list 'GET 2)
                                               (list 'SHA 3)
                                               (list 'WAH 1)
                                               (list 'BOOM 1)
                                               (list 'JOB 2)
                                               (list 'NA 16)
                                               (list 'YIP 9))))

(define song '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(encode song song-tree)

#|(0 1 1 1 1 1 0 1 1 0 0 0 1 1 1 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 1 1 1 1 1 0 1 1 0 0 0 1 1 1 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 1 1 0 1 1 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1
   0 0 1 0 0 1 0 0 1 1 1 0 0 1 1 0 1 0)|#
(length (encode song song-tree)) ; 84 bits

#|With 8 symbols in the alphabet, 3 bits would be needed to encode each
symbol. Since there are 36 symbols in the song, 108 bits would be needed|#

#|Exercise 2.71: Suppose we have a Huffman tree for an alphabet of n symbols,
and that the relative frequencies of the symbols are 1, 2, 4, . . . , 2^(n-1).
Sketch the tree for n = 5; for n = 10. In such a tree (for general n) how many bits
are required to encode the most frequent symbol? The least
frequent symbol?|#

#|
n = 5:

(1 2 4 8 16) -> 16
             -> (1 2 4 8) -> 8
                          -> (1 2 4) -> 4
                                     -> (1 2) -> 2
                                              -> 1

n = 10:

(1 2 4 8 16 32 64 128 256 512) -> 512
                               -> (1 2 4 8 16 32 64 128 256) -> 256
                                                             -> (1 2 4 8 16 32 64 128) (cont')


(1 2 4 8 16 32 64 128) -> 128
                       -> (1 2 4 8 16 32 64) -> 64
                                             -> (1 2 4 8 16 32) -> 32
                                                                -> (1 2 4 8 16) (see above)

1 bit is required to encode the most frequent symbol. (n-1) bits are required to encode
the least frequent symbol.
|#

#|Exercise 2.72: Consider the encoding procedure that you
designed in Exercise 2.68. What is the order of growth in
the number of steps needed to encode a symbol? Be sure
to include the number of steps needed to search the symbol list
at each node encountered. To answer this question
in general is difficult. Consider the special case where the
relative frequencies of the n symbols are as described in Exercise 2.71,
and give the order of growth (as a function of n)
of the number of steps needed to encode the most frequent
and least frequent symbols in the alphabet.|#

#|
Considering the special case,
the order of growth of the number of steps needed to encode the most
frequent symbol is O(n).

The order of growth of the number of steps needed to encode the least
frequent symbol is O(n^2).
|#
