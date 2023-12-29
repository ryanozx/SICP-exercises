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

#|
Exercise 2.73: Section 2.3.2 described a program that performs symbolic differentiation:
(define (deriv exp var)
(cond ((number? exp) 0)
((variable? exp)
(if (same-variable? exp var) 1 0))
((sum? exp)
(make-sum (deriv (addend exp) var)
(deriv (augend exp) var)))
((product? exp)
(make-sum (make-product
(multiplier exp)
(deriv (multiplicand exp) var))
(make-product
(deriv (multiplier exp) var)
(multiplicand exp))))
⟨more rules can be added here⟩
(else (error "unknown expression type:
DERIV" exp))))

We can regard this program as performing a dispatch on
the type of the expression to be differentiated. In this
situation the “type tag” of the datum is the algebraic operator
symbol (such as +) and the operation being performed is
deriv. We can transform this program into data-directed
style by rewriting the basic derivative procedure as|#

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
#|a. Explain what was done above. Why can’t we assimilate the
predicates number? and variable? into the
data-directed dispatch?
b. Write the procedures for derivatives of sums and products,
and the auxiliary code required to install them in
the table used by the program above
|#

#|
a. Instead of using several predicates to extract the car of the
expression and compare it against the various symbols that represent
different operations, the operator is instead extracted from the
operation using the operator procedure, while the operands are extracted
from the operation using the operand procedure. The operator procedure
is used to obtain the type tag to use with get in order to retrieve the
appropriate 'deriv procedure to apply. The procedure returned is then applied
on the operands obtained using the operand procedure.

Through this method, the deriv procedure can support differentiation of any number
of different operations without requiring any update to the procedure itself,
as long as the expression is in prefix-notation. To add a new operation, register
the corresponding procedure under the 'deriv operation with the appropriate symbol
corresponding to the type.

Number and variable expressions do not use operators or operands,
thus the procedures in the data-directed dispatch cannot be applied to
them and therefore the predicates have to be tested outside the dispatch.
|#

(define (variable? exp) (not (pair? exp)))
(define (same-variable? exp var)
  (and (variable? exp) (equal? exp var)))
(define (=number? var val) (and (number? var) (= var val)))
(define (make-sum e1 e2)
    (cond ((and (number? e1) (number? e2)) (+ e1 e2))
          ((=number? e1 0) e2)
          ((=number? e2 0) e1)
          (else (list '+ e1 e2))))
(define (make-product e1 e2)
    (cond ((and (number? e1) (number? e2)) (* e1 e2))
          ((or (=number? e1 0) (=number? e2 0)) 0)
          ((=number? e1 1) e2)
          ((=number? e2 1) e1)
          (else (list '* e1 e2))))

(define (install-sum-package)
  (define (addend s) (car s))
  (define (augend s)
    (if (null? (cddr s))
        (cadr s)
        (cons '+ (cdr s))))
  (put 'deriv '+ (lambda (operands var)
                   (make-sum (deriv (addend operands) var)
                             (deriv (augend operands) var)))))

(define (install-mult-package)
  (define (multiplier p) (car p))
  (define (multiplicand p) (if (null? (cddr p))
                               (cadr p)
                               (cons '* (cdr p))))
  (put 'deriv '* (lambda (operands var)
                   (make-sum (make-product
                              (multiplier operands)
                              (deriv (multiplicand operands) var))
                             (make-product
                              (deriv (multiplier operands) var)
                              (multiplicand operands))))))

(define (install-exp-package)
  (define (square x) (* x x))
  (define (pow base exp)
    (define (helper b e acc)
      (cond ((= e 0) acc)
            ((even? e) (helper (square b) (/ e 2) acc))
            (else (helper b (- e 1) (* acc b)))))
    (helper base exp 1))
  (define (base e) (car e))
  (define (exponent e) (cadr e))
  (define (make-exponentiation base exp)
    (cond ((=number? exp 0) 1)
          ((=number? exp 1) base)
          ((=number? base 1) 1)
          ((and (number? base) (number? exp)) (pow base exp))
          (else (list '** base exp))))
  (put 'deriv '** (lambda (operands var)
                    (make-product
                     (exponent operands)
                     (make-product
                      (make-exponentiation (base operands)
                                           (make-sum (exponent operands) -1))
                      (deriv (base operands) var))))))

(install-sum-package)
(install-mult-package)
(install-exp-package)

#|d. By swapping the tag and op, the packages used will have
to be structured differently. Instead of writing packages for
different types (e.g. +, *) for a particular operation (deriv),
packages will have to be written for different "operations"
(deriv, etc.) for a particular "type" (e.g. +). Besides that,
all calls to the put and get procedure will require the swapping
of the tag and op.|#

#|
Exercise 2.74: Insatiable Enterprises, Inc., is a highly decentralized
conglomerate company consisting of a large number of independent
divisions located all over the world. The
company’s computer facilities have just been interconnected
by means of a clever network-interfacing scheme that makes
the entire network appear to any user to be a single computer.
Insatiable’s president, in her first attempt to exploit
the ability of the network to extract administrative information
from division files, is dismayed to discover that, although all
the division files have been implemented as data
structures in Scheme, the particular data structure used varies
from division to division. A meeting of division managers
is hastily called to search for a strategy to integrate the files
that will satisfy headquarters’ needs while preserving the
existing autonomy of the divisions.
Show how such a strategy can be implemented with datadirected
programming. As an example, suppose that each
division’s personnel records consist of a single file, which
contains a set of records keyed on employees’ names. The
structure of the set varies from division to division. Furthermore,
each employee’s record is itself a set (structured
differently from division to division) that contains information
keyed under identifiers such as address and salary.
In particular:

a. Implement for headquarters a get-record procedure
that retrieves a specified employee’s record from a
specified personnel file. The procedure should be applicable
to any division’s file. Explain how the individual divisions’
files should be structured. In particular,
what type information must be supplied?

b. Implement for headquarters a get-salary procedure
that returns the salary information from a given employee’s
record from any division’s personnel file. How
should the record be structured in order to make this
operation work?

c. Implement for headquarters a find-employee-record
procedure. This should search all the divisions’ files
for the record of a given employee and return the record.
Assume that this procedure takes as arguments an
employee’s name and a list of all the divisions’ files.

d. When Insatiable takes over a new company, what changes
must be made in order to incorporate the new personnel
information into the central system?|#

(define insatiable (make-table))
(define ins-get (insatiable 'lookup-proc))
(define ins-put (insatiable 'insert-proc!))

(define (get-record division employee-name)
  ((get division 'record) employee-name))

#|a) The individual divisions' files should contain a set
of records for each employee keyed on employees' names,
as well as procedures that allow the extraction of various
identifiers and records e.g. record as a procedure to
return a particular emmployee's record, address as a procedure
to extract the address from a record, salary as a procedure
to extract the salary from a record.|#

(define (get-salary division record)
  ((get division 'salary) record))

#|b) The record should be structured as a set containing information
keyed under identifiers such as address and salary, with the identifiers
common across all records in a division's personnel file.|#

(define (find-employee-record divisions employee-name)
  (if (null? divisions)
      false
      (let ((record (get-record (car divisions) employee-name)))
        (if record
            record
            (find-employee-record (cdr divisions) employee-name)))))

#|
d) The new company needs to add its own record and salary procedures to
its file.
|#

#|Exercise 2.75: Implement the constructor make-from-magang in
message-passing style. This procedure should be analogous to the
make-from-real-imag procedure given above.|#

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos (angle a))))
          ((eq? op 'imag-part) (* r (sin (angle a))))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

#|Exercise 2.76: As a large system with generic operations
evolves, new types of data objects or new operations may
be needed. For each of the three strategies — generic operations
with explicit dispatch, data-directed style, and message
passing-style — describe the changes that must be made to a
system in order to add new types or new operations. Which
organization would be most appropriate for a system in
which new types must often be added? Which would be
most appropriate for a system in which new operations
must often be added?|#

#|
For generic operations with explicit dispatch, new operations
can be added directly to the system without modifying the
existing dispatch. However, when adding new types, all existing
operations need to be modified to accomodate the new type.

For data-directed systems, when a new type is added, one procedure
for each operation must be written and installed in the operating table.
When a new operation is needed, one procedure for each data type must be
written and installed in the operating table. As such, the existing code
does not need to be changed.

For message-passing, when a new type is added, a new dispatch procedure that
implements all the operations needs to be added. However, when a new operation
is added, each data type must alter its own dispatch procedure to include the
new operation.
|#