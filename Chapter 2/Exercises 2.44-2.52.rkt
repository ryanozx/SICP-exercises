#lang sicp

(#%require sicp-pict)

#|Exercise 2.44: Define the procedure up-split used by cornersplit. It is similar to right-split, except that it switches
the roles of below and beside.
|#

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

#|Exercise 2.45: right-split and up-split can be expressed
as instances of a general splitting operation. Define a procedure
split with the property that evaluating

(define right-split (split beside below))
(define up-split (split below beside))

produces procedures right-split and up-split with the
same behaviors as the ones already defined.|#

(define (split main sec)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split main sec) painter (- n 1))))
          (main painter (sec smaller smaller))))))

#|Exercise 2.46: A two-dimensional vector v running from
the origin to a point can be represented as a pair consisting
of an x-coordinate and a y-coordinate. Implement a data
abstraction for vectors by giving a constructor make-vect
and corresponding selectors xcor-vect and ycor-vect. In
terms of your selectors and constructor, implement procedures
add-vect, sub-vect, and scale-vect that perform
the operations vector addition, vector subtraction, and
multiplying a vector by a scalar:

(x1, y1) + (x2, y2) = (x1 + x2, y1 + y2),
(x1, y1) - (x2, y2) = (x1 - x2, y1 - y2),
s · (x , y) = (sx ,sy)|#

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (frame-origin-1 frame) (add-vect (scale-vect (xcor-vect v) (frame-edge1-1 frame))
                                             (scale-vect (ycor-vect v) (frame-edge2-1 frame))))))

(define (make-vect x y) (cons x y))
(define (xcor-vect vect) (car vect))
(define (ycor-vect vect) (cdr vect))

(define (add-vect v1 v2) (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2) (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v) (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

#|Exercise 2.47: Here are two possible constructors for frames:|#

(define (make-frame-1 origin edge1 edge2) (list origin edge1 edge2))
(define (make-frame-2 origin edge1 edge2) (cons origin (cons edge1 edge2)))

#|For each constructor supply the appropriate selectors to
produce an implementation for frames.|#

(define (frame-origin-1 frame) (car frame))
(define (frame-edge1-1 frame) (cadr frame))
(define (frame-edge2-1 frame) (caddr frame))

(define (frame-origin-2 frame) (car frame))
(define (frame-edge1-2 frame) (cadr frame))
(define (frame-edge2-2 frame) (cddr frame))

#|Exercise 2.48: A directed line segment in the plane can be
represented as a pair of vectors—the vector running from
the origin to the start-point of the segment, and the vector
running from the origin to the end-point of the segment.
Use your vector representation from Exercise 2.46 to define a
representation for segments with a constructor make-segment and
selectors start-segment and end-segment|#

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

#|Exercise 2.49: Use segments->painter to define the following primitive painters:
a. The painter that draws the outline of the designated
frame.
b. The painter that draws an “X” by connecting opposite
corners of the frame.
c. The painter that draws a diamond shape by connecting the
midpoints of the sides of the frame.
d. The wave painter|#

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

(define frame-outline
  (let ((p1 (make-vect 0 0))
        (p2 (make-vect 1 0))
        (p3 (make-vect 1 1))
        (p4 (make-vect 0 1)))
    (segments->painter (list (make-segment p1 p2) (make-segment p2 p3) (make-segment p3 p4) (make-segment p4 p1)))))

(define draw-x
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1)) (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define diamond
  (let ((p1 (make-vect 0.5 0))
        (p2 (make-vect 1 0.5))
        (p3 (make-vect 0.5 1))
        (p4 (make-vect 0 0.5)))
    (segments->painter (list (make-segment p1 p2) (make-segment p2 p3) (make-segment p3 p4) (make-segment p4 p1)))))

#|The wave painter is left as an exercise to the reader.|#

#|Exercise 2.50: Define the transformation flip-horiz, which
flips painters horizontally, and transformations that rotate
painters counterclockwise by 180 degrees and 270 degrees.|#

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

(define (rotate90 painter) (transform-painter painter
                                              (make-vect 1.0 0.0)
                                              (make-vect 1.0 1.0)
                                              (make-vect 0.0 0.0)))

(define (flip-horiz painter) (transform-painter painter
                                                (make-vect 1.0 0.0)
                                                (make-vect 0.0 0.0)
                                                (make-vect 1.0 1.0)))

(define (rotate180 painter) (transform-painter painter
                                               (make-vect 1.0 1.0)
                                               (make-vect 0.0 1.0)
                                               (make-vect 1.0 0.0)))

(define (rotate270 painter) (transform-painter painter
                                               (make-vect 0.0 1.0)
                                               (make-vect 0.0 0.0)
                                               (make-vect 1.0 1.0)))

#|Exercise 2.51: Define the below operation for painters. below
takes two painters as arguments.The resulting painter, given
a frame, draws with the first painter in the bottom of the
frame and with the second painter in the top. Define below
in two different ways—first by writing a procedure that is
analogous to the beside procedure given above, and again
in terms of beside and suitable rotation operations (from
Exercise 2.50)|#

(define (my-beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left (transform-painter painter1
                                         (make-vect 0.0 0.0)
                                         split-point
                                         (make-vect 0.0 1.0)))
          (paint-right (transform-painter painter 2
                                          split-point
                                          (make-vect 1.0 0.0)
                                          (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (my-below1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom (transform-painter painter1
                                           (make-vect 0.0 0.0)
                                           (make-vect 1.0 0.0)
                                           split-point))
          (paint-top (transform-painter painter2
                                        split-point
                                        (make-vect 1.0 0.5)
                                        (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (my-below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

#|Exercise 2.52: Make changes to the square limit of wave
shown in Figure 2.9 by working at each of the levels described above.
In particular:

a. Add some segments to the primitive wave painter of
Exercise 2.49 (to add a smile, for example).
b. Change the pattern constructed by corner-split (for
example, by using only one copy of the up-split and
right-split images instead of two).
c. Modify the version of square-limit that uses squareof-four
so as to assemble the corners in a different
pattern. (For example, you might make the big Mr.
Rogers look outward from each corner of the square.)|#

(define (modified-corner-split painter n)
  (if (= n 0)
      painter
      (beside (below painter (up-split painter (- n 1)))
              (below (right-split painter (- n 1)) (modified-corner-split painter (- n 1))))))

(define (modified-square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (quarter (flip-horiz quarter)))))
      (below (flip-vert half) half))))