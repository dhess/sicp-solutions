;; First implementation using the solution from exercise 2.2:

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment p)
  (car p))

(define (end-segment p)
  (cdr p))

(define (square x) (* x x))

(define (segment-length s)
  (let ((p1 (start-segment s))
        (p2 (end-segment s)))
    (sqrt (+ (square (- (x-point p2)
                        (x-point p1)))
             (square (- (y-point p1)
                        (y-point p2)))))))

;; p1, p2 and p3 are points which construct a right triangle:
;;
;; p1 +
;;    |
;;    |
;; p2 +------+ p3

(define (make-rectangle p1 p2 p3)
  (cons (make-segment p1 p2) (make-segment p2 p3)))

(define (rectangle-width r)
  (segment-length (cdr r)))

(define (rectangle-height r)
  (segment-length (car r)))

(define (rectangle-perimeter r)
  (+ (* 2 (rectangle-width r))
     (* 2 (rectangle-height r))))

(define (rectangle-area r)
  (* (rectangle-width r) (rectangle-height r)))


;; A more robust representation which takes a line segment specifying
;; the length of the base of the rectangle, and a scalar values for
;; the height. The other accessors are the same.

(define (make-rectangle base height)
  (cons base height))

(define (rectangle-width r)
  (segment-length (car r)))

(define (rectangle-height r)
  (cdr r))
