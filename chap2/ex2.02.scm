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

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment s)
  (let ((midx (average (x-point (start-segment s))
                       (x-point (end-segment s))))
        (midy (average (y-point (start-segment s))
                       (y-point (end-segment s)))))
    (make-point midx midy)))
