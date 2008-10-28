(define (make-interval a b) (cons a b))

(define (lower-bound i) (car i))

(define (upper-bound i) (cdr i))

(define (interval-width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent center percent)
  (let ((width (* center ( / percent 100.0))))
    (make-center-width center width)))

(define (percent i)
  (* (/ (interval-width i) (center i)) 100.0))
