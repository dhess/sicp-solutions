;;; Requires the PLT DrScheme graphical environment. Set the language
;;; to "Module".
#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;;; Solution, part a.

(define nil '())

;;; Convenience function for drawing connected line
;;; segments. vect-list must have at least 2 elements.

(define (connect vect-list)
  (define (iter segment-list remaining)
    (if (null? (cdr remaining))
        (reverse segment-list)
        (iter (cons (make-segment (car remaining)
                                  (cadr remaining))
                    segment-list)
              (cdr remaining))))
  (iter nil vect-list))

;;; Draws the "wave" image.

(define (smile-wave frame)
  ((segments->painter (append (connect (list (make-vect 0.4  0.0)
                                             (make-vect 0.5  0.33)
                                             (make-vect 0.6  0.0))) ;inside legs
                              (connect (list (make-vect 0.25 0.0)
                                             (make-vect 0.33 0.5)
                                             (make-vect 0.3  0.6)
                                             (make-vect 0.1  0.4)
                                             (make-vect 0.0  0.6))) ;lower left
                              (connect (list (make-vect 0.0  0.8)
                                             (make-vect 0.1  0.6)
                                             (make-vect 0.33 0.65)
                                             (make-vect 0.4  0.65)
                                             (make-vect 0.35 0.8)
                                             (make-vect 0.4  1.0))) ;upper left
                              (connect (list (make-vect 0.75 0.0)
                                             (make-vect 0.6  0.45)
                                             (make-vect 1.0  0.15)));lower right
                              (connect (list (make-vect 1.0  0.35)
                                             (make-vect 0.8  0.65)
                                             (make-vect 0.6  0.65)
                                             (make-vect 0.65 0.8)
                                             (make-vect 0.6  1.0)));upper right
                              (connect (list (make-vect 0.45 0.75)
                                             (make-vect 0.5  0.7)
                                             (make-vect 0.55 0.75)));smile
                              ))
   frame))

;;; Solution, part b.

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (alt-corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (alt-corner-split painter (- n 1))))
        (beside (below painter up)
                (below right corner)))))

;;; Solution, part c.

(define (identity x) x)

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n) 
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))
