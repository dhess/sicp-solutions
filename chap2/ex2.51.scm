;;; Requires the PLT DrScheme graphical environment. Set the language
;;; to "Module".
#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (my-below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           ((transform-painter (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point)
            painter1))
          (paint-top
           ((transform-painter split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))
            painter2)))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (my-below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))
