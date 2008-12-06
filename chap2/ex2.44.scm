;;; Requires the PLT DrScheme graphical environment. Set the language
;;; to "Module".
#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (up-split einstein 2))
