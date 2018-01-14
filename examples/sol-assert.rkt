#lang rosette/safe
(require rosette/lib/synthax)

(define-symbolic a b boolean?)
(define-symbolic x y integer?)
(define sol
    (solve (begin (assert a)
                  (assert (= x 1))
                  (assert (= y 2)))))
(define sol-fail
    (solve (begin (assert a)
                  (assert (equal?  a y))
                  (assert (= x 1))
                  (assert (= y 2)))))
(define variable (evaluate (list 4 5 x) sol))
(define (make-contract model variable)
  (if (sat? model)
     `(begin (define x1 ,variable))
     '(begin self-destruct)))
;(if (sat? sol)
;     `(begin (define x1 ,variable))
;     '(begin self-destruct))

;(if (sat? sol-fail)
;     `(begin (define x1 ,variable))
;     '(begin self-destruct))


