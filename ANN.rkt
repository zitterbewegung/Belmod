#lang racket
;;; Scheme Artificial Neural Network
;; TODO
;; - learning rate
;; - load/save weights

;;; Supporting functions

;; Stack push macro
(define-syntax push!
  (syntax-rules ()
    ((push item place)
     (set! place (cons item place)))))

;; Sum weights in given list
(define (sum-weight lst)
  (if (empty? lst) 0
      (+ (* ((caar lst)) (cadar lst))
         (sum-weight (cdr lst)))))

;; Sigmoid learning function
(define (sigmoid x)
  (/ (+ 1.0 (exp (- x)))))

;; Generate a new random weight
(define (rand-weight)
  (- (* (random) 1.2) 0.6))

;; Generate a new random threshold
(define (rand-theta)
  (- (* (random) 1.2) 0.6))

;;; Neuron functions

;; Train neurons in weight list
(define (train lst err)
  (if (empty? lst)
      '()
      (let ((n (caar lst))
            (w (cadar lst)))
        (n 'sum (* err w))
        (cons (list n (+ w (* (n) err)))
              (train (cdr lst) err)))))

;; Create a new neuron
(define (new-neuron)
  (let ((theta (rand-theta))
        (backward '())
        (cache #f)
        (trained #f)
        (train-sum 0))
    ;; Neuron function with closure
    (lambda ([method 'activate] [arg '()])
      (cond
       ((eq? method 'backward)
        (push! (list arg (rand-weight)) backward))
       ((eq? method 'set)
        (set! cache arg))
       ((eq? method 'reset)
        (set! cache #f)
        (set! trained #f)
        (set! train-sum 0))
       ((eq? method 'sum)
        (set! train-sum (+ train-sum arg)))
       ((eq? method 'list)
        (map (lambda (el) (cadr el)) backward))
       ((eq? method 'train)
        (if (not trained)
            (set! backward (train backward
                                  (* cache
                                     (- 1 cache)
                                     train-sum)))
            (set! trained #t)))
       ((eq? method 'activate)
        (if cache
            cache
            (begin
              (set! cache (sigmoid (- (sum-weight backward)
                                      theta)))
              cache)))))))

;;; Layer functions

;; Create a new neuron layer
(define (new-layer n)
  (if (= n 0) '()
      (cons (new-neuron) (new-layer (- n 1)))))

;; Link two layers together
(define (link-layers left right)
  (if (or (empty? left) (empty? right))
      '()
      (begin
        ((car right) 'backward (car left))
        (link-layers (cdr left) right)
        (link-layers left (cdr right)))))

;; Link up layers in an unlinked ann
(define (link-ann ann)
  (if (= (length ann) 1) '()
      (begin
        (link-layers (car ann) (cadr ann))
        (link-ann (cdr ann)))))

;; Hard set a layer of neurons
(define (set-layer layer in)
  (if (empty? layer) '()
      (begin
        ((car layer) 'set (car in))
        (set-layer (cdr layer) (cdr in)))))

;; Activate a layer, which activates all layers behind it
(define (run-layer layer)
  (if (empty? layer) '()
      (cons ((car layer)) (run-layer (cdr layer)))))

;; Reset a single layer
(define (reset-layer layer)
  (if (empty? layer)
      '()
      (begin
        ((car layer) 'reset)
        (reset-layer (cdr layer)))))

;; Train a layer, which back-propagates
(define (sum-layer layer out desired a)
  (if (empty? layer)
      '()
      (begin
        ((car layer) 'sum (* a (- (car desired) (car out))))
        (cons (car out)
              (sum-layer (cdr layer)
                           (cdr out
                           (cdr desired)
                           a))))))

;; Run 'train on each neuron in layer
(define (train-layer layer)
  (if (empty? layer)
      '()
      (begin
        ((car layer) 'train))))

;; Run training on all layers from front (pass in reversed)
(define (train-layers rev-ann)
  (if (empty? rev-ann)
      '()
      (begin
        (train-layer (car rev-ann))
        (train-layers (cdr rev-ann)))))

;;; ANN functions

;; Create new ann based on specification
(define (new-ann spec)
  (let ((ann (map new-layer spec)))
    (link-ann ann)
    ann))

;; Reset each neuron in ann
(define (reset-ann ann)
  (if (empty? ann)
      '()
      (begin
        (reset-layer (car ann))
        (reset-ann (cdr ann)))))

;; Get output of ann
(define (run-ann ann in)
  (set-layer (car ann) in)
  (let ((out (run-layer (last ann))))
    (reset-ann ann)
    out))

;; Train the ann
(define (train-ann ann in desired [a 1])
  (set-layer (car ann) in)
  (let ((out (run-layer (last ann))))
    (sum-layer (last ann) out desired a)
    (train-layers (reverse ann))
    (reset-ann ann)
    out))

;;; App functions

;; round to binary 1's and 0's
(define (round-output out)
  (map (compose inexact->exact round) out))

;; Turn integer into list suitable for an ANN
(define (int->blist int size)
  (if (= 0 size)
      '()
      (cons (remainder int 2)
            (int->blist (floor (/ int 2)) (- size 1)))))

;; Turn ANN output into integer
(define (blist->int lst)
  (if (null? lst)
      0
      (+ (car lst) (* 2 (blist->int (cdr lst))))))