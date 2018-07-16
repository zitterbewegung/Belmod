#lang rosette

(require javascript
         csv-reading
         rosette/lib/synthax)
(require rosette/query/debug rosette/lib/render)
(define-symbolic a b boolean?)
(define-symbolic x y integer?)
(define-symbolic able-to-trade-trigger boolean?)

(current-bitwidth 256) 
;Setting current bitwidth to 256 
;Create a set of conditions where we are able to trade
; And a buy and sell price are satisified
(define angelic-bitcoin-solution
  ; Input: A boolean that allows me to trade, And the price of bitcoin to execute the trade at.
  (solve (begin (assert able-to-trade-trigger)
                (assert (> x 10000))))) ;Buy Price

(define sol
    (solve (begin (assert a)
                  (assert (= x 1))
                  (assert (= y 2)))))
(define sol-fail
    (solve (begin (assert a)
                  (assert (equal?  a y))
                  (assert (= x 1))
                  (assert (= y 2)))))
(define variable (evaluate 4 sol))

; Model Variable -> Pyramid-Smart-Contract
; creates a variable that
; y pixels from the top and 10 from the left
(define (make-contract model variable)
  (if (sat? model)
     `(begin (define x1 ,variable))
     '(begin self-destruct)))

(define (make-contract-javascript model variable)
  (if (sat? model)
     `(begin  (fprintf (current-output-port)
           "var x=~a ;"
           ,variable))
     '(begin self-destruct)))

;Model Variable
(define (conditional-template boolean value-to-evaluate)
  (let ((x (if boolean 'red 'green)))
  (cond ((eq? x 'red)   (make-contract sol 1))
        ((eq? x 'green) (make-contract sol -2))
        (else          '(begin self-destruct)))))


(define optimize-sol
  (optimize #:maximize (list (+ x y))
            #:guarantee (assert (< (- y x) 1))))
;From https://github.com/Joshuaek/bitcoin-forecast/blob/master/Bitcoin%2Bprice%2Bforecast.ipynb
(define input "809,10497.55071817646,14429.437428824125\n810,10504.662222465608,14416.385521201237\n811,10473.934629975964,14445.35613558157\n812,10344.797889171332,14367.442637747907\n813,10292.691542759238,14294.650528545799\n")
(define next-row (make-csv-reader input))
(next-row)
(define (behavior-delta threshold)
    (if (> threshold (string->number (cadr (next-row))))
        (conditional-template #t threshold)
        (conditional-template #f threshold)))




(define output '(("" "ds" "yhat" "yhat_lower" "yhat_upper")
  ("809"
   "2018-01-05 21:00:00"
   "12443.0934730835"
   "10497.55071817646"
   "14429.437428824125")
  ("810"
   "2018-01-05 22:00:00"
   "12461.01925136911"
   "10504.662222465608"
   "14416.385521201237")
  ("811"
   "2018-01-05 23:00:00"
   "12442.784072308054"
   "10473.934629975964"
   "14445.35613558157")
  ("812"
   "2018-01-06 00:00:00"
   "12387.559937091984"
   "10344.797889171332"
   "14367.442637747907")
  ("813"
   "2018-01-06 01:00:00"
   "12313.81510242808"
   "10292.691542759238"
   "14294.650528545799")))
