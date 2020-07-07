#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1: Invariant for Fast Exponentiation
; Define fast-expt-iter
; Design a procedure that
; evolves an iterative exponentiation process
; that uses successive squaring and uses a logarithmic number of steps
; as does fast-expt
(define (fast-expt-iter b n)
  (define (iter base power mul)
    (cond ((= power 0) mul)
          ((even? power) (iter (* base base) (/ power 2) mul))
          (else (iter base (- power 1) (* mul base)))))
  (iter b n 1))

; Exercise 2: Golden Ratio (Optional)
; Read the subsection on finding fixed points of functions in SICP,
; Golden ration: phi^2 = phi + 1
; the golden ratio phi is
; a ﬁxed point of the transformation x -> 1 + 1/x,
; and use this fact to compute phi by means of the fixed-point procedure.

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define phi
  (fixed-point (lambda (x) (+ 1.0 (/ 1.0 x))) 1.5))


; Exercise 3 - Define cont-frac: an infinite continued fraction
;; f = n(k)/(d(i) + (n(i-1)/(d(i-1) + ...
;; 1/phi = 1 / (1 + 1/(1+(1/(1+...
;; recursive
(define (cont-frac n d k)
  (define (iter m)
    (if (> m k)
      0.0
      (/ (n m) (+ (d m) (iter (+ m 1))))))
  (iter 1))

;; iteration
(define (cont-frac-iter n d k)
  (define (iter counter result)
    (if (= counter 0)
        result
        (iter (- counter 1) (/ (n counter) (+ (d counter) result)))))
  (iter k 0.0))

;; check:
; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 20)
; (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 20)

;; euler's e ??
(define (e k)
  (+ (cont-frac
       (lambda (i) 1.0)
       (lambda (i) (if (= (remainder i 3) 2)
                       (+ 2.0 (* 2.0 (quotient i 3)))
                       1.0)) k) 2.0))

; Exercise 4 - Define next-perf
(define (next-perf n)
  (define (sum-of-factors counter)
    (cond [(<= counter 0) 0]
          [(= (remainder n counter) 0)
           (+ counter (sum-of-factors (- counter 1)))]
          [else (sum-of-factors (- counter 1))]))
  (if (and (= (sum-of-factors (- n 1)) n) (not (= n 0)))
      n
      (next-perf (+ n 1))))

;; (define (next-perf-iter n)
;;   (define (sum-of-factors x)
;;     (define (iter x counter result)
;;       (cond ((= x counter) result)
;;             ((= (remainder x counter) 0) (iter x (+ 1 counter) (+ result counter)))
;;             (else (iter x (+ 1 counter) result))))
;;     (iter x 1 0))
;;   (if (= n (sum-of-factors n))
;;       n
;;       (next-perf-iter (+ n 1))))

; Exercise 5 - Explain what happens when the base cases are interchanged.
#|
(define (count-change amount)
  (cc amount '(50 25 10 5 1)))


(define (cc amount kinds-of-coins)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (empty? kinds-of-coins)) 0]
        [else (+ (cc amount
                     (bf kinds-of-coins))
                 (cc (- amount
                        (first kinds-of-coins))
                     kinds-of-coins))] ))


Your explanation here
if the amount is zero and kinds-of-coins is empty, two different answers are produced
|#

; Exercise 6 - Give a formula relating b, n, counter and product in expt-iter.

#|
(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product))))
|#

#|
Formula for expt:
b^n = b^n
Formula for expt-iter:
b^counter*product = b^n
|#


