#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define substitute
;; (substitute '(she loves you yeah yeah yeah) 'yeah 'maybe)
;; (she loves you maybe maybe maybe)
(define (substitute sent old-word new-word)
  (cond ((empty? sent) '())
    ((equal? old-word (first sent)) (se new-word (substitute (bf sent) old-word new-word)))
    (else (se (first sent) (substitute (bf sent) old-word new-word)))))


; Exercise 2 - Try out the expressions!

#|
(lambda (x) (+ x 3))
-> returns: 
#<procedure>

((lambda (x) (+ x 3)) 7)
-> returns:
10

(define (make-adder num)
  (lambda (x) (+ x num))) 
((make-adder 3) 7)
-> returns:
10

(define plus3 (make-adder 3)) 
(plus3 7)
-> returns:
10

(define (square x) (* x x)) 
(square 5)
-> returns:
25

(define square (lambda (x) (* x x))) 
(square 5)
-> returns
25

(define (try f) (f 3 5)) 
(try +)
-> returns:
8

(try word)
-> returns:
35

|#

; Exercise 3
#|
Consider a function g for which the expression
((g) 1)
returns the value 3 when evaluated.

Number of arguments g has: 
0

Type of value returned by g:
procedure

|#

; Exercise 4 - Define f1, f2, f3, f4, and f5
;  For each expression, give a definition of f such that evaluating the expression will not cause an error, and say what the expression's value will be, given your definition. 
#|
-> f1
(define f1 2)
2

-> (f2)
(define f2 +)
#<procedure

-> (f3 3)
(define (f3 x) (* x x))
9

((f4))
(define f4 (lambda () (lambda () (* 4 4))))
16

(((f5)) 3)
(define f5 (lambda () (lambda () (lambda (x) (* x x)))))
9

|#
; Exercise 5 - Try out the expressions

(define (t f) 
  (lambda (x) (f (f (f x)))) )

#|
1. ((t add1) 0)
returns:
3

2. ((t (t add1)) 0) returns:
9

3. (((t t) add1) 0) returns:
27
|#

; Exercise 6 - Try out the expressions

(define (s x)
  (+ 1 x))

#|

1. ((t s) 0) returns:
3

2. ((t (t s)) 0) returns:
9

3. (((t t) s) 0) returns:
27

|#

; Exercise 7 - Define make-tester
#|
-> ((make-tester 'hal) 'hal)
#t
-> ((make-tester 'hal) 'cs61a)
#f
-> (define sicp-author-and-astronomer? (make-tester 'gerry))
-> (sicp-author-and-astronomer? 'hal)
#f
-> (sicp-author-and-astronomer? 'gerry)
#t
|#

(define (make-tester wd)
  (lambda (wd2) (equal? wd wd2)))

; Exercise 8 - SICP exercises

; SICP 1.31a
;; Write a procedure called product that returns the product of the values of a function at points over a given range. Show how to define factorial in terms of product Also use product to compute approximations to PI using the formula: (2*4*4*6*6*8...)/(3*3*5*5*7*7)
(define (product term a next b)
  (if (> a b) 1
              (* (term a) (product term (next a) next b))))

(define (factorial n)
  (product (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(define (estimate-pi)
  (define (sqaure x) (* x x))
  (define (add2 x) (+ 2.0 x))
  (/ (* 8.0 (product sqaure 4.0 add2 100)) (* 100 (product sqaure 3.0 add2 100))))

; SICP 1.32a

;; This is called my-accumulate so it doesn't conflict with Simply
;; Scheme's accumulate.
(define (my-accumulate combiner null-value term a next b)
  (if (> a b) null-value
              (combiner (term a) (my-accumulate combiner null-value term (next a) next b))))

;; Write sum in terms of my-accumulate:
(define (sum-accum term a next b)
  (my-accumulate + 0 term a next b))

;; Write product in terms of my-accumulate:
(define (product-accum term a next b)
  (my-accumulate * 1 term a next b))


; SICP 1.33
;  write filtered-accumulate that combines only those terms derived from values in the range that satisfy a specified condition. The resulting filtered-accumulate abstraction takes the same arguments as accumulate, together with an additional predicate of one argument that specifies the filter.
(define (filtered-accumulate combiner null-value term a next b pred)
  (cond ((> a b) null-value)
        ((pred a) (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b pred)))
        (else (filtered-accumulate combiner null-value term (next a) next b pred))))

(define (sum-sq-prime a b)
  (filtered-accumulate + 0 (lambda (x) (* x x)) a add1 b prime?))

(define (rel-prime? x y)
  (= (gcd x y) 1))

(define (prod-of-some-numbers n)
  (filtered-accumulate * 1  (lambda (x) x) 1 add1 n (lambda (x) (rel-prime? x n))))

; SICP 1.40 - Define cubic
; Define a procedure cubic that can be used together with the newtons-method procedure in expressions of the form : x^3 + ax^2 + bx + c
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

; SICP 1.41 - Define double

(define (double proc)
  (lambda (x) (proc (proc x))))

; SICP 1.43 - Define repeated
;; challenging
(define (my-repeated proc n)
  (lambda (x)
    (if (= n 1) (proc x)
              (proc ((my-repeated proc (- n 1)) x)))))

; Exercise 9 - Define my-every

(define (my-every proc sent)
  (if (empty? sent) 
    '()
    (se (proc (first sent)) (my-every proc (bf sent)))))

; Exercise 10 - Try out the expressions

#|

(every (lambda (letter) (word letter letter)) 'purple)
-> returns:
'(pp uu rr pp ll ee)

(every (lambda (number) (if (even? number) (word number number) number))
       '(781 5 76 909 24))
-> returns:
'(781 5 7676 909 2424)

(keep even? '(781 5 76 909 24))
-> returns:
'(76 24)

(keep (lambda (letter) (member? letter 'aeiou)) 'bookkeeper)
-> returns:
'ooeee

(keep (lambda (letter) (member? letter 'aeiou)) 'syzygy)
-> returns:
""

(keep (lambda (letter) (member? letter 'aeiou)) '(purple syzygy))
-> returns:
error: invelide arguments for member?

(keep (lambda (wd) (member? 'e wd)) '(purple syzygy))
-> returns:
'(purple)
|#
