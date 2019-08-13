#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 0
;Write 5 expressions whose values are the number ten:
;1. Atom
10
;2. Compound Expression (3 Atoms)
(+ 6 4)
;3. Compound Expression (4 Atoms)
(+ 6 3 1)
;4. Compound Expression (1 Atom and 2 subexpressions)
(+ (* 2 2) (* 3 2))
;5. Any Other Kind Expression
(- 11 1)

;Exercise 1
(define (second wd)
  (first (bf wd)))

;1. Define first-two
(define (first-two wd)
  ; your code here
  (word (first wd) (second wd))
)

;;2. Define two-first
(define (two-first x y)
  ; your code here
  (word (first x) (first y)))

;;3. Define two-first-sent
(define (two-first-sent sent)
  ; your code here
  (word (first (first sent)) (first (second sent))))

;Exercise 2 - Define teen?
(define (teen? num)
  ; your code here
  (and (<= num 19) (>= num 13)))

;Exercise 3 - Define indef-article
(define (indef-article wd)
  ; your code here
  (if (member (first wd) '(a e u o i))
      (se 'an wd)
      (se 'a wd)))

;Exercise 4 - Define insert-and
(define (insert-and sent)
  ; your code here
  (se (bl sent) 'and (last sent)))

;Exercise 5 - Define query
(define (query sent)
  ; your code here
  (se (second sent) (first sent) (bl (bf (bf sent))) (word (last sent) '?)))

;Exercise 6 - Define european-time and american-time
(define (european-time time)
  ; your code here
  (cond ((equal? time '(12 am)) 0)
        ((equal? time '(12 pm)) 12)
        ((equal? (last time) 'pm) (+ (first time) 12))
        (else (first time))))

(define (american-time time)
  ; your code here
  (cond ((equal? time 0) '(12 am))
        ((equal? time 12) '(12 pm))
        ((> time 12) (se (- time 12) 'pm))
        (else (se time 'am))))

;Exercise 7 - Define describe-time
(define (describe-time secs)
  ; your code here
  (cond ((< secs 60) (se secs 'seconds))
        ((< secs (* 60 60)) (se (/ secs 60.0) 'minutes))
        ((< secs (* 60 60 24)) (se (/ secs (* 60.0 60)) 'hours))
        ((< secs (* 60 60 24 365.25)) (se (/ secs (* 60.0 60 24)) 'days))))

;Exercise 8 - Explain why superlative doesnt work:
(define (superlative adjective word)
  (se (word adjective 'est) word))

#|

Explanation here.
The built-in procedure "word" has been replaced with whatever was passed as the second
argument to "superlative". Thus, the "word" built-in is no longer available within the
scope of the procedure. Change the "word" variable in the superlative procedure to fix
|#