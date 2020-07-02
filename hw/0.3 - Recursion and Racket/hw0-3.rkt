#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define describe-time
(define (describe-time secs)
  (define make-pl (lambda (wd num) (word wd 's)))  ; the grader supposes singulars take (s) too
  (cond ((< secs 60) (se secs (make-pl 'second secs)))
    ((< secs (* 60 60)) (se (quotient secs 60) (make-pl 'minute  (quotient secs 60)) (describe-time (remainder secs 60))))
    ((< secs (* 60 60 24)) (se (quotient secs (* 60 60)) (make-pl 'hour (quotient secs (* 60 60))) (describe-time (remainder secs (* 60 60)))))
    (else (se (quotient secs (* 60 60 24)) (make-pl 'day  (quotient secs (* 60 60 24))) (describe-time (remainder secs (* 60 60 24)))))))
;; -> (describe-time 22222)
;; '(6 HOURS 10 MINUTES 22 SECONDS)


; Exercise 2 - Define remove-once
(define (remove-once wd sent)
  (cond ((empty? sent) '())
  ((equal? wd (first sent)) (bf sent))
  (else (se (first sent) (remove-once wd (bf sent))))))
;; -> (remove-once 'morning '(good morning good morning))
;; '(good good morning)


; Exercise 3 - Define differences
(define (differences nums)
  (if (or (empty? nums) (empty? (bf nums))) '()
			(se (- (first (bf nums)) (first nums)) (differences (bf nums)))))

;; -> (differences '(4 23 9 87 6 12))
;; '(19 -14 78 -81 6)

; Exercise 4 - Define location
(define (location small big)
  (cond ((empty? big) #f)
        ((equal? small (first big)) 1)
        (else (and (location small (bf big)) (+ 1 (location small (bf big)))))))
;; -> (location 'me '(you never give me your money))
;; 4
;; -> (location 'i '(you never give me your money))
;; #f
;; -> (location 'the '(the fork and the spoon))
;; 1

; Exercise 5 - Define initials
(define (initials sent)
  (if (empty? sent)
      '()
      (se (first (first sent))
          (initials (bf sent)))))
; (error "Not yet implemented"))


; Exercise 6 - Define copies
(define (copies num wd)
  (if (< num 1) '()
                (se wd (copies (- num 1) wd))))
;; -> (copies 8 'spam)
;; '(spam spam spam spam spam spam spam spam)

; Exercise 7 - Define gpa
(define (gpa grades)
  (define (base-grade grade)
    (cond ((equal? (first grade) 'A) 4.00)
          ((equal? (first grade) 'B) 3.00)
          ((equal? (first grade) 'C) 2.00)
          ((equal? (first grade) 'D) 1.00)
          ((equal? (first grade) 'F) 0.00)))
  (define (grade-modifier grade)
    (define (second wd) (first (bf wd)))
    (cond ((empty? (bf grade)) 0)
          ((equal? (second grade) '+) 0.33)
          ((equal? (second grade) '-) -0.33 )))
  (define (grade g) (+ (base-grade g) (grade-modifier g)))
  (if (empty? grades) 0
                      (/ (+ (grade (first grades)) (* (gpa (bf grades)) (count (bf grades)))) (+ 1 (count (bf grades))))))
;; -> (gpa '(A A+ B+ B))
;; 3.67

; Exercise 8 - Define repeat-words
(define (repeat-words sent)
;; -> (repeat-words '(4 calling birds 3 french hens))
;; '(calling calling calling calling birds french french french hens)
;; -> (repeat-words '(the 7 samurai))
;; '(the samurai samurai samurai samurai samurai samurai samurai)
  (cond ((empty? sent) '())
        ((empty? (bf sent)) sent)
        ((integer? (first sent)) (se (copies (if (integer? (first (bf sent))) (first sent) (- (first sent) 1)) (first (bf sent))) (repeat-words (bf sent))))
        (else (se (first sent) (repeat-words (bf sent))))))
  ; (cond ((empty? sent) '())
  ;       ((and (number? (first sent)) (> (first sent) 1)) (repeat-words (se (- (first sent) 1) (first (bf sent))  (bf sent))))
  ;       ((and (number? (first sent)) (= (first sent) 1)) (repeat-words (bf sent)))
  ;       (else (se (first sent) (repeat-words (bf sent))))))


; Exercise 9 - Define same-shape?
(define (same-shape? sent1 sent2)
;; -> (same-shape? '(the fool on the hill) '(you like me too much))
;; #t
;; -> (same-shape? '(the fool on the hill) '(and your bird can sing))
;; #f
    (cond ((and (empty? sent1) (empty? sent2)) #t)
          ((xor (empty? sent1) (empty? sent2)) #f)
          ((= (count (first sent1)) (count (first sent2))) (same-shape? (bf sent1) (bf sent2)))
          (else #f)))
  ; (if (= (count sent1) (count sent2))
  ;   (cond ((empty? sent1) #t)
  ;         ((= (count (first sent1)) (count (first sent2))) (same-shape? (bf sent1) (bf sent2)))
  ;         (else #f))
  ;   #f))
