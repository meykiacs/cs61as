#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))

;;Begin Project 1
(require "adjectives.rkt") 

;;Below is some procedures to help you out. Dont worry about what it says or does.
;;Questions are below.

(define (want-exit? line)
  (or (member? 'exit line) (member? 'quit line) (member? 'bye line)))

(define (print-sentence sent)
  (for-each (lambda (x) (display x) (display " "))
            sent)
  (newline))

(define (interact bot)
  (define (helper)
    (display "> ") (flush-output)
    (let ([line (read-line)])
      (unless (want-exit? line)
        (print-sentence (bot line))
        (helper))))
  (read-line)
  (helper))

(define (chatter bot1 bot2 start iterations)
  (define (helper b1 b2 sent i)
    (when (< i iterations)
          (display "bot ") (display (add1 (remainder i 2))) (display ": ")
          (let ((out (b1 sent)))
            (print-sentence out)
            (helper b2 b1 out (add1 i)))))
  (display "start: ") (print-sentence start)
  (helper bot1 bot2 start 0))

;;; Checks if a given word is an adjective or not
;;; Requires adjectives.scm to be loaded beforehand
(define adjective?
  (let ((hash (make-hash)))
    (for-each (lambda (adj)
		(hash-set! hash adj #t))
	      adjectives)
    (lambda (wd) (hash-ref hash wd #f))))


;; Begin Questions:
;;Q1 - babybot
  (define (babybot sent)
    sent)

;;Q2 - stupidbot-creator
  (define (stupidbot-creator motto)
    (lambda (sent)
      motto))

;;Q3 - matcherbot-creator
  (define (matcherbot-creator pattern)
    (define (helper sent my-pattern) 
      (cond ((empty? my-pattern) sent)
            ((empty? sent) #f)
            ((equal? (first my-pattern) (first sent)) (helper (bf sent) (bf my-pattern)))
            (else (helper (bf sent) pattern))))
    (lambda (sent) (helper sent pattern)))

;;Q4 - substitutebot-creator
  (define (substitutebot-creator from to)
    (define (helper sent fr t)
      (cond ((empty? sent) '())
            ((empty? fr) (se (first sent) (helper (bf sent) from to)))
            ((equal? (first sent) (first fr)) (se (first t) (helper (bf sent) from to)))
            (else (helper sent (bf fr) (bf t)))))
    (lambda (sent) (helper sent from to)))

;;Q5 - switcherbot
  (define (switcherbot sent)
    (define helper (substitutebot-creator '(me I am was my mine you are were your yours) '(you you are were your yours me am was my mine)))
    (if (equal? (first sent) 'you)
        (se 'I (helper (bf sent)))
        (helper sent)))


;;Q6 - inquisitivebot
  (define (inquisitivebot sent)
    (if (empty? sent)
        ('())
        (se (switcherbot sent) '?)))
  
;;Q7 - eliza
  (define (eliza sent)
    (cond ((empty? sent) '(how can I help you ?))
          ((equal? (first sent) 'hello) '(hello there!))
          ((equal? (last sent) '?) '(I can not answer your question.))
          (((matcherbot-creator '(I am)) sent) (se '(why are you) (inquisitivebot ((matcherbot-creator '(I am)) sent))))
          (else (switcherbot sent))))

;;Q8 - reactorbot-creator
  (define (reactorbot-creator bot pat out)
    (lambda (sent)
      (if (equal? sent pat)
          out
          (bot sent))))

;;Q9 - replacerbot-creator
  (define (replacerbot-creator bot pat before after)
    (lambda (sent)
      (if ((matcherbot-creator pat) sent)
          (se before ((matcherbot-creator pat) sent) after)
          (bot sent))))

;;Q10 - exagerate
  (define (exaggerate bot n)
    (define (helper sent)
      (cond ((empty? sent) '())
            ((adjective? (first sent)) (se 'very (first sent) (helper (bf sent)) ))
            (else (se (first sent) (helper (bf sent))))))
    
    (lambda (sent)
      ((repeated helper n) sent)))
;;REMEMBER TO ADD YOUR OWN TESTS TO GRADER.RKT!
;;END OF PROJECT 1
