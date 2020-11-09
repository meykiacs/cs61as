#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))

;;Begin Project 1
(require "adjectives.rkt")

;; Below is some procedures to help you out. Dont worry about what it says or does.
;; Questions are below.

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
;; Q1 - babybot

(test-case
    "babybot"
  (check-equal?
   (babybot '(I am babybot))
   '(I am babybot)
   "test 1")
  ;; Add more tests here
  )
(define (babybot sent)
  sent)

;; Q2 - stupidbot-creator
(test-case
    "stupidbot-creator"
  (check-equal?
   ((stupidbot-creator '(I am Groot)) '(who are you))
   '(I am Groot)
   "test 1")
  ;; Add more tests here
  )
(define (stupidbot-creator motto)
  (lambda (sent)
    motto))

;; Q3 - matcherbot-creator
;; take in a sentence,PATTERN, and output a ChatterBot, OUT.
;; The resulting ChatterBot OUT takes in a
;; sentence, SENT. If PATTERN is a part of SENT, the chatter-matcher should return
;; everything that follows the first appearence of PATTERN in SENT. If PATTERN is
;; not in SENT, return #f. If PATTERN is at the very end of the sentence, return
;; the empty sentence. If PATTERN is the empty sentence, return SENT.

;; (test-case
;;     "matcherbot-creator"
;;   (check-equal?
;;    ((matcherbot-creator '(my name is)) '(my name is starlord))
;;    '(starlord)
;;    "test 1")
;;   (check-equal?
;;    ((matcherbot-creator '(my name is)) '(the names starlord))
;;    #f
;;    "test 2")
;;   ;; Add more tests here
;;   (check-equal?
;;    ((matcherbot-creator '(hufflepuffs are great))
;;     '(slytherins hate hufflepuffs but hufflepuffs are great finders))
;;    '(finders)
;;    "test 3")
;;   (check-equal?
;;    ((matcherbot-creator '(hufflepuffs are great))
;;     '(slytherins hate hufflepuffs but hufflepuffs are great))
;;    '()
;;    "test 4")
;;   (check-equal?
;;    ((matcherbot-creator '())
;;     '(slytherins hate hufflepuffs but hufflepuffs are great finders))
;;    '(slytherins hate hufflepuffs but hufflepuffs are great finders)
;;    "test 5")
;;   (check-equal?
;;    ((matcherbot-creator '(hufflepuffs are great))
;;     '(hufflepuffs are great finders))
;;    '(finders)
;;    "test 6")
;;   (check-equal?
;;    ((matcherbot-creator '(hufflepuffs are great))
;;     '(are great finders))
;;    #f
;;    "test 7")
;;   )

(define (matcherbot-creator pattern)
  (define (helper sent my-pattern)
    (cond ((empty? my-pattern) sent)
          ((empty? sent) #f)
          ((equal? (first my-pattern) (first sent)) (helper (bf sent) (bf my-pattern)))
          (else (helper (bf sent) pattern))))
  (lambda (sent) (helper sent pattern)))

;; Q4 - substitutebot-creator
;; Write a procedure called substitutebot-creator that takes in two same-length sentences,
;; FROM and TO, and outputs a ChatterBot.
;; The first item of FROM and TO are associated, as
;; are the second two items, the third, etc. The outputted ChatterBot will take in
;; a sentence, SENT, and output a new sentence where every word in SENT that is a
;; member of FROM is replaced with its corresponding word in TO. If FROM and TO
;; are empty sentences, the ChatterBot will simply return SENT
;; (test-case
;;     "substitutebot-creator"
;;   (check-equal?
;;    ((substitutebot-creator '(bad ugly stupid hate sad mad disgusting) '(good pretty smart lov happy calm delicious)) '(bad ugly stupid))
;;    '(good pretty smart)
;;    "test 1")
;;   ;; Add more tests here
;;   (check-equal?
;;    ((substitutebot-creator '(bad ugly stupid hate sad mad disgusting) '(good pretty smart lov happy calm delicious)) '())
;;    '()
;;    "test 2")
;;   (check-equal?
;;    ((substitutebot-creator '(bad ugly stupid hate sad mad disgusting) '(good pretty smart lov happy calm delicious)) '(hello there!))
;;    '(hello there!)
;;    "test 3")
;;   )

(define (substitutebot-creator from to)
  (define (helper sent fr t)
    (cond ((empty? sent) '())
          ((empty? fr) (se (first sent) (helper (bf sent) from to)))
          ((equal? (first sent) (first fr)) (se (first t) (helper (bf sent) from to)))
          (else (helper sent (bf fr) (bf t)))))
  (lambda (sent) (helper sent from to)))

;; Q5 - switcherbot
;; A list of viewpoints:
;; 	me <-> you
;; 	I <-> you
;; 	am <-> are
;; 	was <-> were
;; 	my <-> your
;; 	yours <-> mine

;; switcherbot takes in a sentence SENT and outputs SENT with its viewpoint switched.
;; (Hint: How can you use a previous problem to help you solve this problem?)

;; (test-case
;;   "switcherbot"
;;   (check-equal?
;;     (switcherbot '(you are smart but I am smarter than you))
;;     '(I am smart but you are smarter than me)
;;     "test 1")
;;   ;; Add more tests here
;;   (check-equal?
;;     (switcherbot '(I am smart but you are smarter than me))
;;     '(you are smart but me am smarter than you)
;;     "test 2")
;;   (check-equal?
;;     (switcherbot '(my cat is smart but yours is smarter than mine))
;;     '(your cat is smart but mine is smarter than yours)
;;     "test 3")
;; )
(define (switcherbot sent)
  (define helper (substitutebot-creator '(me I am was my mine you are were your yours) '(you you are were your yours me am was my mine)))
  (if (equal? (first sent) 'you)
      (se 'I (helper (bf sent)))
      (helper sent)))

;; Q6 - inquisitivebot
;; It will take in a sentence SENT in the first person and rephrase it into a question
;; in the second person. The question mark should not be part of the last word. If
;; SENT is the empty sentence, inquisitivebot should return the empty sentence with
;; no question mark.

;; (test-case
;;     "inquisitivebot"
;;   (check-equal?
;;    (inquisitivebot '(I am happy))
;;    '(you are happy ?)
;;    "test 1")
;;   (check-equal?
;;    (inquisitivebot '(I can see you))
;;    '(you can see me ?)
;;    "test 2")
;;   ;; Add more tests here
;;   )
(define (inquisitivebot sent)
  (if (empty? sent)
      ('())
      (se (switcherbot sent) '?)))

;; Q7 - eliza
	;; - eliza will reply '(hello there!) to any sentence where the first word is 'hello.
	;; - It will reply to statements that contain '(I am) by:
	;; 	- removing '(I am) and anything before it from the beginning of the sentence
	;; 	- switching the perspective of everything after
	;; 	- prepending '(why are you) to the beginning of the sentence
	;; - If you ask a question, i.e. any statement where the last word is '?, eliza
  ;;       will reply with '(I can not answer your question.)
	;; - If the empty sentence is entered, eliza should answer with
  ;;       (how can I help you ?).
	;; - If any other statement is made, eliza should reply with the same statement
  ;;       except with its perspective switched.

;; (test-case
;;   "eliza"
;;   (check-equal?
;;     (eliza '(hello))
;;     '(hello there!)
;;     "test 1")
;;   (check-equal?
;;     (eliza '(I am tired of being bullied at school))
;;     '(why are you tired of being bullied at school ?)
;;     "test 2")
;;   (check-equal?
;;     (eliza '(how are you today ?))
;;     '(I can not answer your question.)
;;     "test 3")
;;   (check-equal?
;;     (eliza '())
;;     '(how can I help you ?)
;;     "test 4")
;;   ;; Add more tests here
;;   (check-equal?
;;     (eliza '(I want to talk to you))
;;     '(you want to talk to me)
;;     "test 5")
;; )
(define (eliza sent)
  (cond ((empty? sent) '(how can I help you ?))
        ((equal? (first sent) 'hello) '(hello there!))
        ((equal? (last sent) '?) '(I can not answer your question.))
        (((matcherbot-creator '(I am)) sent) (se '(why are you) (inquisitivebot ((matcherbot-creator '(I am)) sent))))
        (else (switcherbot sent))))


;; Q8 - reactorbot-creator
;; create procedure called reactorbot-creator that takes a ChatterBot BOT, a
;; sentence PAT, and a sentence OUT. This will output another ChatterBot that
;; works just like BOT except when PAT is in the input. When PAT is in the input,
;; return OUT.

;; (test-case
;;     "reactorbot-creator"
;;   (check-equal?
;;    ((reactorbot-creator (stupidbot-creator '(I am Groot)) '(no Groot youll die why are you doing this) '(WE are Groot)) '(whats up Groot))
;;    '(I am Groot)
;;    "test 1")
;;   (check-equal?
;;    ((reactorbot-creator (stupidbot-creator '(I am Groot)) '(no Groot youll die why are you doing this) '(WE are Groot)) '(no Groot youll die why are you doing this))
;;    '(WE are Groot)
;;    "test 2")
;;   ;; Add more tests here
;;   (check-equal?
;;    ((reactorbot-creator (matcherbot-creator '(I am Groot)) '(no Groot youll die why are you doing this) '(WE are Groot)) '(I am Groot hi))
;;    '(hi)
;;    "test 3")
;;   (check-equal?
;;    ((reactorbot-creator (matcherbot-creator '(I am Groot)) '(no Groot youll die why are you doing this) '(WE are Groot)) '(no Groot youll die why are you doing this))
;;    '(WE are Groot)
;;    "test 3")
;;   )
(define (reactorbot-creator bot pat out)
  (lambda (sent)
    (if (equal? sent pat)
        out
        (bot sent))))

;; Q9 - replacerbot-creator

;; The replacerbot-creator acts similarly to the
;; reactorbot-creator. It takes in a ChatterBot BOT, a sentence PAT, a sentence
;; BEFORE, a sentence AFTER and returns a new ChatterBot. This ChatterBot
;; should act the same as BOT, except when it finds PAT anywhere in the input sentence, where it should:

;; 	- remove the first instance of PAT and everything before it from SENT
;; 	- prepend BEFORE to the beginning of SENT
;; 	- append AFTER to the end of SENT

;; (test-case
;;     "replacerbot-creator"
;;   (check-equal?
;;    ((replacerbot-creator (lambda (sent) (if (member? '? sent) '(I dont know) '(thats nice))) '(I am) '(hi) '(im dadbot)) '(youre pretty dumb))
;;    '(thats nice)
;;    "test 1")
;;   (check-equal?
;;    ((replacerbot-creator (lambda (sent) (if (member? '? sent) '(I dont know) '(thats nice))) '(I am) '(hi) '(im dadbot)) '(I am hungry))
;;    '(hi hungry im dadbot)
;;    "test 2")
;;   ;; Add more tests here
;;   )
(define (replacerbot-creator bot pat before after)
  (lambda (sent)
      (if ((matcherbot-creator pat) sent)
          (se before ((matcherbot-creator pat) sent) after)
          (bot sent))))

;; Q10 - exagerate

;; Write the procedure exaggerate that takes in a bot. BOT1 and a number N. It returns
;; a new bot, BOT. BOT takes in a sentence, and responds how BOT1 would except that
;; it prepends 'very' before any adjective in its response.
;; The bot inspects its output and repeats this process N times.

;; (test-case
;;     "exaggerate"
;;   (check-equal?
;;    ((exaggerate babybot 1) '(this soup is hot and tasty))
;;    '(this soup is very hot and very tasty)
;;    "test 1")
;;   ;; Add more tests here
;;   (check-equal?
;;    ((exaggerate babybot 2) '(this soup is hot and tasty))
;;    '(this soup is very very very hot and very very very tasty)
;;    "test 2")
;;   (check-equal?
;;    ((exaggerate babybot 0) '(this soup is hot and tasty))
;;    '(this soup is hot and tasty)
;;    "test 3")
;;   )
(define (exaggerate bot n)
  (define (helper sent)
    (cond ((empty? sent) '())
          ((adjective? (first sent)) (se 'very (first sent) (helper (bf sent)) ))
          (else (se (first sent) (helper (bf sent))))))
  (lambda (sent)
    ((repeated helper n) sent)))
;; REMEMBER TO ADD YOUR OWN TESTS TO GRADER.RKT!
;; END OF PROJECT 1

