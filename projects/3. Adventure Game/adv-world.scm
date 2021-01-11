(load "tables.scm")
(load "obj.scm")
(load "adv.scm")
;;;  Data for adventure game.  This file is adv-world.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting up the world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Soda (instantiate place 'Soda))
(define BH-Office (instantiate place 'BH-Office))
(define MJC-Office (instantiate place 'MJC-Office))
(define art-gallery (instantiate place 'art-gallery))
(define Pimentel (instantiate place 'Pimentel))
(define 61A-Lab (instantiate place '61A-Lab))
(define Sproul-Plaza (instantiate place 'Sproul-Plaza))
(define Telegraph-Ave (instantiate place 'Telegraph-Ave))
(define Noahs (instantiate place 'Noahs))
(define Intermezzo (instantiate place 'Intermezzo))
(define s-h (instantiate place 'sproul-hall))


(can-go Soda 'up art-gallery)
(can-go art-gallery 'down Soda)
(can-go art-gallery 'west BH-Office)
(can-go BH-Office 'east art-gallery)
(can-go art-gallery 'east MJC-Office)
(can-go MJC-Office 'west art-gallery)
(can-go Soda 'south Pimentel)
(can-go Pimentel 'north Soda)
(can-go Pimentel 'south 61A-Lab)
(can-go 61A-Lab 'north Pimentel)
(can-go 61A-Lab 'west s-h)
(can-go s-h 'east 61A-Lab)
(can-go Sproul-Plaza 'east s-h)
(can-go s-h 'west Sproul-Plaza)
(can-go Sproul-Plaza 'north Pimentel)
(can-go Sproul-Plaza 'south Telegraph-Ave)
(can-go Telegraph-Ave 'north Sproul-Plaza)
(can-go Telegraph-Ave 'south Noahs)
(can-go Noahs 'north Telegraph-Ave)
(can-go Noahs 'south Intermezzo)
(can-go Intermezzo 'north Noahs)

;; Some people.
; MOVED above the add-entry-procedure stuff, to avoid the "The computers
; seem to be down" message that would occur when hacker enters 61a-lab
; -- Ryan Stejskal

(define Brian (instantiate person 'Brian BH-Office))
(define hacker (instantiate person 'hacker 61A-Lab))
(define nasty (instantiate thief 'nasty Sproul-Plaza))

;; Question A3
;; whenever a person goes to a new place, the place gets an 'ENTER message.
;; the place the person previously inhabited gets an 'EXIT message.
;; When the place gets the message, it calls each procedure on its list of
;; ENTRY-PROCEDURES or EXIT-PROCEDURES as appropriate.
;; Places have the following methods defined for manipulating these lists of procedures:
;; ADD-ENTRY-PROCEDURE, ADD-EXIT-PROCEDURE, REMOVE-ENTRY-PROCEDURE, REMOVE-EXIT-PROCEDURE,
;; and CLEAR-ALL-PROCS. You can read their definitions in the code.

;; Sproul Hall has a particularly obnoxious exit procedure attached to it.
;; Fix sproul-hall-exit so that it counts how many times it gets called,
;; and stops being obnoxious after the third time.

;; Remember that the exit-procs list contains procedures, not names of procedures!
;; It's not good enough to redefine sproul-hall-exit,
;; since Sproul Hall's list of exit procedures still contains the old procedure.
;; The best thing to do is just to load adv-world.scm again,
;; which will define a new Sproul Hall and add the new exit procedure.

;; (define (sproul-hall-exit)
;;    (error "You can check out any time you'd like, but you can never leave"))

(define sproul-hall-exit
	(let ((x 0))
		(lambda ()
			(set! x (+ x 1))
			(if (> x 3)
					(print "You can leave now")
					(error "You can check out any time you'd like, but you can never leave")))))


(define (bh-office-exit)
  (print "What's your favorite programming language?")
  (let ((answer (read)))
    (if (eq? answer 'scheme)
	(print "Good answer, but my favorite is Logo!")
	(begin (newline) (bh-office-exit)))))

(ask s-h 'add-entry-procedure
 (lambda () (print "Miles and miles of students are waiting in line...")))
(ask s-h 'add-exit-procedure sproul-hall-exit)
(ask BH-Office 'add-exit-procedure bh-office-exit)
(ask Noahs 'add-entry-procedure
 (lambda () (print "Would you like lox with it?")))
(ask Noahs 'add-exit-procedure
 (lambda () (print "How about a cinnamon raisin bagel for dessert?")))
(ask Telegraph-Ave 'add-entry-procedure
 (lambda () (print "There are tie-dyed shirts as far as you can see...")))
(ask 61A-Lab 'add-entry-procedure
 (lambda () (print "The computers seem to be down")))
(ask 61A-Lab 'add-exit-procedure
 (lambda () (print "The workstations come back to life just in time.")))

;; Some things.

(define bagel (instantiate thing 'bagel))
(ask Noahs 'appear bagel)

(define coffee (instantiate thing 'coffee))
(ask Intermezzo 'appear coffee)


;; Q1
;; Instantiate a new Person object to represent yourself.
;; Put yourself in a new place called dormitory (or wherever you live)
;; and connect it to campus so that it is a reachable place.
;; Create a place called kirin, north of soda. Put a thing called potstickers there.
;; Then give the necessary commands to move your character to kirin,
;; take the potstickers, then move yourself to where Brian is,
;; put down the potstickers, and have Brian take them.
;; Then go back to the lab and get back to work.

;; Q1 answer
;; define dormitory
(define dormitory (instantiate place 'dormitory))

;; connect dormitory to pimentel
(can-go dormitory 'east Pimentel)
(can-go Pimentel 'west dormitory)

;; define mey in dormitory
(define mey (instantiate person 'Mey dormitory))

;; define kirin
(define kirin (instantiate place 'Kirin))

;; connect kirin to soda
(can-go kirin 'south Soda)
(can-go Soda 'north kirin)

;; define potstickers and put it in kirin
(define potstickers (instantiate thing 'potstickers))
(ask kirin 'appear potstickers)

;; command mey to go to kirin from dormitory
(ask mey 'go 'east)
(ask mey 'go 'north)
(ask mey 'go 'north)

;; mey takes potstickers
(ask mey 'take potstickers)

;; mey goes to bh
(ask mey 'go 'south)
(ask mey 'go 'up)
(ask mey 'go 'west)

;; mey put potstickers
(ask mey 'lose potstickers)

;; when mey goes to some place, place will have his things
;; (ask BH-Office 'appear potstickers)

;; brian takes the potstickers
(ask Brian 'take potstickers)

;; mey goes to lab
(ask mey 'go 'east)
(ask mey 'go 'down)
(ask mey 'go 'south)
(ask mey 'go 'south)

;; -----------------------------------------------------------------------------

;; Q2a What kind of thing is the value of variable Brian?
;; Hint: What is returned by STk in the following situation:
;; > Brian
;; answer: procedure

;; Q2b List all the messages that a Place understands.
;; (You might want to maintain such a list for your own use,
;; for every type of object, to help in the debugging effort.)

;; answer:
;; class: place:
;; instantiation vars: name
;; instance vars: directions-and-neighbors people entry-procs exit-procs
;; methods: type neigbors exits look-in appear enter gone exit new-neigbor
;;          add-entry-procedure add-exit-procedure remove-entry-procedure remove-exit-procedure
;;          clear-all-procs

;; class: person
;; instantiation vars: name place
;; instance vars: possessions saying
;; methods: (type) (look-around) (take thing) (lose thing) (talk) (set-talk string)
;;          (exits) (notice person) (go direction)
;; init:    (ask place 'enter self)


;; Q2c We have been defining a variable to hold each object in our world.
;; For example, we defined bagel by saying:

;; > (define bagel (instantiate thing 'bagel))
;; This is just for convenience. Every object does not have to have a top-level definition.
;; Every object DOES have to be constructed and connected to the world.
;; For instance, suppose we did this:

;; > (can-go Telegraph-Ave 'east (instantiate place 'Peoples-Park))

;; ;;; assume Brian is at Telegraph
;; > (ask Brian 'go 'east)
;; What is returned by the following expressions and why?

;; > (ask Brian 'place)
;; answer: procedure. place is an instance var for person returning a place object

;; > (let ((where (ask Brian 'place)))
;;        (ask where 'name))
;; answer: Peoples-Park: the place is found and its name is asked

;; > (ask Peoples-park 'appear bagel)
;; answer: error. because People-park is an object and not bound to any reference


;; Q2d The implication of all this is that there can be multiple names for objects.
;; One name is the value of the object's internal name variable.
;; In addition, we can define a variable at the top-level to refer to an object.
;; Moreover, one object can have a private name for another object.
;; for example, Brian has a variable place which is currently bound to the object
;; that represents People's Park. Some examples to think about:

;; > (eq? (ask Telegraph-Ave 'look-in 'east) (ask Brian 'place))
;; #t
;; > (eq? (ask Brian 'place) 'Peoples-Park)
;; #f
;; > (eq? (ask (ask Brian 'place) 'name) 'Peoples-Park)
;; #t

;; Suppose we type the following into STk:

;; > (define computer (instantiate thing 'Durer))
;; Which of the following is correct? Why?

;; (ask 61A-Lab 'appear computer)
;; or

;; (ask 61A-Lab 'appear Durer)
;; or

;; (ask 61A-Lab 'appear 'Durer)

;; What is returned by (computer 'name)? Why?

;; Answer:
;; we want the object name to appear in the place not its internal name.
;; so the first one is correct.

;; by (computer 'name) a procedure is returned.
;; calling an object on a message returns the dispatch
;; calling ask on the dispatch runs the method associated by that message


;; codes for Qa4 part 1

(define singer (instantiate person 'rick Sproul-Plaza))
(ask singer 'set-talk "My funny valentine, sweet comic valentine")
(define preacher (instantiate person 'preacher Sproul-Plaza))
(ask preacher 'set-talk "Praise the Lord")
(define street-person (instantiate person 'harry Telegraph-Ave))
(ask street-person 'set-talk "Brother, can you spare a buck")

;; codes for Qa4 part 2

(define warehouse (instantiate locked-place 'warehouse))
(can-go Intermezzo 'south warehouse)
(can-go warehouse 'north Intermezzo)

;; code for qa5
;; garages
(define g1 (instantiate garage 'g1))
(define g2 (instantiate garage 'g2))
(can-go g1 'north 61A-Lab)
(can-go 61A-Lab 'south g1)
(can-go g2 'west Pimentel)
(can-go Pimentel 'east g2)

;; vehicles
(define bmw (instantiate thing 'bmw))
(define glx (instantiate thing 'glx))
(define toyota (instantiate thing 'toyota))
(define honda (instantiate thing 'honda))

;; vehicles in places
(ask Pimentel 'appear bmw)
(ask Pimentel 'appear glx)
(ask Pimentel 'appear toyota)
(ask g1 'appear honda)									; a vehicle without owner

;; people
(define jj (instantiate person 'jj Pimentel))
(ask mey 'go 'north)										; mey at pimentel

(ask mey 'take toyota)
(ask jj 'take bmw)

;; checks if park happens if vehicle not there
;; (ask g2 'park bmw)

(ask jj 'go 'east)

(ask g2 'park bmw)
(ask jj 'go 'west)
;; checks if unpark happens if the ticket owner is not there
;; (ask g2 'unpark (car (ask jj 'possessions)))

(ask jj 'go 'east)
(ask g2 'unpark (car (ask jj 'possessions)))

;; checks parking a vehicle without owner
;; (ask g2 'park honda)


;; there are other things to check by maybe later
