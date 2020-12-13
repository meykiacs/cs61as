(load "obj.scm")
(load "adv.scm")
(load "adv-world.scm")

;; q1
;; Instantiate a new Person object to represent yourself.
;; Put yourself in a new place called dormitory (or wherever you live)
;; and connect it to campus so that it is a reachable place.
;; Create a place called kirin, north of soda. Put a thing called potstickers there.
;; Then give the necessary commands to move your character to kirin,
;; take the potstickers, then move yourself to where Brian is,
;; put down the potstickers, and have Brian take them.
;; Then go back to the lab and get back to work.

;; answer
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

;; q2
;; a) What kind of thing is the value of variable Brian? Hint: What is returned by STk in the following situation:
;; > Brian
;; answer: procedure

;; b) List all the messages that a Place understands. (You might want to maintain such a list for your own use, for every type of object, to help in the debugging effort.)

;; answer:
;; instantiation vars: name
;; instance vars: directions-and-neighbors people entry-procs exit-procs
;; methods: type neigbors exits look-in appear enter gone exit new-neigbor
;;          add-entry-procedure add-exit-procedure remove-entry-procedure remove-exit-procedure
;;          clear-all-procs

;; c) We have been defining a variable to hold each object in our world. For example, we defined bagel by saying:

;; > (define bagel (instantiate thing 'bagel))
;; This is just for convenience. Every object does not have to have a top-level definition. Every object DOES have to be constructed and connected to the world. For instance, suppose we did this:

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
