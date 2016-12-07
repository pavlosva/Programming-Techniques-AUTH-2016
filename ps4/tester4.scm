#lang racket

;;; ---------------------------------------------------------------------------
;;; Simple object system with inheritance

(define (ask object message . args)  ;; See your Scheme manual to explain `.'
  (let ((method (get-method object message)))
    (if (method? method)
	(apply method (cons object args))
	(error "No method" message (cadr method)))))

(define (get-method object message)
  (object message))

(define (no-method name)
  (list 'no-method name))

(define (method? x)
  (not (no-method? x)))

(define (no-method? x)
  (if (pair? x)
      (eq? (car x) 'no-method)
      false))

;;; ----------------------------------------------------------------------------
;;; Persons, places, and things will all be kinds of named objects

(define (make-named-object name)
  (lambda (message) 
    (cond ((eq? message 'name) (lambda (self) name))
	  (else (no-method name)))))

;;; Persons and things are mobile since their places can change

(define (make-mobile-object name location)
  (let ((named-obj (make-named-object name)))
    (lambda (message)
      (cond ((eq? message 'place)    (lambda (self) location))
	    ((eq? message 'install)
	     (lambda (self)
	       (ask location 'add-thing self)))	; Synchonize thing and place
	    ;; Following method should never be called by the user...
	    ;;  it is a system-internal method.
	    ;; See CHANGE-PLACE instead
	    ((eq? message 'set-place)
	     (lambda (self new-place)
	       (set! location new-place)
	       'place-set))
	    (else (get-method named-obj message))))))

(define (make&install-mobile-object name place)
  (let ((mobile-obj (make-mobile-object name place)))
    (ask mobile-obj 'install)
    mobile-obj))

;;; A thing is something that can be owned

(define (make-thing name birthplace)
  (let ((owner     'nobody)
	(mobile-obj (make-mobile-object name birthplace)))
    (lambda (message)
      (cond ((eq? message 'owner)    (lambda (self) owner))
	    ((eq? message 'ownable?) (lambda (self) true))
	    ((eq? message 'owned?)
	     (lambda (self)
	       (not (eq? owner 'nobody))))
	    ;; Following method should never be called by the user...
	    ;;  it is a system-internal method.
	    ;; See TAKE and LOSE instead.
	    ((eq? message 'set-owner)
	     (lambda (self new-owner)
	       (set! owner new-owner)
	       'owner-set))
	    (else (get-method mobile-obj message))))))

(define (make&install-thing name birthplace)	
  (let ((thing  (make-thing name birthplace)))
    (ask thing 'install)
    thing))

;;; Implementation of places

(define (make-place name)
  (let ((neighbor-map '())		
	(things       '())
	(named-obj (make-named-object name)))
    (lambda (message)
      (cond ((eq? message 'things) (lambda (self) things))
	    ((eq? message 'neighbors)
	     (lambda (self) (map cdr neighbor-map)))
	    ((eq? message 'exits)
	     (lambda (self) (map car neighbor-map)))
	    ((eq? message 'neighbor-towards)
	     (lambda (self direction)
	       (let ((places (assq direction neighbor-map)))
		 (if places
		     (cdr places)
		     false))))
            ((eq? message 'add-neighbor)
             (lambda (self direction new-neighbor)
               (cond ((assq direction neighbor-map)
                      (display-message (list "Direction already assigned"
					      direction name))
		      false)
                     (else
                      (set! neighbor-map
                            (cons (cons direction new-neighbor) neighbor-map))
		      true))))
	    ((eq? message 'accept-person?)
	     (lambda (self person)
	       true))
 
	    ;; Following two methods should never be called by the user...
	    ;;  they are system-internal methods. See CHANGE-PLACE instead.
            ((eq? message 'add-thing)
             (lambda (self new-thing)
               (cond ((memq new-thing things)
                      (display-message (list (ask new-thing 'name)
					     "is already at" name))
		      false)
                     (else (set! things (cons new-thing things))
			   true))))
            ((eq? message 'del-thing)
             (lambda (self thing)
               (cond ((not (memq thing things))
                      (display-message (list (ask thing 'name)
					     "is not at" name))
		      false)
                     (else (set! things (delq thing things))	;; DELQ defined
			   true))))                             ;; below

            (else (get-method named-obj message))))))

;;; ----------------------------------------------------------------------------
;;; Implementation of people

(define (make-person name birthplace threshold)
  (let ((possessions '())
	(mobile-obj  (make-mobile-object name birthplace)))
    (lambda (message)
      (cond ((eq? message 'person?)     (lambda (self) true))
	    ((eq? message 'possessions) (lambda (self) possessions))
	    ((eq? message 'list-possessions)
	     (lambda (self)
	       (ask self 'say
		    (cons "I have"
			  (if (null? possessions)
			      '("nothing")
			      (map (lambda (p) (ask p 'name))
				      possessions))))
	       possessions))
	    ((eq? message 'say)
	     (lambda (self list-of-stuff)
	       (display-message
		 (append (list "At" (ask (ask self 'place) 'name)
			       ":"  name "says --")
			 (if (null? list-of-stuff)
			     '("Oh, nevermind.")
			     list-of-stuff)))
	       'said))
	    ((eq? message 'have-fit)
	     (lambda (self)
	       (ask self 'say '("Yaaaah! I am upset!"))
	       'I-feel-better-now))
	    ((eq? message 'look-around)
	     (lambda (self)
	       (let ((other-things
		       (map (lambda (thing) (ask thing 'name))
                               (delq self                       ;; DELQ
                                     (ask (ask self 'place)     ;; defined
                                          'things)))))          ;; below
                 (ask self 'say (cons "I see" (if (null? other-things)
						  '("nothing")
						  other-things)))
		 other-things)))

	    ((eq? message 'take)
	     (lambda (self thing)
	       (cond ((memq thing possessions)
		      (ask self 'say
			   (list "I already have" (ask thing 'name)))
		      true)
		     ((and (let ((things-at-place (ask (ask self 'place) 'things)))
			     (memq thing things-at-place))
			   (is-a thing 'ownable?))
		      (if (ask thing 'owned?)
			  (let ((owner (ask thing 'owner)))
			    (ask owner 'lose thing)
			    (ask owner 'have-fit))
			  'unowned)

		      (ask thing 'set-owner self)
		      (set! possessions (cons thing possessions))
		      (ask self 'say
			   (list "I take" (ask thing 'name)))
		      true)
		     (else
		      (display-message
		       (list "You cannot take" (ask thing 'name)))
		      false))))
	    ((eq? message 'lose)
	     (lambda (self thing)
	       (cond ((eq? self (ask thing 'owner))
		      (set! possessions (delq thing possessions)) ;; DELQ
		      (ask thing 'set-owner 'nobody)              ;; defined
		      (ask self 'say                              ;; below
			   (list "I lose" (ask thing 'name)))
		      true)
		     (else
		      (display-message (list name "does not own"
					     (ask thing 'name)))
		      false))))
	    ((eq? message 'move)
	     (lambda (self)
	       (cond ((= (random threshold) 0)
		      (ask self 'act)
		      true))))
	    ((eq? message 'act)
	     (lambda (self)
	       (let ((new-place (random-neighbor (ask self 'place))))
		 (if new-place
		     (ask self 'move-to new-place)
		     false))))		; All dressed up and no place to go

	    ((eq? message 'move-to)
	     (lambda (self new-place)
	       (let ((old-place (ask self 'place)))
		 (cond ((eq? new-place old-place)
			(display-message (list name "is already at"
					       (ask new-place 'name)))
			false)
		       ((ask new-place 'accept-person? self)
			(change-place self new-place)
			(for-each (lambda (p) (change-place p new-place))
				  possessions)
			(display-message
			  (list name "moves from" (ask old-place 'name)
				     "to"         (ask new-place 'name)))
			(greet-people self (other-people-at-place self new-place))
			true)
		       (else
			(display-message (list name "can't move to"
					       (ask new-place 'name))))))))
	    ((eq? message 'go)
	     (lambda (self direction)
	       (let ((old-place (ask self 'place)))
		 (let ((new-place (ask old-place 'neighbor-towards direction)))
		   (cond (new-place
			  (ask self 'move-to new-place))
			 (else
			  (display-message (list "You cannot go" direction
						 "from" (ask old-place 'name)))
			  false))))))
	    ((eq? message 'install)
	     (lambda (self)
	       (add-to-clock-list self)
	       ((get-method mobile-obj 'install) self)))
	    (else (get-method mobile-obj message))))))
  
(define (make&install-person name birthplace threshold)
  (let ((person (make-person name birthplace threshold)))
    (ask person 'install)
    person))

;;; A troll is a kind of person (but not a kind person!)

(define (make-troll name birthplace threshold)
  (let ((person (make-person name birthplace threshold)))
    (lambda (message)
      (cond ((eq? message 'act)
	     (lambda (self)
	       (let ((others (other-people-at-place self (ask self 'place))))
		 (if (not (null? others))
		     (ask self 'eat-person (pick-random others))
		     ((get-method person 'act) self)))))
	    ((eq? message 'eat-person)
	     (lambda (self person)
	       (ask self 'say
		    (list "Growl.... I'm going to eat you,"
			  (ask person 'name)))
	       (go-to-heaven person)
	       (ask self 'say
		    (list "Chomp chomp." (ask person 'name)
			  "tastes yummy!"))
	       '*burp*))
	    (else (get-method person message))))))

(define (make&install-troll name birthplace threshold)
  (let ((troll  (make-troll name birthplace threshold)))
    (ask troll 'install)
    troll))


(define (go-to-heaven person)
  (for-each (lambda (item) (ask person 'lose item))
	    (ask person 'possessions))
  (ask person 'say
       '("
                   Dulce et decorum est 
                   pro computatore mori!"
	 ))
  (ask person 'move-to heaven)
  (remove-from-clock-list person)
  'game-over-for-you-dude)

(define heaven (make-place 'heaven))		; The point of no return

;;; --------------------------------------------------------------------------
;;; Clock routines

(define *clock-list* '())
(define *the-time* 0)

(define (initialize-clock-list)
  (set! *clock-list* '())
  'initialized)

(define (add-to-clock-list person)
  (set! *clock-list* (cons person *clock-list*))
  'added)

(define (remove-from-clock-list person)
  (set! *clock-list* (delq person *clock-list*))  ;; DELQ defined below
  'removed)

(define (clock)
  (newline)
  (display "---Tick---")
  (set! *the-time* (+ *the-time* 1))
  (for-each (lambda (person) (ask person 'move))
	    *clock-list*)
  'tick-tock)
	     

(define (current-time)
  *the-time*)

(define (run-clock n)
  (cond ((zero? n) 'done)
	(else (clock)
	      (run-clock (sub1 n)))))

;;; --------------------------------------------------------------------------
;;; Miscellaneous procedures

(define (is-a object property)
  (let ((method (get-method object property)))
    (if (method? method)
	(ask object property)
	false)))

(define (symbol-append s1 s2)
  (string->symbol (string-append (symbol->string s1)
                                 (symbol->string s2))))

(define (change-place mobile-object new-place)	; Since this bridges the gap
  (let ((old-place (ask mobile-object 'place))) ; between MOBILE-OBJECT and
    (ask mobile-object 'set-place new-place)	; PLACE, it is best it not
    (ask old-place 'del-thing mobile-object))	; be internal to either one.
  (ask new-place 'add-thing mobile-object)
  'place-changed)

(define (other-people-at-place person place)
  (filter (lambda (object)
	    (if (not (eq? object person))
		(is-a object 'person?)
		false))
	  (ask place 'things)))

(define (greet-people person people)
  (if (not (null? people))
      (ask person 'say
	   (cons "Hi"
		 (map (lambda (p) (ask p 'name))
			 people)))
      'sure-is-lonely-in-here))

(define (display-message list-of-stuff)
  (newline)
  (for-each (lambda (s) (display s) (display " "))
	    list-of-stuff)
  'message-displayed)

(define (random-neighbor place)
  (pick-random (ask place 'neighbors)))

(define (filter predicate lst)
  (cond ((null? lst) '())
	((predicate (car lst))
	 (cons (car lst) (filter predicate (cdr lst))))
	(else (filter predicate (cdr lst)))))

(define (pick-random lst)
  (if (null? lst)
      false
      (list-ref lst (random (length lst)))))  ;; See manual for LIST-REF

(define (delq item lst)
  (cond ((null? lst) '())
	((eq? item (car lst)) (delq item (cdr lst)))
	(else (cons (car lst) (delq item (cdr lst))))))

;;; -------------------------------------------------------------------
;;; Other interesting procedures for PS6, Spring 95

(define (make&install-sd-card name birthplace id)
  (let ((card (make-sd-card name birthplace id)))
    (ask card 'install)
    card))

(define (make-sd-card name birthplace idnumber)
  (let ((id idnumber)
        (thing (make-thing name birthplace)))
    (lambda (message)
      (cond ((eq? message 'sd-card?) (lambda (self) true))
            ((eq? message 'id) (lambda (self) id))
            (else (get-method thing message))))))

(define (copy-sd-card card)
  (let ((name   (symbol-append 'copy-of- (ask card 'name)))
	(place  (ask card 'place))
	(id     (ask card 'id)))
    (make&install-sd-card name place id)))




(initialize-clock-list)

;; Here we define the places in our world...
;;------------------------------------------

(define EGG-Atrium   (make-place 'EGG-Atrium))
(define dungeon      (make-place 'dungeon))
(define Building-36  (make-place 'Building-36))
(define computer-lab (make-place 'computer-lab))
(define Tech-Square  (make-place 'Tech-Square))
(define gerry-office  (make-place 'gerry-office))
(define albert-office  (make-place  'albert-office))
(define dormitory    (make-place 'dormitory))

;; One-way paths connect individual places in the world.
;;------------------------------------------------------

(define (can-go from direction to)
  (ask from 'add-neighbor direction to))

(define (can-go-both-ways from direction reverse-direction to)
  (can-go from direction to)
  (can-go to reverse-direction from))

(can-go-both-ways Building-36   'up    'down  computer-lab)
(can-go-both-ways Building-36   'north 'south Tech-Square)
(can-go-both-ways Building-36   'west  'east  EGG-Atrium)
(can-go-both-ways Tech-Square   'up    'down  albert-office)
(can-go-both-ways albert-office 'up    'down  gerry-office)
(can-go-both-ways dormitory     'west  'east  Building-36)

(can-go dungeon      'up    EGG-Atrium)

;; The important critters in our world...
;;---------------------------------------

(define albert   (make&install-person 'albert albert-office 3))
(define gerry    (make&install-person 'gerry  gerry-office  2))

(define grendel  (make&install-troll  'grendel dungeon     4))

(define gerry-card
  (make&install-sd-card 'gerry-card gerry-office '888-12-3456))
(define albert-card
  (make&install-sd-card 'albert-card albert-office '888-98-7654))

;; The beginning of an ever-expanding game script
;;------------------------------------------------

(include "ps4.scm")

(define laptop (make&install-thing 'laptop dormitory))
(define phone (make&install-thing 'phone dormitory))
(define dave (make&install-person 'dave dormitory 1000))

;; in order to test ex 2
(define card-place (make-card-locked-place 'card-place))
(can-go-both-ways card-place 'up 'down Building-36)
(define card1 (make&install-sd-card 'card1 dormitory '1234c))

;; in order to test ex3
(define safe-residence (make-student-residence 'safe-residence))
(define residence-card (make&install-sd-card 'residence-card safe-residence 'mit-7217))
(define administrator-card (make&install-sd-card 'administrator-card safe-residence 'mit-1547))
(can-go-both-ways safe-residence 'up 'down card-place)
(define forged-residence-card (make&install-sd-card 'forged-residence-card Building-36 'mit-7217))

;; in order to test ex4
(define cellar (make-place 'cellar))
(define broom-closet (make-place 'closet))
(can-go-both-ways cellar 'up 'down broom-closet)
(define stolen-card (make&install-sd-card 'stolen-card cellar 'mit-5931))

;; in order to test ex5
(define big-eye (make-big-brother 'big-eye))
(define bio-lab (make-surveillance-room 'bio-lab big-eye))
(define library (make-surveillance-room 'lib big-eye))
(define foyer (make-place 'foyer))
(define suspicious-card (make&install-sd-card 'suspicious-card bio-lab 'mit-7180))
(define suspicious-card-copy (make&install-sd-card 'suspicious-card-copy library 'mit-7180))
(define other-card (make&install-sd-card 'other-card library 'mit-1052))
(define other-card-copy (make&install-sd-card 'other-card foyer 'mit-1052))
(define forged-suspicious-card (make&install-sd-card 'forged-suspicious-card foyer 'mit-7180))

(define (play-game)
  (ask gerry 'take gerry-card)
  (ask gerry 'go 'down)
  (ask gerry 'go 'down)
  )

(define (test-flip)
  (define flip1 (make-flip))
  (define flip2 (make-flip))
  (displayln (flip1))
  (displayln (flip1))
  (displayln (flip1))
  (displayln (flip2))
  (displayln (flip1)))

(define (take-card)
  (ask dave 'take laptop) ;; Take something that's not a card to make sure all objects are checked
  (ask dave 'move-to card-place)
  (ask dave 'take card1)
  (ask dave 'take phone)
  (ask dave 'move-to card-place))

(define (get-in-residence)
  (newline) (display (ask safe-residence 'register-card forged-residence-card))
  (ask safe-residence 'register-card administrator-card)
  (ask dave 'move-to Building-36)
  (ask dave 'take forged-residence-card) ;; It's forged, but it'll work
  (ask dave 'move-to safe-residence)
  (ask safe-residence 'register-card residence-card)
  (ask dave 'move-to safe-residence))
  
(define (take-stolen-card)
  (ask gerry 'move-to cellar)
  (ask albert 'move-to cellar)
  (ask gerry 'take stolen-card)
  (ask (report-stolen-card 'mit-5931) 'move-to cellar) ;; Move the ogre to the cellar by force
  (run-clock 3))

(define (get-in-surveillance)
  (ask dave 'move-to foyer)
  (ask bio-lab 'register-card suspicious-card)
  (ask library 'register-card suspicious-card-copy)
  (ask library 'register-card other-card)

  ;; Add a few dummy entries to make sure big-brother does not report false positives
  (ask dave 'move-to bio-lab)
  (ask dave 'take forged-suspicious-card)
  (ask dave 'move-to library)
  (set! *the-time* (add1 *the-time*)) ;; Add a clock tick
  (ask dave 'move-to foyer)
  (ask dave 'move-to bio-lab)
  (ask dave 'move-to foyer)
  (ask dave 'lose forged-suspicious-card)
  (ask dave 'take other-card-copy)
  (ask dave 'move-to library)
  (ask dave 'move-to foyer)
  (ask dave 'lose other-card-copy)
  (ask dave 'take forged-suspicious-card)
  (ask dave 'move-to library)

  (newline) (display (ask big-eye 'display-stolen-card)) ;; Should be empty

  (ask dave 'move-to foyer)
  (ask dave 'move-to library)

  (newline) (display (ask big-eye 'display-stolen-card)) ;; Should contain the one card
  (newline))

(define (take-suspicious-book)
  (ask dave 'move-to dormitory)
  (ask dave 'go 'north)
  (ask dave 'take suspicious-book)
  (ask dave 'go 'north))

;; ...now whenever you re-load this file, you can bring things up to
;; date by invoking PLAY-GAME.

;; Exercise 0
;;;;;;;;;;;;;;;;;;;;;;
;; EXPECTED OUTPUT  ;;
;; 1                ;;
;; 0                ;;
;; 1                ;;
;; 1                ;;
;; 0                ;;
;;;;;;;;;;;;;;;;;;;;;;
;(test-flip)

;; Exercise 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPECTED OUTPUT                                          ;;
;; At dormitory : myself says -- I take late-homework       ;;
;; myself moves from dormitory to Building-36               ;;
;; myself moves from Building-36 to Tech-Square             ;;
;; myself moves from Tech-Square to albert-office           ;;
;; At albert-office : myself says -- Hi albert              ;;
;; myself moves from albert-office to gerry-office          ;;
;; At gerry-office : myself says -- Hi gerry                ;;
;; At gerry-office : myself says -- I lose late-homework    ;;
;; At gerry-office : gerry says -- I take late-homework #t  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(give-gerry-my-homework)

;; Exercise 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPECTED OUTPUT                             ;;
;; At dormitory : dave says -- I take laptop   ;;
;; dave can't move to card-place               ;;
;; At dormitory : dave says -- I take card1    ;;
;; At dormitory : dave says -- I take phone    ;;
;; dave moves from dormitory to card-place #t  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(take-card)

;; Exercise 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPECTED OUTPUT                                             ;;
;; #f                                                          ;;
;; dave moves from card-place to Building-36                   ;;
;; At Building-36 : dave says -- I take forged-residence-card  ;;
;; dave can't move to safe-residence                           ;;
;; dave moves from Building-36 to safe-residence #t            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(get-in-residence)

;; Exercise 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPECTED OUTPUT (since the game is random, there might be slight differences)  ;;
;; gerry moves from gerry-office to cellar                                        ;;
;; albert moves from albert-office to cellar                                      ;;
;; At cellar : albert says -- Hi gerry                                            ;;
;; At cellar : gerry says -- I take stolen-card                                   ;;
;; ogremit-5931 moves from dungeon to cellar                                      ;;
;; At cellar : ogremit-5931 says -- Hi albert gerry                               ;;
;; ---Tick---                                                                     ;;
;; At cellar : ogremit-5931 says -- Thief!!!!! I'm going to eat you, gerry        ;;
;; At cellar : gerry says -- I lose stolen-card                                   ;;
;; At cellar : gerry says -- I lose late-homework                                 ;;
;; At cellar : gerry says --                                                      ;;
;;                    Dulce et decorum est                                        ;;
;;                    pro computatore mori!                                       ;;
;; gerry moves from cellar to heaven                                              ;;
;; At cellar : ogremit-5931 says -- Chomp chomp. gerry tastes yummy!              ;;
;;                                  ...                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(take-stolen-card)

;; Exercise 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPECTED OUTPUT                                        ;;
;; dave moves from safe-residence to foyer                ;;
;; dave can't move to bio-lab                             ;;
;; At foyer : dave says -- I take forged-suspicious-card  ;;
;; dave moves from foyer to lib                           ;;
;; dave moves from lib to foyer                           ;;
;; dave moves from foyer to bio-lab                       ;;
;; dave moves from bio-lab to foyer                       ;;
;; At foyer : dave says -- I lose forged-suspicious-card  ;;
;; At foyer : dave says -- I take other-card              ;;
;; dave moves from foyer to lib                           ;;
;; dave moves from lib to foyer                           ;;
;; At foyer : dave says -- I lose other-card              ;;
;; At foyer : dave says -- I take forged-suspicious-card  ;;
;; dave moves from foyer to lib                           ;;
;; ()                                                     ;;
;; dave moves from lib to foyer                           ;;
;; dave moves from foyer to lib                           ;;
;; (mit-7180)                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(get-in-surveillance)

;; Exercise 6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPECTED OUTPUT                                         ;;
;; dave moves from lib to dormitory                        ;;
;; You cannot go north from dormitory                      ;;
;; At dormitory : dave says -- A new passage opens north!  ;;
;; At dormitory : dave says -- I take suspicious-book      ;;
;; dave moves from dormitory to Tech-Square #t             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(take-suspicious-book)
