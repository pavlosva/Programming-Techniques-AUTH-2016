 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                           Exercise 0                          ;;
 ;;                                                               ;;
 ;; Create an object that every time it's called, flips its state ;;
 ;;                        between 0 and 1                        ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-flip)
  (let ((value 0))             ;; every time make-flip is called, we initialize a new value to 0
    (lambda ()                 ;; make-flip returns a new procedure
      (set! value (- 1 value)) ;; switch the value between 1 and 0
      value)))                 ;; return the value

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                               Exercise 1                              ;;
 ;;                                                                       ;;
 ;; Install new character with free will, create late-homework object and ;;
 ;;                    start character in dormitory.                      ;;
 ;;                                                                       ;;
 ;;   Also create a function that finds gerry and gives him the homework  ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define myself
  (make&install-person 'myself dormitory 100))

(define late-homework
  (make&install-thing 'late-homework dormitory))

(define (give-gerry-my-homework)
  (ask myself 'take late-homework)
  (ask myself 'go 'west)
  (ask myself 'go 'north)
  (ask myself 'go 'up)
  (ask myself 'go 'up)
  (ask myself 'lose late-homework) ;; myself will get upset without this command
  (ask gerry 'take late-homework))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                       Exercise 2                       ;;
 ;;                                                        ;;
 ;; Creates a card locked place (overriding accept-person) ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-card-locked-place name)
  (let ((place (make-place name)))
    (lambda (message)
      (cond ((eq? message 'accept-person?)
             ;; check if person holds a card
             (lambda (self person) ;; 'accept-person? points to a procedure that takes the place and the person to check as arguments
               (let ((card-checker (lambda (object) (is-a object 'sd-card?))) ;; define a procedure (card-checker) that checks if an object is an sd-card
                     (possessions (ask person 'possessions))) ;; the objects held by the person
                 (findf card-checker possessions)))) ;; findf is a Racket function that looks if there is an item in possessions that satisfies card-checker
             (else (get-method place message))))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                             Exercise 3                            ;;
  ;;                                                                   ;;
  ;; Create student-residence class that accepts only persons carrying ;;
  ;;                        cards with valid IDs                       ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-student-residence name)
  (let ((place (make-card-locked-place name))
        (ids '()))
    (lambda (message)
      (cond ((eq? message 'accept-person?)
             (lambda (self person)
               (let* ((acceptable-id? (lambda (id) (member id ids))) ; a procedure that checks whether a card ID is accepted
                      (card-checker (lambda (object) (and (is-a object 'sd-card?) (acceptable-id? (ask object 'id))))) ;; a procedure that checks whether an object is an acceptable card
                      ;; Note that `and` will only evaluate (acceptable-id? ) if the first condition (is-a object 'sd-card?) is true!
                      ;; That means we won't run into any problems by calling 'id on an object that's not a card
                      ;; You can read more about this behaviour which is common in programming languages at https://docs.racket-lang.org/reference/if.html#%28form._%28%28lib._racket%2Fprivate%2Fletstx-scheme..rkt%29._and%29%29

                      (possessions (ask person 'possessions)))
                 (findf card-checker possessions)))) ;; Note that 'accept-person? returns #f if a card is not found, and the card itself if the person has a valid one
                                                     ;; This will be useful below
             ((eq? message 'register-card)
              (lambda (self card)
                (define id (ask card 'id)) ;; The card ID
                
                (if (equal? (ask card 'place) self) ;; Check if the card is inside this place (self)
                    (set! ids (cons id ids)) ;; Append the ID to the ids list
                    #f))) ;; Return false
             (else (get-method place message))))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                             Exercise 4                             ;;
 ;;                                                                    ;;
 ;;              Create ogre that hunts specific card id.              ;;
 ;; Also create procedure that reports a stolen card and start an ogre ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-ogre name birthplace threshold card-id)
  (let ((person (make-person name birthplace threshold)))
    (lambda (message)
      (cond ((eq? message 'act)
	     (lambda (self)
               (let* ((others (other-people-at-place self (ask self 'place))) ;; A list of anyone in the same room with the ogre
                      (card-checker (lambda (object) ;; A procedure that checks if an object is a stolen card
                                      (and (is-a object 'sd-card?) (eq? (ask object 'id) card-id))))
                      (person-checker (lambda (person) ;; A procedure that checks if a person holds a stolen card
                                        (findf card-checker (ask person 'possessions))))
                      (violator (findf person-checker others))) ;; The person who stole the card (or #f if there is no such person here)
                 (if violator
                     (ask self 'eat-person violator) ;; Eat the thief!
                     ((get-method person 'act) self))))) ;; Calls the 'act method of our parent subclass (note the definition of person in the first line of this procedure)
	    ((eq? message 'eat-person)
	     (lambda (self person)
	       (ask self 'say
		    (list "Thief!!!!! I'm going to eat you,"
			  (ask person 'name)))
	       (go-to-heaven person)
	       (ask self 'say
		    (list "Chomp chomp." (ask person 'name)
			  "tastes yummy!"))
	       '*burp*))
	    (else (get-method person message))))))

(define (make&install-ogre name birthplace threshold card-id)
  (let ((ogre (make-ogre name birthplace threshold card-id)))
    (ask ogre 'install)
    ogre))

(define (report-stolen-card id)
  (make&install-ogre (symbol-append 'ogre id) dungeon 1 id)) ;; symbol-append is a procedured provided in the original tester4.scm

             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             ;;                  Exercise 5                 ;;
             ;;                                             ;;
             ;; Implement big-brother and surveillance-room ;;
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-big-brother name)
 (let ((named-obj (make-named-object name))
        (logs '())
        (stolen-cards '()))
    (lambda (message)
      (cond ((eq? message 'inform)
            (lambda (self place card-id)
              ;;check if stolen card
              (let ((log (list place card-id (current-time))) ;; The new log entry
                    (log-found (member log logs)))             ;; Whether the new log entry already exists in the logs
                
                (set! logs (cons log logs)) ;; Add the new entry to the logs
                
                (if log-found
                    (let ((id (cadr log)))
                      (set! stolen-cards (cons id stolen-cards)) ;; Add the stolen card ID to the stolen cards list
                      (report-stolen-card id))
                    #f))))
            ((eq? message 'display-stolen-card)
             (lambda (self) stolen-cards))
            (else (get-method named-obj message))))))

(define (make-surveillance-room name big-brother)
  (let ((student-residence (make-student-residence name)))
    (lambda (message)
      (cond ((eq? message 'accept-person?)
            (lambda (self person)
              (let ((card (ask student-residence 'accept-person? person))) ;; student-residence's 'accept-person? returns the person's usable card, so we can use that instead of searching it
                                                                           ;; Note that this may not be the proper OOP way of doing this, since 'accept-person?'s specification only
                                                                           ;; guarantees that we get a Yes/No answer
                (if card 
                    (begin (ask big-brother 'inform self (ask card 'id)) ;; Inform the big brother about the card usage
                           #t)                                           ;; Make sure to return true if the card is found
                    #f))))                                               ;; Return false if the card is not found
            (else (get-method student-residence message))))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                              Exercise 6                              ;;
 ;;                                                                      ;;
 ;; Create class secret that extends thing and when taken opens a secret ;;
 ;;            passage way between dormitory and Tech Square             ;;
 ;;                                                                      ;;
 ;;  Also create object suspicious-book and install it in the dormitory  ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-secret name birthplace)
  (let ((thing (make-thing name birthplace)))
    (lambda (message)
      (cond ((eq? message 'set-owner)
             (lambda (self new-owner)
               (can-go-both-ways dormitory 'north 'east Tech-Square)
               (begin0 (ask thing 'set-owner new-owner)                            ;; begin0 will return the result of the parent 'set-owner call, but will run the commands in order
                       (ask new-owner 'say (list "A new passage opens north!"))))) ;; 'say is called after 'set-owner, so that the person says the message after they say they've taken the book
             (else (get-method thing message))))))

(define (make&install-secret name birthplace)
  (let ((secret (make-secret name birthplace)))
    (ask secret 'install)
    secret))

(define suspicious-book
  (make&install-secret 'suspicious-book dormitory))
