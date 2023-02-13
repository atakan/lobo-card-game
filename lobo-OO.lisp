;;; convention: "lobo" refers to the game, "wolf" refers to the imaginary opponent in the game.

(defclass lobo-game ()
  ((wolf-hand :initform nil :accessor w-hand)
   (your-hand :initform nil :accessor y-hand)
   (deck :initarg :init-deck :accessor deck)
   (discard :initform nil :accessor discard)
   (top-card-revealed :initform nil :accessor tc-revealed)
   (wolf-score :initform 0 :accessor w-score)
   (your-score :initform 0 :accessor y-score))
  (:documentation "This keeps track of the status of the game. The first four are card collections, the fifth is a boolean variable, next two are integers."))

(defun make-lobo-deck (&key (difficulty "hard"))
  "This is a deck with 5 suits and numbers from 1 to 8 (easy) or 9 (normal) or 10 (hard). Suits are actually irrelevant, but we'll keep them to make a GUI easier in the future.
  I should probably define a struct or class for cards at some point."
  (let ((nmax (cond ((equal difficulty "hard") 10) ; case uses eql, so i use cond
		    ((equal difficulty "normal") 9)
		    ((equal difficulty "easy") 8))))
    (loop for n from 1 to nmax
	  append (loop for s in (list "a" "b" "c" "d" "e")
		       collect (list n s)))))

(defun shuffle (sequence)
  "This takes a list and returns a shuffled list.
   It uses Knuth shuffle. I took it from
   https://rosettacode.org/wiki/Knuth_shuffle "
  (etypecase sequence
    (list  (nshuffle-list sequence))
    (array (nshuffle-array sequence))))

(defun nshuffle-list (list)
  "Shuffle the list using an intermediate vector."
  (let ((array (nshuffle-array (coerce list 'vector))))
    (declare (dynamic-extent array))
    (map-into list 'identity array)))

(defun nshuffle-array (array)
  (loop for i from (length array) downto 2
        do (rotatef (aref array (random i))
                    (aref array (1- i)))
        finally (return array)))

(defgeneric print-status (lobo-game)
  (:documentation "Prints the status of a Lobo game. This includes hands, scores, the top card of deck and number of card in the deck and the discard."))

(defmethod print-status ((g lobo-game))
  (format t "~&WOLF (~a): " (w-score g))
  (format t "~s" (w-hand g))
  (format t "~&YOU (~a): " (y-score g))
  (format t "~s" (y-hand g))
  (format t "~&Top Card: ~a" (first (deck g)))
  (setf (tc-revealed g) t)
  (format t " (~a cards left in deck," (length (deck g)))
  (format t " ~a cards in discard.)" (length (discard g))))

;;; pop and push do not work as I expected, so I need to make this a macro. If I use defun, the arguments are not modified. Maybe it would work if I make it a method.
(defmacro move-cards (n &key from to)
  `(dotimes (i ,n)
     (push (pop ,from) ,to)))

;;; XXX this should check if there are enough cards left in the deck, and if not shuffle the discard and put it at the bottom of deck before dealing cards.
(defmethod deal-new-hands ((g lobo-game))
  (move-cards 4 :from (deck g) :to (y-hand g))
  (move-cards 4 :from (deck g) :to (w-hand g)))

(defun simple-game ()
  (let ((g (make-instance 'lobo-game
			  :init-deck (shuffle (make-lobo-deck)))))
    (print-status g)
    (deal-new-hands g)
    (game-loop g)))

(defun lobo-help ()
  (format t "
 You need to enter a command as a lisp list:
 (<com> (<your-hand>) (<lobo-hand>))
 here, <com> is the command (see options below), <your-hand> or <lobo-hand> is a list of indices indicating the cards to be used from your hand or the wolf's hand, respectively. They are 1-indexed, e.g., (1 3) indicates the first and the third card.
 Your options for command are:
   m ('match'): Take a card from your hand and the matching card from the wolf's hand. You receive the top card.
   s ('sum'): Take several of your cards from your hand to add up to one of wolf's cards. You receive the top card.
   w ('sweep'): A single card from your hand matches the sum of several card's from the wolf's hand. The wolf receives the top card.
   o ('over'): A single card from your hand is greater than a single card from the wolf's hand. The wolf receives cards in number equal to difference of the two cards.
   f ('fold'): The round ends. The wolf receives points equal to sum of the cards in its hand.

  The round will also end if as a result of a command either hand is down to zero cards.")) 

(defmethod game-mess ((g lobo-game) mess)
  (format t mess))

(defun card-val (card)
  (first card))

(defun sum-hand (hand) ; note: hand is a simple list for this implementation
  (apply '+ (mapcar 'card-val hand)))

(defmethod fold ((g lobo-game) cards)
  (if (or (first cards) (second cards))
      (game-mess g "You need to fold with two nils.")
      (progn
	(game-mess g "You folded.")
	(setf (w-score g) (+ (w-score g) (sum-hand (w-hand g)))) ; incr w's score
	(move-cards (length (y-hand g)) :from (y-hand g) :to (discard g))
	(move-cards (length (w-hand g)) :from (w-hand g) :to (discard g))
	(if (tc-revealed g) (move-cards 1 :from (deck g) :to (discard g)))
	(deal-new-hands g))))
    
(defmethod match ((g lobo-game) cards)
  (format t "this is match.~%")
  (format t "your cards: ~a, wolf's cards: ~a" (first cards) (second cards)))

(defmethod sum ((g lobo-game) cards)
  (format t "this is sum.~%")
  (format t "your cards: ~a, wolf's cards: ~a" (first cards) (second cards)))

(defmethod sweep ((g lobo-game) cards)
  (format t "this is sweep.~%")
  (format t "your cards: ~a, wolf's cards: ~a" (first cards) (second cards)))

(defmethod over ((g lobo-game) cards)
  (format t "this is over.~%")
  (format t "your cards: ~a, wolf's cards: ~a" (first cards) (second cards)))

(defmethod game-loop ((g lobo-game))
  (loop
    (print-status g)
    (format t "~&[m]atch, [s]um, s[w]eep, [o]ver, [f]old or [q]uit.~%")
    (format t "Enter a lisp list for a command: ")
    (let ((command (read *standard-input*)))
      (if (not (equal 3 (length command))) ;try to guarantee something acceptable
	  (lobo-help)
	  (case (first command)
	    (m (match g (rest command)))
	    (s (sum   g (rest command)))
	    (w (sweep g (rest command)))
	    (o (over  g (rest command)))
	    (f (fold  g (rest command)))
	    (q (return 'quit))
	    (t (lobo-help))))))
  ;; do game checks here; 1. end the round if one of the hands is empty
  ;; 2. end the game if the final score is reached.
  )
       
;(defun print-hand (hand)
;  (dolist (card hand)
;    (format t "~a " (card-val card)))
;  (format t "~&"))

