;;; convention: "lobo" refers to the game, "wolf" refers to the imaginary opponent in the game.

(defclass lobo-game ()
  ((wolf-hand :initform nil :accessor w-hand)
   (your-hand :initform nil :accessor y-hand)
   (deck :initarg :init-deck :accessor deck)
   (discard :initform nil :accessor discard)
   (top-card-revealed :initform nil :accessor tc-revealed)
   (wolf-score :initform 0 :accessor w-score)
   (your-score :initform 0 :accessor y-score)
   (max-score :initform 100 :initarg :max-score :accessor max-score))
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
  (:documentation "Prints the status of a Lobo game. This includes hands, scores, the top card of deck and number of cards in the deck and the discard."))

(defmethod print-status ((g lobo-game))
  (format t "~&~%WOLF (~a): " (w-score g))
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

(defun simple-game (&key (difficulty "hard") (max-score 100))
  (let ((g (make-instance 'lobo-game
			  :init-deck (shuffle
				      (make-lobo-deck :difficulty difficulty))
			  :max-score max-score)))
    (game-mess g "~&Starting new game")
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
   w ('sweep'): A single card from your hand matches the sum of several cards from the wolf's hand. The wolf receives the top card.
   o ('over'): A single card from your hand is greater than a single card from the wolf's hand. The wolf receives cards in number equal to difference of the two cards.
   f ('fold'): The round ends. The wolf receives points equal to sum of the cards in its hand.

  The round will also end if as a result of a command either hand is down to zero cards.")) 

(defgeneric game-mess (lobo-game &rest args)
  (:documentation "A method to print messages."))

(defmethod game-mess ((g lobo-game) &rest args)
  (apply #'format (cons t args)))

(defun card-val (card)
  (first card))

(defun sum-hand (hand) ; note: hand is a simple list for this implementation
  (apply '+ (mapcar 'card-val hand)))

(defmethod fold ((g lobo-game) cards)
  (if (or (first cards) (second cards))
      (game-mess g "You need to fold with two nils.")
      (progn
	(game-mess g "You folded.")
	(move-cards (length (y-hand g)) :from (y-hand g) :to (discard g)))))

(defun card-val-sum (hand indices)
  "Given a hand and a list of indices, calculates and returns the sum of card values"
  (loop for i in indices
	sum (card-val (elt hand i))))

;; a macro to implement various actions, perhaps an overkill

(defmacro make-action2 (action-name
			y-card-cond w-card-cond wrong-card-mess
			val-diff-cond wrong-val-mess
			success-mess num-deal-cards deal-to-player)
  `(defmethod ,action-name ((g lobo-game) cards)
     (let ((y-cards (first cards))
	   (w-cards (second cards)))
       (unless (and (,(first y-card-cond) (length y-cards) ,(second y-card-cond))
		    (,(first w-card-cond) (length w-cards) ,(second w-card-cond)))
	 (game-mess g ,wrong-card-mess)
	 (return-from ,action-name))
       (let ((val-diff (- (card-val-sum (y-hand g) y-cards)
			  (card-val-sum (w-hand g) w-cards))))
	 (unless (,@val-diff-cond val-diff)
	   (game-mess g ,wrong-val-mess)
	   (return-from ,action-name))
	 (game-mess g ,success-mess)
	 (setf (y-hand g) (remove-cards g y-cards (y-hand g)))
	 (setf (w-hand g) (remove-cards g w-cards (w-hand g)))
	 (move-cards ,num-deal-cards :from (deck g) :to (,deal-to-player g))
	 (setf (tc-revealed g) nil)))))

;; using the macro above to define some methods for actions.
;; note that the last one uses val-diff which is declared in the macro
(make-action2 match
	     (= 1) (= 1) "for match, you need to indicate one card in both hands"
	     (= 0) "the cards you indicate are not equal to each other"
	     "you matched successfully!" 1 y-hand)

(make-action2 sum
	      (> 1) (= 1) "for sum, you need to indicate one card in wolf's and several in yours"
	      (= 0) "the cards you indicate do not sum up to each other"
	      "you summed successfully!" 1 y-hand)

(make-action2 sweep
	     (= 1) (> 1) "for sweep, you need to indicate one card in your hand and several in wolf's"
	     (= 0) "the cards you indicate do not sum up to each other"
	     "you swept successfully!" 1 w-hand)

(make-action2 over
	     (= 1) (= 1) "for over, you need to indicate one card in both hands"
	     (< 0) "the card for your hand has to be larger than the card for wolf's"
	     "you went over successfully!" val-diff w-hand)
    
(defmethod game-loop ((g lobo-game))
  (loop
    (print-status g)
    (game-mess g "~&[m]atch, [s]um, s[w]eep, [o]ver, [f]old or [q]uit.~%")
    (game-mess g "Enter a lisp list for a command: ")
    (let ((command (read *standard-input*)))
      (if (or (not (listp command)) (not (equal 3 (length command)))) ;try to guarantee something acceptable
	  (lobo-help)
	  (case (first command)
	    (m (match g (rest command)))
	    (s (sum   g (rest command)))
	    (w (sweep g (rest command)))
	    (o (over  g (rest command)))
	    (f (fold  g (rest command)))
	    (q (return 'quit))
	    (t (lobo-help)))))
    ;; do game checks here;
    ;; 1. end the round if one of the hands is empty,
    (when (or (equal (w-hand g) nil) (equal (y-hand g) nil))
      (if (y-hand g)
	  (progn
	    (game-mess g "~&You won the round")
	    ;; 1b. update score,
	    (incf (y-score g) (sum-hand (y-hand g)))
	    ;; 1c. discard non-empty hand(and top card if revealed) and deal new hands.
	    (move-cards (length (y-hand g)) :from (y-hand g) :to (discard g))
	    (if (tc-revealed g) (move-cards 1 :from (deck g) :to (discard g)))
	    (deal-new-hands g))
	  (progn
	    (game-mess g "~&Wolf won the round")
	    ;; 1b. update score,
	    (incf (w-score g) (sum-hand (w-hand g)))
	    ;; 1c. discard non-empty hand(and top card if revealed) and deal new hands.
	    (move-cards (length (w-hand g)) :from (w-hand g) :to (discard g))
	    (if (tc-revealed g) (move-cards 1 :from (deck g) :to (discard g)))
	    (deal-new-hands g)))
      ;; 2. end the game if the final score is reached.
      (if (or (> (w-score g) (max-score g))
	      (> (y-score g) (max-score g)))
	  (progn
	    (game-mess g "~&Final scores -- You: ~d  Wolf: ~d"
		       (y-score g) (w-score g))
	    (return 'quit))
	  ;; 2b. shuffle discard and add to bottom of deck if not.
	  (progn
	    (game-mess g "~&Shuffling discard and moving to the bottom of deck")
	    (shuffle (discard g))
	    (setf (deck g) (concatenate 'list (deck g) (discard g)))
	    (setf (discard g) nil))))))
       

;;; A utility function that can potentially be useful elsewhere
(defun remove-nth (n lst)
  "Remove nth element from a given list (0 indexed)."
  (if (= n 0)
      (cdr lst)
      (if (> n (length lst))
	  ((lambda (x) x) lst) ; I don't know how to simply return the list
	  (cons (car lst) (remove-nth (1- n) (cdr lst))))))

(defun remove-ele (indices lst)
  "Given an index (as an atom) or a list of indices (as a list) remove all those from the given list (0 indexed) and return the result."
  (if (atom indices)
      (setf (elt lst indices) nil)
      (dolist (i indices)
	(setf (elt lst i) nil)))
  (remove nil lst))

(defmethod remove-cards ((g lobo-game) indices hand)
  ;;"Given an index (as an atom) or a list of indices (as a list) move all those from the given hand into the discard. Use 0-index."
  (if (atom indices)
      (let ((tmp (elt hand indices)))
	(push tmp (discard g))
	(setf (elt hand indices) nil))
      (dolist (i indices)
	(let ((tmp (elt hand i)))
	  (push tmp (discard g))
	  (setf (elt hand i) nil))))
  (remove nil hand))


;;; A hand class, let's see where this goes

(defclass card-collection ()
  ((cards :initform nil :accessor cards))
  (:documentation "A collection of cards. This can be the deck, the discard pile or the hands."))

(defclass hand (card-collection)
  ()
  (:documentation "This is a sub-class of card collection. It refers to either your hand or wolf's hand."))

(defgeneric deal-to-hand (lobo-game to &optional from)
  ) ; if from is not given, it is dealt from the deck
