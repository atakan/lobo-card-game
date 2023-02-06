(defclass lobo-game ()
  ((lobo-hand :initform nil :accessor l-hand)
   (your-hand :initform nil :accessor y-hand)
   (deck :initarg :init-deck :accessor deck)
   (discard :initform nil :accessor discard)
   (top-card-revealed :initform nil :accessor tc-revealed)
   (lobo-score :initform 0 :accessor l-score)
   (your-score :initform 0 :accessor y-score))
  (:documentation "This keeps track of the status of the game. The first four are card collections, the fifth is a boolean variable, next two are integers."))

(defun make-lobo-deck ()
  "This is a deck with 5 suits and numbers from 1 to 10. Suits are actually irrelevant, but we'll keep them to make a GUI easier in the future.
  I should probably define a struct for cards at some point."
  (loop for n from 1 to 10
	append (loop for s in (list "a" "b" "c" "d" "e")
		     collect (list n s))))

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
  (format t "~&LOBO (~a): " (l-score g))
  (format t "~s" (l-hand g))
  (format t "~&YOU (~a): " (y-score g))
  (format t "~s" (y-hand g))
  (format t "~&Top Card: ~a" (first (deck g)))
  (setf (tc-revealed g) t)
  (format t " (~a cards left in deck," (length (deck g)))
  (format t " ~a cards in discard.)" (length (discard g))))

(defgeneric deal-to-hand (lobo-game player n)
  (:documentation "Deals n cards to the player (lobo or you)."))

;(defmethod deal-to-hand ((g lobo-game) player n)
;  (let ((hand nil))
;    (ecase player
;      (lobo (setf hand (l-hand g)))
;      (you (setf hand (y-hand g))))
;    (dotimes (i n)
;      (push (pop (deck g)) hand))
;    (setf (tc-revealed g) nil)))

;;; pop and push do not work as I expected, so I need to make this a macro. If I use defun, the arguments are not modified. Maybe it would work if I make it a method.
(defmacro deal-from-deck (n deck hand)
  `(dotimes (i ,n)
     (push (pop ,deck) ,hand)))

;(defun deal-from-deck (n deck hand)
;  (dotimes (i n)
;    (push (pop deck) hand)))

(defmethod deal-to-hand ((g lobo-game) player n)
  (ecase player
    (lobo (deal-from-deck 4 (deck g) (l-hand g)))
    (you (deal-from-deck 4 (deck g) (y-hand g)))))

(defun simple-game ()
  (let ((my-game (make-instance 'lobo-game
				:init-deck (shuffle (make-lobo-deck)))))
    (print-status my-game)
    (deal-to-hand my-game 'lobo 4)
    (deal-to-hand my-game 'you 4)
    (print-status my-game)))

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

(defun fold ()
  (format t "you folded~%"))

(defun match (your-cards lobo-cards)
  (format t "this is match.~%")
  (format t "your cards: ~a, lobo-cards: ~a" your-cards lobo-cards))

(defun lobo-prompt (lobo-game)
  (format t "[m]atch, [s]um, s[w]eep, [o]ver or [f]old.~%")
  (format t "Enter a lisp list for a command: ")
  (let ((command nil))
    (setf command (read *standard-input*))
    (case (first command)
      (m (match (second command) (third command)))
      (f (fold))
      (t (lobo-help))))
  (lobo-prompt))
       

;(defun card-val (card)
;  (first card))

;(defun print-hand (hand)
;  (dolist (card hand)
;    (format t "~a " (card-val card)))
;  (format t "~&"))

