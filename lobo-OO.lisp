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

;;; pop and push do not work as I expected, so I need to make this a macro. If I use defun, the arguments are not modified.
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

;(defun card-val (card)
;  (first card))

;(defun print-hand (hand)
;  (dolist (card hand)
;    (format t "~a " (card-val card)))
;  (format t "~&"))

