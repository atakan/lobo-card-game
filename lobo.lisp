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
	

(defun deal-from-deck (n deck hand)
  (dotimes (i n)
    (push (pop deck) hand)))

(defun card-val (card)
  (first card))

(defun print-hand (hand)
  (dolist (card hand)
    (format t "~a " (card-val card)))
  (format t "~&"))

(defun print-status (lobo-hand your-hand deck)
  (format t "LOBO: ")
  (print-hand lobo-hand)
  (format t "YOU:  ")
  (print-hand your-hand)
  (format t "Top Card: ")
  (format t "~a~&" (card-val (first deck))))

(defun simple-game ()
  (let ((deck (make-lobo-deck))
	(lobo-hand nil)
	(your-hand nil))
    (setf deck (shuffle deck))
    (deal-from-deck (4 deck lobo-hand))
    (deal-from-deck (4 deck your-hand))
    (print-status)))
