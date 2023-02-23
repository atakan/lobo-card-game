;(defgeneric deal-cards (card-collection card-collection)
;  ) ;this will replace the pop/push macro above
  
;(defun print-hand (hand)
;  (dolist (card hand)
;    (format t "~a " (card-val card)))
;  (format t "~&"))


;(defmethod match ((g lobo-game) cards)
;  (let ((y-cards (first cards))
;	(w-cards (second cards)))
;    ;; ensure that there is one index in each card list
;    ;(if (or (not (= 1 (length y-cards))) (not (= 1 (length w-cards))))
;    (unless (and (= 1 (length y-cards)) (= 1 (length w-cards)))
;      (game-mess g "for match, you need to indicate one card in each hand")
;      (return-from match))
;    ;; ensure that the cards match
;    (unless (equal (card-val (elt (y-hand g) (car y-cards)))
;		   (card-val (elt (w-hand g) (car w-cards))))
;      (game-mess g "the cards you indicate do not match")
;      (return-from match))
;    (game-mess g "you matched successfully!")
;    ;; remove the indicated cards
;    (setf (y-hand g) (remove-cards g y-cards (y-hand g)))
;    (setf (w-hand g) (remove-cards g w-cards (w-hand g)))
;    ;; deal next card to the player (this turns off top-card-revealed)
;    (move-cards 1 :from (deck g) :to (y-hand g))
;    (setf (tc-revealed g) nil)))

;(defmethod sum ((g lobo-game) cards)
;  (let ((y-cards (first cards))
;	(w-cards (second cards)))
;    ;; ensure that there is one index in wolf's hand and several in your hand
;    (unless (and (> (length y-cards) 1) (= (length w-cards) 1))
;      (game-mess "for sum, you need to indicate one card in wolf's and several in yours")
;      (return-from sum))
;    ;; ensure that the cards match
;    (unless (equal (card-val-sum (y-hand g) y-cards)
;		   (card-val-sum (w-hand g) w-cards))
;      (game-mess g "the cards you indicate do not sum up to each other")
;      (return-from sum))
;    (game-mess g "you summed successfully!")
;    ;; remove the indicated cards
;    (setf (y-hand g) (remove-cards g y-cards (y-hand g)))
;    (setf (w-hand g) (remove-cards g w-cards (w-hand g)))
;    ;; deal next card to the player (this turns off top-card-revealed)
;    (move-cards 1 :from (deck g) :to (y-hand g))
;    (setf (tc-revealed g) nil)))

;(defmethod sweep ((g lobo-game) cards)
;  (let ((y-cards (first cards))
;	(w-cards (second cards)))
;    ;; ensure that there is one index in your hand and several in wolf's
;    (unless (and (= (length y-cards) 1) (> (length w-cards) 1))
;      (game-mess g "for sweep, you need to indicate one card in your hand and several in wolf's")
;      (return-from sweep))
;    ;; ensure that the cards match
;    (unless (equal (card-val-sum (y-hand g) y-cards)
;		   (card-val-sum (w-hand g) w-cards))
;      (game-mess g "the cards you indicate do not sum up to each other")
;      (return-from sweep))
;    (game-mess g "you swept successfully!")
;    ;; remove the indicated cards
;    (setf (y-hand g) (remove-cards g y-cards (y-hand g)))
;    (setf (w-hand g) (remove-cards g w-cards (w-hand g)))
;    ;; deal next card to the wolf (this turns off top-card-revealed)
;    (move-cards 1 :from (deck g) :to (w-hand g))
;    (setf (tc-revealed g) nil)))

;(defmethod over ((g lobo-game) cards)
;  (let ((y-cards (first cards))
;	(w-cards (second cards)))
;    ;; ensure that there is one index for both your hand and wolf's
;    (unless (and (= (length y-cards) 1) (= (length w-cards) 1))
;      (game-mess g "for sweep, you need to indicate one card in both your hand and wolf's")
;      (return-from over))
;    ;; ensure that your card is larger
;    (unless (> (card-val-sum (y-hand g) y-cards)
;	       (card-val-sum (w-hand g) w-cards))
;      (game-mess g "the card for your hand is not larger than the card for wolf's")
;      (return-from over))
;    (game-mess g "you went over successfully!")
;    (let ((val-diff (- (card-val-sum (y-hand g) y-cards)
;		       (card-val-sum (w-hand g) w-cards))))
;      ;; remove the indicated cards
;      (setf (y-hand g) (remove-cards g y-cards (y-hand g)))
;      (setf (w-hand g) (remove-cards g w-cards (w-hand g)))
;      ;; deal bunch of cards to the wolf (this turns off top-card-revealed)
;      (move-cards val-diff :from (deck g) :to (w-hand g))
;      (setf (tc-revealed g) nil))))


;(defmacro make-action (action-name
;		       y-card-cond w-card-cond wrong-card-mess
;		       card-val-cond wrong-val-mess
;		       success-mess num-deal-cards deal-to-player
;		       )
;  `(defmethod ,action-name ((g lobo-game) cards)
;     (let ((y-cards (first cards))
;	   (w-cards (second cards)))
;       (unless (and (,(first y-card-cond) (length y-cards) ,(second y-card-cond))
;		    (,(first w-card-cond) (length w-cards) ,(second w-card-cond)))
;	 (game-mess g ,wrong-card-mess)
;	 (return-from ,action-name))
;       (unless (,card-val-cond (card-val-sum (y-hand g) y-cards)
;			       (card-val-sum (w-hand g) w-cards))
;	 (game-mess g ,wrong-val-mess)
;	 (return-from ,action-name))
;       (game-mess g ,success-mess)
;       (setf (y-hand g) (remove-cards g y-cards (y-hand g)))
;       (setf (w-hand g) (remove-cards g w-cards (w-hand g)))
;       (move-cards ,num-deal-cards :from (deck g) :to (,deal-to-player g))
;       (setf (tc-revealed g) nil))))
;(make-action match
;	     (= 1) (= 1) "for match, you need to indicate one card in both hands"
;	     equal "the cards you indicate are not equal to each other"
;	     "you matched successfully!" 1 y-hand)

;(make-action sum
;	     (> 1) (= 1) "for sum, you need to indicate one card in wolf's and several in yours"
;	     equal "the cards you indicate do not sum up to each other"
;	     "you summed successfully!" 1 y-hand)

;(make-action sweep
;	     (= 1) (> 1) "for sweep, you need to indicate one card in your hand and several in wolf's"
;	     equal "the cards you indicate do not sum up to each other"
;	     "you swept successfully!" 1 w-hand)
