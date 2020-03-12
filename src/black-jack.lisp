;;;; black-jack.lisp

(in-package #:black-jack)

;; Returns a hand's value giving aces a value of 1
(defmethod min-value (hand)
  (loop :for card :in (hand-cards hand)
        :sum (card-value card)))

;; Returns a hand's real value
;; Aces are valued 11 if the hand doesn't bust
(defmethod value (hand)
  (let ((value 0) (found-ace NIL))
    (loop :for card :in (hand-cards hand) :do
          (when (= (card-value card) 1)
            (setf found-ace t))
          (incf value (card-value card)))
    (when (and found-ace (<= value 11))
      (incf value 10))
    value))

;; Returns the probability of busting
(defmethod bust-prob (hand)
  (let ((value (min-value hand)))
    (cond ((<= value 11) 0)
          ((>= value 21) 1)
          (t (/ (loop :for v :from 1 :to 10
                      :sum (if (> (+ value v) 21)
                               (- (if (= v 10) 16 4)
                                  (loop :for card :in (hand-cards hand)
                                        :sum (if (= (card-value card) v) 1 0)))
                               0))
                (- 52 (length (hand-cards hand))))))))

;; Returns the probability of losing to the dealer
(defmethod lose-prob (pvalue dealer)
  (let ((dvalue (value dealer)))
    (cond ((> dvalue 21) 0)
          ((>= dvalue 17) (if (>= dvalue pvalue) 1 0))
          (t (/ (loop :for card :in (make-13-card-list :no-suit)
                      :sum (lose-prob pvalue (add-card dealer card)))
                13)))))

;; Play n games between the dealer and the player
(defun match (n)
  (let ((dwins 0))
    (loop :for i :from 1 :to n :do
          (when (eq (game #'dealer-strategy #'player-strategy) :dealer-wins)
            (incf dwins)))
    (print (cons "Dealer wins" dwins))
    (print (cons "Player wins" (- n dwins)))))

;; Play a game between a dealer and a player
(defun game (dealer-strategy player-strategy)
  (let* ((deck (make-52-card-deck))
         (dealer (play deck (make-2-card-hand deck) dealer-strategy))
         (player (play deck (make-2-card-hand deck) player-strategy))
         (pvalue (value player))
         (dvalue (value dealer)))
    (cond ((> pvalue 21) :dealer-wins)
          ((> dvalue 21) :player-wins)
          ((= pvalue dvalue) :dealer-wins)
          ((> pvalue dvalue) :player-wins)
          (t :dealer-wins))))

;; Play a hand following a given strategy
(defun play (deck hand strategy)
  (loop :while (and (<= (min-value hand) 21) (funcall strategy hand)) :do
         (hit deck hand))
  hand)

;; The default strategy used by the dealer in most casinos 
(defun dealer-strategy (hand)
  (< (value hand) 17))

;; The player asks for a card when the odds of busting are < 0.5
(defun player-strategy (hand)
  (< (bust-prob hand) 0.5))
