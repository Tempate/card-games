;;;; cards.lisp

(in-package #:cards)

(defclass hand ()
  ((size :reader hand-size
         :initarg :size
         :initform 0
         :type integer)
   (cards :reader hand-cards
          :initarg :cards)))

(defmethod print-object ((self hand) stream)
  (dolist (card (hand-cards self))
    (prin1 card stream))
  self)

(defclass card ()
  ((suit :reader card-suit
         :initarg :suit)
   (rank :reader card-rank
         :initarg :rank)
   (unicode :reader card-unicode
            :initarg :unicode)))

(defmethod print-object ((self card) stream)
  (if (or *print-readably*
          (not (slot-boundp self 'unicode)))
      (print-unreadable-object (self stream :identity t :type t)
        (format stream "~A of ~A" (card-rank self) (card-suit self)))
      (princ (card-unicode self) stream))
  self)

(defmethod make-new-deck ()
  (make-instance 'hand :size 52
                 :cards (loop :for suit :in '(:clubs :diamonds :hearts :spades)
                              :for base :in '(#x1f0D0 #x1f0C0 #x1f0B0 #x1f0A0)          
                              :nconc (make-13-card-list suit base))))

(defmethod make-13-card-list (&optional (suit nil suitp) (base nil basep))
  (loop :for rank  :in '(1 2 3 4 5 6 7 8 9 10 :jack :queen :king)
        :for i :from 1
        :collect (make-instance 'card :rank rank
                                (when suitp :suit suit)
                                (when basep :unicode (code-char (+ base i))))))

(defmethod move-random-cards (hand1 hand2 n)
  (with-slots ((hand1-cards cards) (hand1-size size)) hand1
    (with-slots ((hand2-cards cards) (hand2-size size)) hand2
      (dotimes (i n)
        (let ((card (nth (random hand1-size) hand1-cards)))
          (push card hand2-cards)
          (setf hand1-cards (delete card hand1-cards))
          (decf hand1-size)
          (incf hand2-size))))))

