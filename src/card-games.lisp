;;;; card-games.lisp

(in-package #:card-games)

(defun choose-game (games)
  (format t "What game do you want to play?~%")
  (loop :for game :in games
        :for i :from 0
        :do (format t "~D. ~A~%" i game))
  (let ((user-input (read)) (games-len (length games)))
    (loop :while (and (not (typep user-input 'integer)) (>= user-input 0) (< user-input games-len)) :do
          (print "Your choice must be a number")
          (setq user-input (read)))
    (nth user-input games)))

(defun play ())
