;;;; card-games.asd

(asdf:defsystem #:card-games
  :description "Describe card-games here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "card-games")
               (:file "cards")
               (:file "black-jack")))
