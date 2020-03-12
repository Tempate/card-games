;;;; card-games.asd

(asdf:defsystem #:card-games
  :description "A project to play several card games"
  :author "tempate"
  :license  "GPLv3+"
  :version "0.0.1"
  :components ((:module "src"
                        :serial :t
                        :components ((:file "package")
                                     (:file "card-games")
                                     (:file "cards")
                                     (:file "black-jack"))))
