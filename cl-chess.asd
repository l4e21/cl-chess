;;;; cl-chess.asd

(asdf:defsystem #:cl-chess
  :description "Describe cl-chess here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  ;; :depends-on (#:cl-utter)
  :components ((:file "package")
               (:file "chessboard")
               (:file "lookups")
               (:file "game")))
