;;; -*- Gerbil -*-
(import :std/sugar
        :std/srfi/19) ; date/time stuff
(export Message
        create-message)

(defstruct Message (time message)
  transparent: #t)

(def (create-message msg)
  (Message (current-date) msg))
