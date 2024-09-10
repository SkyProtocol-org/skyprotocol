;;; -*- Gerbil -*-
(import :std/error ; check-argument
        :tcpubsub/pubsub/command
        :std/sugar
        :std/logger
        :std/misc/hash
        :std/io)
(export Node
        create-node)

(deflogger pubsub/node)
(start-logger! (current-output-port))
(current-logger-options 5)

;; Node holds reader and writer for the socket connection, as well as a socket itself, for later reuse
(defstruct Node (id handlers (sock : StreamSocket) (reader : BufferedReader) (writer : BufferedWriter)))

;; Handy function for creating a Node from StreamSocket
(def (create-node id (sock : StreamSocket))
  (Node id (hash) sock (open-buffered-reader (sock.reader)) (open-buffered-writer (sock.writer))))

(def (add-handler (node : Node) cmd handler)
  (hash-ref-set! node.handlers cmd handler))

(def (handle-command (node : Node) (cmd : Command))
  (let (handler (hash-ensure-ref node.handlers cmd.command (error "Can't handle command" "handle-command")))
    (try
      (handler cmd)
      (catch (e)
        (errorf "Cannot handle command (Command ~a): ~a" cmd.command e)))))

