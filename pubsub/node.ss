;;; -*- Gerbil -*-
(import :std/error ; check-argument
        :tcpubsub/pubsub/command
        :std/sugar
        :std/logger
        :std/misc/hash
        :std/misc/evector ; evector
        :std/io)
(export Node
        Peer)

(deflogger pubsub/node)
(start-logger! (current-output-port))
(current-logger-options 5)

;; Peer holds info about the connected peer
(defstruct Peer (id (sock : StreamSocket) (reader : BufferedReader) (writer : BufferedWriter))
  constructor: :init!)

(defmethod {:init! Peer}
  (lambda ((self : Peer) id (sock : StreamSocket))
    (set! self.id id)
    (set! self.sock sock)
    (set! self.reader (open-buffered-reader (sock.reader)))
    (set! self.writer (open-buffered-writer (sock.writer)))))

(defmethod {read-command Peer}
  (lambda (self : Peer)
    (read-command self.reader)))

;; Node is a service accepting connections from peers and doing work
(defstruct Node (id (handlers : HashTable) (messages : evector) messages-mx (peers : evector) peers-mx)
  constructor: :init!)

(defmethod {:init! Node}
  (case-lambda 
    (((self : Node))
      (set! self.id (getpid))
      (set! self.handlers (hash))
      (set! self.messages (make-evector #(#f) 0))
      (set! self.messages-mx (make-mutex))
      (set! self.peers (make-evector #(#f) 0))
      (set! self.peers-mx (make-mutex))
    (((self : Node) (handlers : HashTable))
      (set! self.id (getpid))
      (set! self.handlers handlers)
      (set! self.messages (make-evector #(#f) 0))
      (set! self.messages-mx (make-mutex))
      (set! self.peers (make-evector #(#f) 0))
      (set! self.peers-mx (make-mutex)))))

(defmethod {add-handler Node}
  (lambda ((self : Node) cmd handler)
    (hash-ref-set! self.handlers cmd handler)))

(defmethod {handle-command Node}
  (lambda ((self : Node) (cmd : Command))
    (let (handler (hash-ensure-ref self.handlers cmd.command (error "Can't handle command" "handle-command")))
        (try
          (handler cmd)
          (catch (e)
            (errorf "Cannot handle command (Command ~a): ~a" cmd.command e))))))

(defmethod {run Node}
  (lambda ((self : Node))
    (try
      (while #t
        ))
    ))
