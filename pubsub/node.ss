;;; -*- Gerbil -*-
(import :std/error ; check-argument
        :tcpubsub/pubsub/command
        :std/sugar
        :std/logger
        :std/misc/string ; string manipulation
        :std/misc/hash ; hash tables
        :std/misc/evector ; evector
        :std/hash-table ; hash table types
        :std/io)
(export Node
        Peer)

(deflogger pubsub/node)
(start-logger! (current-output-port))
(current-logger-options 5)

;; Peer holds info about the connected peer
(defstruct Peer (id (sock : StreamSocket) (reader : BufferedReader) (writer : BufferedWriter))
  transparent: #t
  constructor: :init!)

(defmethod {:init! Peer}
  (lambda (self (sock : StreamSocket))
    (set! self.id #f) ;; It's false until the handshake
    (set! self.sock sock)
    (set! self.reader (open-buffered-reader (sock.reader)))
    (set! self.writer (open-buffered-writer (sock.writer)))))

(defmethod {set-id Peer}
  (lambda (self id)
    (set! self.id id)))

(defmethod {recv Peer}
  (lambda (self)
    (read-command self.reader)))

(defmethod {send Peer}
  (lambda (self (cmd : Command))
    (write-command cmd self.writer)))

;; Node is a service accepting connections from peers and doing work
(defstruct Node (id (sock : ServerSocket) (handlers : HashTable) messages messages-mx peers peers-mx)
  transparent: #t
  constructor: :init!)

(defmethod {:init! Node}
  (case-lambda 
    ((self local-addr)
      (set! self.id (getpid))
      (set! self.sock (tcp-listen (resolve-address local-addr)))
      (set! self.handlers (hash))
      (set! self.messages (make-evector #(#f) 0))
      (set! self.messages-mx (make-mutex))
      (set! self.peers (make-evector #(#f) 0))
      (set! self.peers-mx (make-mutex)))
    ((self local-addr (handlers : HashTable))
      (set! self.id (getpid))
      (set! self.sock (tcp-listen (resolve-address local-addr)))
      (set! self.handlers handlers)
      (set! self.messages (make-evector #(#f) 0))
      (set! self.messages-mx (make-mutex))
      (set! self.peers (make-evector #(#f) 0))
      (set! self.peers-mx (make-mutex)))))

(defmethod {add-handler Node}
  (lambda (self cmd handler)
    (hash-ref-set! self.handlers cmd handler)))

(defmethod {handle-command Node}
  (lambda (self (peer : Peer) (cmd : Command))
    (if-let (handler (hash-get self.handlers cmd.command))
      (handler self peer cmd))))

(defmethod {run Node}
  (lambda (self)
    (debugf "Starting node on: ~a" (self.sock.address))
    (while #t
      (try
        (using ((client (self.sock.accept) : StreamSocket)
                (peer (Peer client) : Peer))
          (when client
            (debugf "Accepted connection from: ~a" (client.peer-address))
            (with ((Command 'hello id) {peer.recv})
              (debugf "Received greetings from: ~a, got id: ~a" (client.peer-address) id)
              {peer.set-id (string->number id)})
            (with-lock self.peers-mx (lambda ()
              (evector-push! self.peers peer)))
            (spawn {self.handle-peer} peer)))
        (catch (e)
          (errorf "Error accepting connection: ~a" e))))))

(defmethod {handle-peer Node}
  (lambda (self (peer : Peer))
    (try
      (while #t
        {self.handle-command peer {peer.recv}})
      (catch (e)
        (errorf "Can't handle (Peer ~a): ~a" peer.id e)))))