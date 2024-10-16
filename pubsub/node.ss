;;; -*- Gerbil -*-
(import :std/error
        :skyprotocol/pubsub/command
        :std/sugar
        :std/logger ; logging stuff
        (only-in :std/os/pid getpid)
        (only-in :std/misc/string str)
        (only-in :std/misc/list for-each!)
        :std/hash-table ; HashTable type
        :std/misc/hash ; hash tables
        :std/misc/evector ; evector
        :std/net/address
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
    (debugf "Trying to read command from (Peer '~a')" self.id)
    (read-command self.reader)))

(defmethod {send Peer}
  (lambda (self (cmd : Command))
    (debugf "Writing command: (Command ~a ~a)" cmd.command (bytes->string cmd.message))
    (write-command cmd self.writer)))

;; Node is a service accepting connections from peers and doing work
(defstruct Node (id
                 (sock : ServerSocket)
                 (handlers : HashTable)
                 continue?
                 messages messages-mx
                 peers peers-mx)
  transparent: #t
  constructor: :init!)

(defmethod {:init! Node}
  (case-lambda
    ((self local-addr)
      (set! self.id (getpid))
      (set! self.continue? #f)
      (set! self.sock (tcp-listen (resolve-address local-addr)))
      (set! self.handlers (hash))
      (set! self.messages (hash))
      (set! self.messages-mx (make-mutex))
      (set! self.peers (make-evector #(#f) 0))
      (set! self.peers-mx (make-mutex)))
    ((self local-addr (handlers : HashTable))
      (set! self.id (getpid))
      (set! self.continue? #f)
      (set! self.sock (tcp-listen (resolve-address local-addr)))
      (set! self.handlers handlers)
      (set! self.messages (hash))
      (set! self.messages-mx (make-mutex))
      (set! self.peers (make-evector #(#f) 0))
      (set! self.peers-mx (make-mutex)))))

(defmethod {set-id Node}
  (lambda (self id) (set! self.id id)))

(defmethod {add-handler Node}
  (lambda (self cmd handler) (hash-put! self.handlers cmd handler)))

(defmethod {run Node}
  (lambda (self)
    (set! self.continue? #t)
    (debugf "Starting node on: ~a, with id: ~a" (self.sock.address) self.id)
    (while self.continue?
      (try
        (using ((client (self.sock.accept) : StreamSocket)
                (peer (Peer client) : Peer))
          (when client
            (debugf "Accepted connection from: ~a" (peer.sock.address))
            (with-lock self.peers-mx (lambda ()
              (evector-push! self.peers peer)))
            (spawn/name 'node.run (cut {self.handle-peer peer}))))
        (catch (e)
          (errorf "Error accepting connection: ~a" e))))))

(defmethod {stop Node}
  (lambda (self)
    (set! self.continue? #f)
    (try
      (self.sock.close)
      (catch (e)
        (debugf "Stopping node on: ~a, with id: ~a" (self.sock.address) self.id)))))

(defmethod {handle-peer Node}
  (lambda (self (peer : Peer))
    (try
      (while #t
        {self.handle-command peer {peer.recv}})
      (catch (e)
        (errorf "Can't handle (Peer '~a'): ~a" peer.id e)))))

(defmethod {handle-command Node}
  (lambda (self (peer : Peer) (cmd : Command))
    (if-let (handler (hash-get self.handlers cmd.command))
      (handler self peer cmd)
      (error (str "Can't find handle for (Command " cmd.command ") for (Peer '" peer.id "')") "handle-command"))))

