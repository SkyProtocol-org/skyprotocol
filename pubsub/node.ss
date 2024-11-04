;;; -*- Gerbil -*-
(import :std/error
        :skyprotocol/types
        :skyprotocol/pubsub/command
        :std/sugar
        :std/logger ; logging stuff
        (only-in :std/os/pid getpid)
        (only-in :std/misc/string str)
        (only-in :std/misc/list for-each!)
        :clan/poo/object
        :clan/poo/mop
        :clan/poo/type
        :clan/poo/number
        :clan/persist/content-addressing
        :clan/persist/merkle-trie
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

;; TODO encode type & len into one 32 int at the start of the sequence
(defmethod {recv Peer}
  (lambda (self)
    (debugf "Trying to read command from (Peer '~a')" self.id)
    (let* ((cmd-ty (self.reader.read-u8))
           (cmd-len (self.reader.read-u32))
           (buf (make-u8vector cmd-len 0)))
      (self.reader.read buf)
      (.@ (cmd->type cmd-ty) .<-bytes buf))))

;; TODO the above one
(defmethod {send Peer}
  (lambda (self req/res)
    (debugf "Writing command: (Command ~a)" (.get req/res command))
    (let* ((cmd-ty (type->cmd req/res))
           (buf (.@ req/res .bytes<-)))
      (self.writer.write buf))))

;; Node is a service accepting connections from peers and doing work
(defstruct Node (id
                 (sock : ServerSocket)
                 (handlers : HashTable)
                 continue?
                 messages messages-mx
                 peers peers-mx)
  transparent: #t
  constructor: :init!)

#| (set! foo.messages (.call MessageTrie .acons new-index value foo.messages)) |#
#| Or for lots of messages, use the zipper instead with O(1) .zipper-acons |#




(defmethod {:init! Node}
  (case-lambda
    ((self local-addr)
      (set! self.id (getpid))
      (set! self.continue? #f)
      (set! self.sock (tcp-listen (resolve-address local-addr)))
      (set! self.handlers (hash))
      (set! self.topics (.@ TopicTrie .empty))
      (set! self.topics-mx (make-mutex))
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
  (lambda (self (peer : Peer) req)
    (let (cmd (.get req command))
      (if-let (handler (hash-get self.handlers cmd))
        (handler self peer cmd)
        (error (str "Can't find handle for (Command " cmd ") for (Peer '" peer.id "')") "handle-command")))))
