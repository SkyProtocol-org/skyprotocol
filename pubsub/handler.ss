;;; -*- Gerbil -*-
(import :std/error
        :tcpubsub/pubsub/command
        :tcpubsub/pubsub/node
        :std/sugar
        :std/logger ; logging stuff
        :std/net/address
        :std/misc/hash ; hash tables
        :std/hash-table ; hash table types
        :std/misc/evector ; evector
        :std/misc/string ; string manipulation
        :std/io)
(export hello-handler
        sync-handler
        post-handler
        unknown-handler
        add-peer-handler)

(deflogger pubsub/handle)
(start-logger! (current-output-port))
(current-logger-options 5)

(def (hello-handler (node : Node) (peer : Peer) (cmd : Command))
  (with ((Command 'hello id) cmd)
    (debugf "Received HELLO from: ~a, got id: ~a" (peer.sock.address) (bytes->string id))
    {peer.set-id (bytes->string id)}))

(def (unknown-handler (node : Node) (peer : Peer) (cmd : Command))
  (debugf "Received UNKNOWN command from (Peer '~a')" peer.id))

(def (sync-handler (node : Node) (peer : Peer) (cmd : Command))
  #t
  )

(def (post-handler (node : Node) (peer : Peer) (cmd : Command))
  (with ((Command 'post m) cmd)
    (debugf "Received POST command from (Peer '~a')" peer.id)
    (with-lock {node.messages-mx} (lambda () 
      (debugf "Saving new message: ~a" (bytes->string m))
      (evector-push! {node.messages} m)))))

(def (add-peer-handler (node : Node) (peer : Peer) (cmd : Command))
  #t
  )

; (def (command-reader (node : Node) messages messages-mx nodes nodes-mx)
;   (debugf "Spawned new command-reader for node: ~a" node.id)
;   (try
;     (while #t
;       ;; Wait for the next command
;       (let (cm (read-command node.reader))
;         (match cm
;           ;; SYNC command
;           ((Command 'SYNC _)
;             (debugf "Received SYNC command from: ~a" node.id)
;             (spawn send-messages messages-mx messages node.writer))
;           ;; ADD-PEER command
;           ((Command 'ADD-PEER m)
;             (debugf "Received ADD-PEER command from: ~a" node.id)
;             (try 
;               (using ((node-sock (tcp-connect (resolve-address (bytes->string m))) : StreamSocket)
;                       (new-node (create-node (getpid) node-sock) : Node))
;                 (write-command/try (sync) new-node.writer)
;                 (debugf "Sent sync command to the node: ~a" new-node.id)
;                 (with-lock nodes-mx (lambda () 
;                   (debugf "Added new node to the internal list of nodes")
;                   (evector-push! nodes new-node)))
;                 (spawn command-reader new-node messages messages-mx nodes nodes-mx))
;               (catch (e)
;                 (errorf "Cannot add peer: ~a" e))))
;           ((Command c m)
;             (errorf "Received incorrect command: (Command ~a ~a)" c m)))))
;     (catch (e)
;       (errorf "Cannot read command: ~a" e)
;       (debugf "Closing connection with node: ~a" node.id)
;       (node.sock.close))))

; (def (send-messages mx evec (writer : BufferedWriter))
;   (for* ((i (in-range (evector-fill-pointer evec))))
;       (let ((el (with-lock mx (lambda () (evector-ref evec i)))))
;         (debugf "Sending message: ~a" (bytes->string el))
;         (write-command/try (post el) writer))))
