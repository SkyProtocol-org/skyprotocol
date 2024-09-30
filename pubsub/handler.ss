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
    (with-lock node.messages-mx (lambda () 
      (debugf "Saving new message: ~a" (bytes->string m))
      (evector-push! node.messages m)))))

(def (add-peer-handler (node : Node) (peer : Peer) (cmd : Command))
  (with ((Command 'add-peer m) cmd)
    (debugf "Received ADD-PEER command from (Peer '~a')" peer.id)
    (using ((peer-sock (tcp-connect (resolve-address (bytes->string m))) : StreamSocket)
            (new-peer (Peer peer-sock) : Peer))
      {peer.send (hello (node.sock.address))}
      (with-lock node.peers-mx (lambda ()
        (evector-push! node.peers new-peer)))
      (spawn {node.handle-peer peer}))))
