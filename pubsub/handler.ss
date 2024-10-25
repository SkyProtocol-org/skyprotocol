;;; -*- Gerbil -*-
(import :skyprotocol/pubsub/command
        :skyprotocol/pubsub/node
        :skyprotocol/pubsub/message
        :clan/poo/object
        :clan/poo/mop
        :clan/poo/type
        :clan/poo/number
        :std/error
        :std/sugar
        :std/logger ; logging stuff
        :std/srfi/130
        :std/srfi/19
        :std/net/address
        :std/hash-table ; HashTable types
        :std/misc/hash ; hash tables manipulation
        :std/misc/evector ; evector
        :std/misc/string ; string manipulation
        :std/misc/list ; for-each!
        :std/io)
(export (except-out #t debugf infof warnf errorf verbosef))

(deflogger pubsub/handle)
(start-logger! (current-output-port))
(current-logger-options 5)

; TODO fix this
(defrule (define-handler (name node peer reqres) (type handler ...) ...)
  (begin
    (def (name-handler (node : Node) (peer : Peer) reqres)
      (cond
       ((element? type reqres) handler ...)
       ...
       (else (str "Something went wrong in " 'name " handler"))))
    (export name-handler)))

(define-handler (add-peer node peer reqres)
  (request-add-peer-t
   (debugf "Received ADD-PEER command from (Peer '~a')" peer.id)
   (using ((peer-sock (tcp-connect (resolve-address (bytes->string (.get reqres payload)))) : StreamSocket)
           (new-peer (Peer peer-sock) : Peer))
     {peer.send (request-hello (inet-address->string (node.sock.address)))}
     (with-lock node.peers-mx (lambda () (evector-push! node.peers new-peer)))
     (spawn/name 'node.peer-handler (cut {node.handle-peer peer})))))

; (def (add-peer-handler (node : Node) (peer : Peer) reqres)
;   (cond
;     ((validate request-add-peer-t reqres) (begin
;      (debugf "Received ADD-PEER command from (Peer '~a')" peer.id)
;      (using ((peer-sock (tcp-connect (resolve-address (bytes->string (.get reqres payload)))) : StreamSocket)
;              (new-peer (Peer peer-sock) : Peer))
;       {peer.send (hello (inet-address->string (node.sock.address)))}
;       (with-lock node.peers-mx (lambda () (evector-push! node.peers new-peer)))
;       (spawn/name 'node.peer-handler (cut {node.handle-peer peer})))))
;     (#t (error "Something went wrong in ADD-PEER handler"))))

(def (hello-handler (node : Node) (peer : Peer) reqres)
  (cond
    ((validate request-hello-t reqres) (begin
     {peer.set-id (bytes->string (.get reqres payload))}
     (debugf "Received HELLO request from: ~a, got id: ~a" (peer.sock.address) peer.id)
     {peer.send (response-hello (string->bytes node.id))}))
    ((validate response-hello-t reqres) (begin
     {peer.set-id (bytes->string (.get reqres payload))}
     (debugf "Received HELLO response from: ~a, got id: ~a" (peer.sock.address) peer.id)))
    (#t (error "Something went wrong in HELLO handler"))))

;(def (add-topic-handler (node : Node) (peer : Peer) (cmd : Command))
;  (with ((Command 'add-topic topic-name) cmd)
;    (debugf "Adding topic: ~a" (bytes->string topic-name))
;    (with-lock node.messages-mx (lambda () (hash-get-set! node.messages (bytes->string topic-name) (make-evector #(#f) 0))))))

; (def (get-topics-handler (node : Node) (peer : Peer) (cmd : Command))
;   (with ((Command 'get-topics _) cmd)
;      (debugf "(Peer ~a) requesting topics" peer.id)
;      (with-lock node.messages-mx (lambda () 
;         (for-each! (hash-keys node.messages) (lambda (topic)
;           {peer.send (topic-name topic)}))))))

; (def (describe-topic-handler (node : Node) (peer : Peer) (cmd : Command))
;   (with ((Command 'describe-topic topic) cmd)
;     (debugf "(Peer ~a) requesting description for ~a" peer.id (bytes->string topic))))

; (def (poll-topic-handler (node : Node) (peer : Peer) (cmd : Command))
;   (with ((Command 'poll-topic topic) cmd)
;     (debugf "(Peer ~a) polls topic ~a" peer.id (bytes->string topic))
;     {peer.send (topic-height 2)}))

; (def (read-topic-handler (node : Node) (peer : Peer) (cmd : Command))
;   (with ((Command 'read-topic topic) cmd)
;     (debugf "(Peer ~a) reads topic ~a" peer.id (bytes->string topic))))

; (def (get-data-certificate-handler (node : Node) (peer : Peer) (cmd : Command))
;   (with ((Command 'get-data-certificate topic) cmd)
;     (debugf "(Peer ~a) requests data certificate for ~a" peer.id (bytes->string topic))))

; (def (unknown-handler (node : Node) (peer : Peer) (cmd : Command))
;   (debugf "Received UNKNOWN command from (Peer '~a')" peer.id))

; (def (post-handler (node : Node) (peer : Peer) (cmd : Command))
;   (with ((Command 'post (PostMessage topic msg)) cmd)
;     (debugf "Received POST command from (Peer '~a')" peer.id)
;     (with-lock node.messages-mx (lambda ()
;       (debugf "Saving new message: ~a, to topic: ~a" msg topic)
;       ;; ensure the topic exists on this node
;       (until (hash-get node.messages topic)
;         (hash-put! node.messages topic (make-evector #(#f) 0)))
;       (evector-push! (hash-ref node.messages topic) (create-message msg))))))
