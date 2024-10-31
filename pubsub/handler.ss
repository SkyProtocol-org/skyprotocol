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

(defrule (define-handler (name node peer reqres) (type handler ...) ...)
  (begin
    (def (name-handler (node : Node) (peer : Peer) reqres)
      (cond
       ((element? type reqres) handler ...)
       ...
       (else (str "Something went wrong in " 'name " handler"))))
    (export name-handler)))

(define-handler (hello node peer reqres)
  (request-hello-t
    {peer.set-id (bytes->string (.get reqres payload))}
    (debugf "Received HELLO request from: ~a, got id: ~a" (peer.sock.address) peer.id)
    {peer.send (response-hello (string->bytes node.id))})
  (response-hello-t
    {peer.set-id (bytes->string (.get reqres payload))}
    (debugf "Received HELLO response from: ~a, got id: ~a" (peer.sock.address) peer.id)))

(define-handler (add-peer node peer reqres)
  (request-add-peer-t
   (debugf "Received ADD-PEER request from (Peer '~a')" peer.id)
   (using ((peer-sock (tcp-connect (resolve-address (bytes->string (.get reqres payload)))) : StreamSocket)
           (new-peer (Peer peer-sock) : Peer))
     {peer.send (request-hello (inet-address->string (node.sock.address)))}
     (with-lock node.peers-mx (lambda () (evector-push! node.peers new-peer)))
     (spawn/name 'node.peer-handler (cut {node.handle-peer peer})))))

(define-handler (add-topic node peer reqres)
  (request-add-topic-t
    (debugf "Adding topic: ~a" (bytes->string (.get reqres payload)))
    (with-lock node.messages-mx (lambda () (set! node.messages (.call MessageTrie .acons (.get reqres payload) '()))))))

;; TODO figure out how this will work, do we want to store something more elaborate than just topic -> list of blocks?
(define-handler (describe-topic node peer reqres)
  (request-describe-topic-t
    (debugf "Received DESCRIBE-TOPIC request from (Peer '~a') for topic '~a'" peer.id (bytes->string (.get reqres payload)))
    {peer.send (response-describe-topic (string->bytes "not implemented"))}))

;; TODO how to work with maybe?
;; it may actually be easier to just implement the "Stream" version, since we won't need to think
;; where to store intermediate pointers and stuff.
;; just fork a thread which will send responses continiously
(define-handler (next-topic node peer reqres)
  (request-next-topic-t
    (debugf "Received NEXT-TOPIC request from (Peer '~a')" peer.id)
    {peer.send (response-next-topic (Maybe ...))})) ;; TODO Do I want to zip through the whole tree topic by topic until I hit the specified and return next?

(define-handler (poll-topic node peer reqres)
  (request-poll-topic-t
    (debugf "Received POLL-TOPIC request from (Peer '~a')" peer.id)
    {peer.send (response-poll-topic {topic: (.get reqres payload) height: 1})}))

; (def (read-topic-handler (node : Node) (peer : Peer) (cmd : Command))
;   (with ((Command 'read-topic topic) cmd)
;     (debugf "(Peer ~a) reads topic ~a" peer.id (bytes->string topic))))

; (def (get-data-certificate-handler (node : Node) (peer : Peer) (cmd : Command))
;   (with ((Command 'get-data-certificate topic) cmd)
;     (debugf "(Peer ~a) requests data certificate for ~a" peer.id (bytes->string topic))))

; (def (post-handler (node : Node) (peer : Peer) (cmd : Command))
;   (with ((Command 'post (PostMessage topic msg)) cmd)
;     (debugf "Received POST command from (Peer '~a')" peer.id)
;     (with-lock node.messages-mx (lambda ()
;       (debugf "Saving new message: ~a, to topic: ~a" msg topic)
;       ;; ensure the topic exists on this node
;       (until (hash-get node.messages topic)
;         (hash-put! node.messages topic (make-evector #(#f) 0)))
;       (evector-push! (hash-ref node.messages topic) (create-message msg))))))
