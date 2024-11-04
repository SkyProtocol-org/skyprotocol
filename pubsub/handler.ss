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
    (with-lock node.messages-mx (lambda () (set! node.messages (.call TopicTrie .acons (.get reqres payload) (.@ MessageTrie .empty)))))))

(define-handler (describe-topic node peer reqres)
  (request-describe-topic-t
    (debugf "Received DESCRIBE-TOPIC request from (Peer '~a') for topic '~a'" peer.id (bytes->string (.get reqres payload)))
    {peer.send (response-describe-topic (string->bytes "not implemented"))}))

;; TODO fix this
(define-handler (get-topics node peer reqres)
  (request-get-topics-t
    (debugf "Received GET-TOPICS request from (Peer '~a')" peer.id)
    (spawn/name 'get-topics (lambda () (begin
      (.call TopicTrie .map/key (lambda (k v) {peer.send (response-get-topics k)}) node.topics (.get reqres payload)))))))

(define-handler (poll-topic node peer reqres)
  (request-poll-topic-t
    (debugf "Received POLL-TOPIC request from (Peer '~a')" peer.id)
    {peer.send (response-poll-topic (.o 
      (height (.call MessageTrie .trie-height (.call TopicTrie .ref node.topics (.get reqres payload)))) 
      (certificate (.call MerkleTrie .proof node.topics (.get reqres payload))))}))

(define-handler (read-topic node peer reqres)
  (request-read-topic-t
    (debugf "Received READ-TOPIC request from (Peer '~a')" peer.id)
    ))

(define-handler (get-data-cert node peer reqres)
  (request-get-data-cert-t
    (debugf "Received GET-DATA-CERT request from (Peer '~a')" peer.id))) ;; TODO implement this
