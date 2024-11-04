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
  (with-id define-handler ((hname #'name "-handler"))
    (def (hname (node : Node) (peer : Peer) reqres)
      (debugf (str "Received " 'name " request/response from (Peer '~a') with payload: " (.get reqres payload)) peer.id)
      (cond
       ((element? type reqres) handler ...)
       ...
       (else (str "Something went wrong in " 'name " handler"))))
    (export hname)))

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
   (using ((peer-sock (tcp-connect (resolve-address (bytes->string (.get reqres payload)))) : StreamSocket)
           (new-peer (Peer peer-sock) : Peer))
     {peer.send (request-hello (inet-address->string (node.sock.address)))}
     (with-lock node.peers-mx (lambda () (evector-push! node.peers new-peer)))
     (spawn/name 'node.peer-handler (cut {node.handle-peer peer})))))

(define-handler (add-topic node peer reqres)
  (request-add-topic-t
    (with-lock node.messages-mx (lambda () (set! node.messages (.call TopicTrie .acons (.get reqres payload) (.call MessageTrie .empty)))))))

;; for now it's empty(or not implemented in this case), in the future we want to send data about throughput, etc
(define-handler (describe-topic node peer reqres)
  (request-describe-topic-t
    {peer.send (response-describe-topic (string->bytes "not implemented"))}))

;; TODO how to send null value from Maybe? Is it just void?
(define-handler (get-topics node peer reqres)
  (request-get-topics-t
    (spawn/name 'get-topics (lambda ()
      (with-lock node.topics-mx (lambda () (begin
        (for ([key . val] (.call MerkleTrie .iter<- node.topics (.get reqres payload)))
          {peer.send (response-get-topics key)})
        {peer.send (response-get-topics #void)})))))))

(define-handler (poll-topic node peer reqres)
  (request-poll-topic-t
    {peer.send (response-poll-topic (.o 
      (height (.call MessageTrie .trie-height (.call TopicTrie .ref node.topics (.get reqres payload)))) 
      (certificate (.call MerkleTrie .proof node.topics (.get reqres payload))))}))

; (define-handler (read-topic node peer reqres)
;   (request-read-topic-t
;     ))

; (define-handler (get-data-cert node peer reqres)
;   (request-get-data-cert-t)) ;; TODO implement this
