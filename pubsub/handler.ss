;;; -*- Gerbil -*-
(import (group-in :skyprotocol/pubsub node command)
        (group-in :clan/poo object mop type number table)
        (group-in :std iter error sugar logger hash-table io)
        (group-in :std/misc hash evector string list)
        (group-in :std/srfi 130 19)
        :skyprotocol/types
        :clan/persist/merkle-trie
        :std/net/address)
(export (except-out #t debugf infof warnf errorf verbosef))

(deflogger pubsub/handle)
(start-logger! (current-output-port))
(current-logger-options 5)

(defrule (define-handler (name node peer reqres) (type handler ...) ...)
  (with-id define-handler ((hname #'name "-handler"))
    (def (hname (node : Node) (peer : Peer) reqres)
      ;; something weird goes on here, `peer.id` doesn't work for some reason, when placed in macro
      ; (debugf (str "Received " 'name " request/response from (Peer '~a') with payload: " (.get reqres payload)) peer.id)
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
    (with-lock node.topics-mx (lambda () 
      (set! node.topics 
        (.call TopicTrie .acons (.get reqres payload) (.call MessageTrie .empty)))))))

;; for now it's empty(or not implemented in this case), in the future we want to send data about throughput, etc
(define-handler (describe-topic node peer reqres)
  (request-describe-topic-t
    {peer.send (response-describe-topic (string->bytes "not implemented"))}))

;; TODO how to send null value from Maybe? Is it just void?
(define-handler (get-topics node peer reqres)
  (request-get-topics-t
    (spawn/name 'get-topics (lambda ()
      (with-lock node.topics-mx (lambda () (begin
        (for (key (.call TopicTrie .iter<- node.topics (.get reqres payload)))
          {peer.send (response-get-topics key)})
        {peer.send (response-get-topics #f)})))))))

(define-handler (poll-topic node peer reqres)
  (request-poll-topic-t
    {peer.send (response-poll-topic (.o 
      (height (.call MessageTrie .trie-height 
        (.call TopicTrie .ref node.topics (.get reqres payload)))) 
      (certificate (.call MerkleTrie .proof node.topics (.get reqres payload)))))}))

;; key == height in MessageTrie
(define-handler (get-data-cert node peer reqres)
  (request-get-data-cert-t
    {peer.send (response-poll-topic
      (.call MerkleTrie .proof
        (.call TopicTrie .ref node.topics
          (.get reqres payload topic))
          (.get reqres payload height)))}))

(define-handler (read-topic node peer reqres)
  (request-read-topic-t
    (spawn/name 'read-topic (lambda ()
      (with-lock node.topics-mx (lambda () (begin
        (let* ((topic (.call TopicTrie .ref node.topics (vector-ref (.get reqres payload) 0)))
               (height (vector-ref (.get reqres payload) 1)))
          (.call MessageTrie .for-each/reverse (lambda (key _) (begin
            (when (<= key height)
              {peer.send (response-read-topic 
                (.call MessageTrie .ref topic key))}))) 
            topic)))))))))

(define-handler (publish-block node peer reqres)
  (request-publish-block-t
    (with-lock node.topics-mx (lambda () (begin
      (let* ((topic (.call TopicTrie .ref node.topics (vector-ref (.get reqres payload) 0)))
             (block (vector-ref (.get reqres payload) 1))
             (height (.call MessageTrie .trie-height topic)))
        (.call MessageTrie .acons block (+ 1 height) topic)
        {peer.send (response-publish-block (.o
          (height (+ 1 height))
          (topic topic)))}))))))
