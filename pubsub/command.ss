;;; -*- Gerbil -*-
(import :std/error ; check-argument
        :std/sugar
        :std/logger
        :std/misc/string
        :std/io)
(export (except-out #t debugf infof warnf errorf verbosef))

(deflogger pubsub/command)
(start-logger! (current-output-port))
(current-logger-options 5)

;; TODO ask Fare how to do this right
(define-type Request
  (Record
    command: [Symbol]
    payload: [Any]))

(define-type Response
  (Record
    command: [Symbol]
    payload: [Any]))

(define-type DefaultPayload
  (Record
    body: [u8vector]))

;; Valid message must be a u8vector with length less than 256 bytes
(def (valid-message? m)
  (and (u8vector? m) (< (u8vector-length m) 256)))

(def commands (hash))
(def symbolic-commands (hash))
(def next-id 0)

(def (command->symbolic cmd)
  (hash-ref commands cmd 'unknown))

(def (symbolic->command sym)
  (hash-ref symbolic-commands sym 'unknown))

(def (add-command sym)
  (let (id next-id)
    (set! next-id (+ next-id 1))
    (hash-put! commands id sym)
    (hash-put! symbolic-commands sym id)
    id))

;; TODO implement this
;; NOTE as I understand, to have parametrized types I need functions that will
;; prefill the parameter in the type
(defsyntax make-command
  (syntax-rules (req res)
    ((make-command req 'cmd payload-t)
     (begin
       (add-command 'cmd)
       ;; TODO implement this
       (def (req-cmd payload))
       (export req-cmd)
       ))
    ((make-command res 'cmd payload-t)
     (begin
       (add-command 'cmd)
       ;; TODO implement this
       (def (res-cmd payload))
       (export res-cmd)
       ))))

; (defsyntax make-command
;   (syntax-rules ()
;     ;; for commands that have static or fixed message
;     ((make-command 'cmd msg) 
;      (begin 
;        (add-command 'cmd)
;        (def cmd
;          (Command 'cmd (string->message msg)))
;        (export cmd)))
;     ;; for commands that have dynamic message
;     ((make-command 'cmd)
;      (begin
;        (add-command 'cmd)
;        (def (cmd msg)
;          (Command 'cmd (string->message msg)))
;        (export cmd)))))

(define-type AddPeerPayload
  (Record
    peer: [String]))
(make-command req 'add-peer AddPeerPayload)

(define-type HelloPayload
  (Record
    id: [u8vector]))
(make-command req 'hello HelloPayload)
(make-command res 'hello-res HelloPayload)

(define-type AddTopicPayload
  (Record
    topic: [u8vector]))
(make-command req 'add-topic AddTopicPayload)

(define-type DescribeTopicPayload
  (Record
    topic: [u8vector]))
(make-command req 'describe-topic DescribeTopicPayload)

(define-type DescribeTopicRes
  (Record
    topic-name: [u8vector]))
(make-command res 'describe-topic-res DescribeTopicRes)

(define-type GetTopicsPayload
  (Record
    topic: [u8vector])) ; starting topic
(make-command req 'get-topics GetTopicsPayload)

(define-type GetTopicsRes
  (Record
    topic: [u8vector]
    more?: [bool]))
(make-command res 'get-topics-res GetTopicsRes)

(define-type PollTopicPayload
  (Record
    topic: [u8vector]))
(make-command req 'poll-topic PollTopicPayload)

(define-type PollTopicRes
  (Record
    height: [nat]
    certificate: [u8vector]))
(make-command res 'poll-topic-res PollTopicRes)

(define-type ReadTopicPayload
  (Record
    topic: [u8vector]
    height: [nat]))
(make-command req 'read-topic ReadTopicPayload)

(define-type ReadTopicRes
  (Record
    block-data: [u8vector]
    more?: [bool]))
(make-command res 'read-topic-res ReadTopicRes)

(define-type GetDataCertPayload
  (Record
    topic: [u8vector]
    height: [nat]))
(make-command req 'get-data-cert GetDataCertPayload)

(define-type GetDataCertRes
  (Record
    certificate: [u8vector]))
(make-command res 'get-data-cert-res GetDataCertRes)

(define-type PublishBlockPayload
  (Record
    topic: [u8vector]
    block-data: [u8vector]))
(make-command req 'publish-block)

(define-type PublishBlockRes
  (Record
    block-certificate: [u8vector]))
(make-command res 'publish-block-res PublishBlockRes)
