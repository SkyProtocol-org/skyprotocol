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

(define-type PostPayload
  (Record
    topic: [u8vector]
    body: [u8vector]))

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
       (def (req-cmd payload))))
    ((make-command res 'cmd payload-t)
     (begin
       (add-command 'cmd)
       ;; TODO implement this
       (def (res-cmd payload))))))

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

(make-command req 'add-peer)
(make-command req 'hello)
(make-command res 'hello-res)
(make-command req 'add-topic)
(make-command req 'get-topics)
(make-command res 'topic-name) ;; sub command for 'get-topics
(make-command req 'describe-topic)
(make-command req 'poll-topic)
(make-command res 'topic-height) ;; sub command for 'poll-topic
(make-command req 'read-topic)
(make-command res 'read-topic-res)
(make-command req 'get-data-certificate)
(make-command req 'stop)
(make-command req 'post)
