;;; -*- Gerbil -*-
(import :std/error ; check-argument
        :std/sugar
        :std/logger
        :std/misc/string
        :std/io
        :clan/poo/object
        :clan/poo/brace
        :clan/poo/mop
        :clan/poo/type
        :clan/poo/number)
(export (except-out #t debugf infof warnf errorf verbosef))

(deflogger pubsub/command)
(start-logger! (current-output-port))
(current-logger-options 5)

(def (Request symbol T)
  (Record
    command: [(Exactly symbol)]
    payload: [T]))

(def (Response symbol T)
  (Record
    command: [(Exactly symbol)]
    payload: [T]))

(import :std/debug/DBG)

(defsyntax define-command
  (syntax-rules ()
    ((_ ctx command input-t output-t)
     (with-id ctx ((req-cmd "request-" #'command)
                   (res-cmd "response-" #'command)
                   (req-t "request-" #'command "-t")
                   (res-t "response-" #'command "-t"))
              (def req-t (Request 'command input-t))
              (def res-t (Response 'command output-t))
              (def (req-cmd payload)
                (validate req-t payload))
              (def (res-cmd payload)
                (validate res-t {command: 'command payload}))
              (DBG define-command: ['(command req-cmd res-cmd req-t res-t)])
              (export req-t res-t req-cmd res-cmd)))
    ((ctx command input-t output-t)
     (ctx ctx command input-t output-t))))


(define-type PeerAddress String) ;; the peer
(define-type PeerId Bytes) ;; the id
(define-type TopicId Bytes) ;; topic id
(define-type TopicDescription Bytes)
(define-type Certificate Bytes)
(define-type TopicTop
  (Record
    height: [UInt]
    certificate: [Certificate]))
(define-type BlockData Bytes)
(define-type TopicIndex
  (Record
    topic: [TopicId]
    height: [UInt]))

(define-command hello PeerId PeerId)
(define-command add-peer PeerAddress Unit)
(define-command add-topic TopicId Unit)
(define-command describe-topic TopicId TopicDescription)
(define-command next-topic (Maybe TopicId) (Maybe TopicId))
;;(define-command get-topics (Maybe TopicId) (Stream TopicId)) ;; in the future, with Stream
(define-command poll-topic TopicId TopicTop)
;; (define-command read-topic (Tuple TopicId UInt) (Stream BlockData)) ;; in the future, with Stream
(define-command next-topic-data TopicIndex (Maybe BlockData))
(define-command get-data-cert TopicIndex Certificate)
(define-command publish-block (Tuple TopicId BlockData) TopicTop)
