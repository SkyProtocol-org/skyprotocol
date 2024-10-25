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

(defrule (make-command command input-t output-t)
  (with-id make-command ((req-cmd 'request- #'command)
                         (res-cmd 'response- #'command)
                         (req-t 'request- #'command '-t)
                         (res-t 'response- #'command '-t))
    (def req-t (Request 'command input-t))
    (def res-t (Response 'command output-t))
    (def (req-cmd payload)
      (let (cmd ({command: 'command payload}))
        (validate req-t cmd)
        cmd))
    (def (res-cmd payload)
      (let (cmd ({command: 'command payload}))
        (validate res-t {command: 'command payload})
        cmd))
    (export req-t res-t req-cmd res-cmd)))

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

(make-command add-peer PeerAddress Unit)
(make-command hello PeerId PeerId)
(make-command add-topic TopicId Unit)
(make-command describe-topic TopicId TopicDescription)
(make-command next-topic (Maybe TopicId) (Maybe TopicId))
;;(make-command get-topics (Maybe TopicId) (Stream TopicId)) ;; in the future, with Stream
(make-command poll-topic TopicId TopicTop)
;; (make-command read-topic (Tuple TopicId UInt) (Stream BlockData)) ;; in the future, with Stream
(make-command next-topic-data TopicIndex (Maybe BlockData))
(make-command get-data-cert TopicIndex Certificate)
(make-command publish-block (Tuple TopicId BlockData) TopicTop)
