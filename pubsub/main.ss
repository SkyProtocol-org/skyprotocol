;;; -*- Gerbil -*-
(import (only-in :std/logger start-logger! deflogger current-logger-options debugf errorf)
        (group-in :skyprotocol/pubsub command node)
        (group-in :std interface logger os/pid sugar io)
        (only-in :std/cli/getopt call-with-getopt argument)
        (only-in :std/cli/multicall define-entry-point))

(export main)

(deflogger pubsub)

(define-entry-point (node local-address)
  (help: "Start a Sky node"
   getopt: [(argument 'local-address help: "local address")])
  (start-logger! (current-output-port))
  (current-logger-options 5)
  (run local-address))

(def (run addr)
  (using (node (Node addr #t) : Node)
    (debugf "Starting node on: ~a" (node.sock.address))
    {node.run}))
