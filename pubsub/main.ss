;;; -*- Gerbil -*-
(import :skyprotocol/pubsub/lib
        (only-in :std/logger start-logger! deflogger current-logger-options debugf errorf)
        :tcpubsub/pubsub/command
        :tcpubsub/pubsub/node
        :std/logger ; logger
        :std/os/pid ; for getting PID
        :std/sugar
        (only-in :std/cli/getopt call-with-getopt argument)
        (only-in :std/cli/multicall define-entry-point)
        :std/io ; socket stuff and other
        (only-in :std/iter for* in-range)
        (only-in :std/misc/evector make-evector evector-push! evector-fill-pointer evector-ref)
        (only-in :std/net/address resolve-address))

(export main)

(deflogger pubsub)

(define-entry-point (node local-address)
  (help: "Start a Sky node"
   getopt: [(argument 'local-address help: "local address")])
  (start-logger! (current-output-port))
  (current-logger-options 5)
  (run (hash-get opt 'laddr)))

(def (run addr)
  (using (node (Node addr) : Node)
    (debugf "Starting node on: ~a" (node.sock.address))
    {node.run}))
