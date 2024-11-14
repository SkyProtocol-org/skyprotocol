(import (group-in :skyprotocol/pubsub command node handler)
        (group-in :std/misc evector threads)
        (group-in :std logger sugar io test net/address)
        :skyprotocol/types)
(import :std/debug/DBG)

(export 01-command-test 
        02-node-test)

(deflogger test)
(start-logger! (current-output-port))
(current-logger-options 5)

(def next-id 0)

(def (make-node addr)
  (using (node (Node addr) : Node)
    {node.set-id next-id}
    (set! next-id (+ next-id 1))
    node))

(def 01-node-test
  (test-suite "Integration tests for nodes"
    (test-case "Spin node up and down"
      (using ((node (make-node "localhost:8001") : Node)
              (client (Peer (tcp-connect "localhost:8001")) : Peer))
        (let (th (spawn/name 'test (cut {node.run})))
          (thread-sleep! 1)
          {client.send (hello "test client")}
          {client.send (post "test-topic" "kek")}
          (thread-sleep! 1)
          {node.stop}
          (try
           (thread-join! th)
           (catch (e) (displayln e))))))))
