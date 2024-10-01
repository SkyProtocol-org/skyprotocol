(import :gerbil/gambit
        :tcpubsub/pubsub/command
        :tcpubsub/pubsub/node
        :tcpubsub/pubsub/handler
        :std/misc/evector
        :std/misc/threads
        :std/logger
        :std/sugar
        :std/io
        :std/test
        :std/net/address)

(export 01-handshake-test)

(deflogger test)
(start-logger! (current-output-port))
(current-logger-options 5)

(def next-id 0)

(def (make-node addr)
  (using (node (Node addr) : Node)
    {node.set-id next-id}
    (set! next-id (+ next-id 1))
    {node.add-handler 'hello hello-handler}
    {node.add-handler 'unknown unknown-handler}
    {node.add-handler 'post post-handler}
    {node.add-handler 'add-peer add-peer-handler}
    {node.add-handler 'exit (lambda (node peer cmd) ({node.stop}))}
    node))

(def 01-handshake-test
  (test-suite "Integration test for nodes greeting each other"
    (test-case "Set-up 2 nodes, send ADD-PEER to one of them"
      (using ((node1 (make-node "localhost:8001") : Node)
              (node2 (make-node "localhost:8002") : Node)
              (client (Peer (tcp-connect "localhost:8001")) : Peer))
        (let* ((th1 (spawn (cut {node1.run})))
              (th2 (spawn (cut {node2.run}))))
          (thread-sleep! 1)
          {client.send (hello "test client")}
          {client.send (add-peer "localhost:8002")}
          {client.send stop}
          (thread-join! th2)
          (check-equal? 1 (vector-length (evector->vector node2.peers))))))))
