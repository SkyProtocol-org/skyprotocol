(import :gerbil/gambit
        :tcpubsub/pubsub/command
        :tcpubsub/pubsub/node
        :tcpubsub/pubsub/handler
        :std/logger
        :std/sugar
        :std/io
        :std/test
        :std/net/address)

(deflogger test)
(start-logger! (current-output-port))
(current-logger-options 5)

(def next-id 0)

(def (make-node addr)
  (using (node (Node addr) : Node)
    (debugf "Starting node on: ~a, with id: ~a" (node.sock.address) node.id)
    {node.set-id next-id}
    (set! next-id (+ next-id 1))
    {node.add-handler 'hello hello-handler}
    {node.add-handler 'unknown unknown-handler}
    {node.add-handler 'post post-handler}
    {node.add-handler 'add-peer add-peer-handler}
    node)))

(def 01-handshake-test
  (test-suite "Integration test for nodes greeting each other"
    (test-case "Set-up 2 nodes, send ADD-PEER to one of them"
      (using ((node1 (make-node "localhost:8001") : Node)
              (node2 (make-node "localhost:8002") : Node)
              (client (open-buffered-writer (tcp-connect "localhost:8001")) : StreamSocket))
        (spawn {node1.run})
        (spawn {node2.run})
        (write-command (hello "test client") client)
        (write-command (add-peer "localhost:8002") client)
        (check {node2.peers} ? (lambda (v) (not evector-empty? v)))))))
