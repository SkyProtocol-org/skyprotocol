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

(export 01-command-test 
        02-node-test)

(deflogger test)
(start-logger! (current-output-port))
(current-logger-options 5)

(def 01-command-test
  (test-suite "Unit tests for command marshaling"
    (test-case "SYNC"
      (with ((Command c m) sync)
        (let* ((buf (make-u8vector 20 0))
               (reader (open-buffered-reader buf))
               (writer (open-buffered-writer #f buf)))
          (write-command (Command c m) writer)
          (check-equal? buf (make-u8vector 20 0))
          (check-equal? (read-command reader) (Command c m)))))
    (test-case "POST"
      (with ((Command c m) (post "test"))
        (let* ((buf (make-u8vector 20 0))
               (reader (open-buffered-reader buf))
               (writer (open-buffered-writer #f buf)))
          (write-command (Command c m) writer)
          (check-equal? 1 (u8vector-ref buf 0))
          (check-equal? (u8vector-length m) (u8vector-ref buf 1))
          (check-equal? (read-command reader) (Command c m)))))
    (test-case "ADD-PEER"
      (with ((Command c m) (add-peer "localhost:8000"))
        (let* ((buf (make-u8vector 20 0))
               (reader (open-buffered-reader buf))
               (writer (open-buffered-writer #f buf)))
          (write-command (Command c m) writer)
          (check-equal? 2 (u8vector-ref buf 0))
          (check-equal? (u8vector-length m) (u8vector-ref buf 1))
          (check-equal? (read-command reader) (Command c m)))))
    (test-case "HELLO"
      (with ((Command c m) (hello "test_id"))
        (let* ((buf (make-u8vector 20 0))
               (reader (open-buffered-reader buf))
               (writer (open-buffered-writer #f buf)))
          (write-command (Command c m) writer)
          (check-equal? 3 (u8vector-ref buf 0))
          (check-equal? (u8vector-length m) (u8vector-ref buf 1))
          (check-equal? (read-command reader) (Command c m)))))
    (test-case "ADD-TOPIC"
      (with ((Command c m) (add-topic "test"))
        (let* ((buf (make-u8vector 20 0))
               (reader (open-buffered-reader buf))
               (writer (open-buffered-writer #f buf)))
          (write-command (Command c m) writer)
          (check-equal? 4 (u8vector-ref buf 0))
          (check-equal? (u8vector-length m) (u8vector-ref buf 1))
          (check-equal? (read-command reader) (Command c m)))))
    (test-case "STOP"
      (with ((Command c m) stop)
        (let* ((buf (make-u8vector 20 0))
               (reader (open-buffered-reader buf))
               (writer (open-buffered-writer #f buf)))
          (write-command (Command c m) writer)
          (check-equal? 5 (u8vector-ref buf 0))
          (check-equal? (read-command reader) (Command c m)))))
    (test-case "Invalid commands should be treated as UKNOWN"
      (with ((Command c m) (post "test"))
        (let* ((buf (make-u8vector 20 0))
               (reader (open-buffered-reader buf))
               (writer (open-buffered-writer #f buf)))
          (write-command (Command c m) writer)
          (u8vector-ref-set! buf 0 100)
          (check-equal? (read-command reader) (Command 'unknown m)))))))

(def next-id 0)

(def (make-node addr)
  (using (node (Node addr) : Node)
    {node.set-id next-id}
    (set! next-id (+ next-id 1))
    {node.add-handler 'hello hello-handler}
    {node.add-handler 'unknown unknown-handler}
    {node.add-handler 'post post-handler}
    {node.add-handler 'add-peer add-peer-handler}
    node))

(def 02-node-test
  (test-suite "Integration tests for nodes"
    (test-case "Spin node up and down"
      (using ((node (make-node "localhost:8001") : Node)
              (client (Peer (tcp-connect "localhost:8001")) : Peer))
        (let (th (spawn (cut {node.run})))
          (thread-sleep! 1)
          {client.send (hello "test client")}
          (thread-join! th))))))
    ; (test-case "Set-up 2 nodes, send ADD-PEER"
    ;   (using ((node1 (make-node "localhost:8001") : Node)
    ;           (node2 (make-node "localhost:8002") : Node)
    ;           (client (Peer (tcp-connect "localhost:8001")) : Peer))
    ;     (let* ((th1 (spawn (cut {node1.run})))
    ;            (th2 (spawn (cut {node2.run}))))
    ;       (thread-sleep! 1)
    ;       {client.send (hello "test client")}
    ;       {client.send (add-peer "localhost:8002")}
    ;       {client.send stop}
    ;       (thread-join! th2)
    ;       (check-equal? 1 (vector-length (evector->vector node2.peers))))))))
