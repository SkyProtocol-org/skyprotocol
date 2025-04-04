(import :gerbil/gambit
        :skyprotocol/pubsub/command
        :skyprotocol/pubsub/node
        :skyprotocol/pubsub/handler
        :std/misc/evector
        :std/misc/threads
        :std/logger
        :std/sugar
        :std/io
        :std/test
        :std/net/address)
(import :std/debug/DBG)

(export 01-command-test 
        02-node-test)

(deflogger test)
(start-logger! (current-output-port))
(current-logger-options 5)

(def (test-command cmd cmd-sym)
  (with ((Command c m) cmd)
    (let* ((buf (make-u8vector 20 0))
           (reader (open-buffered-reader buf))
           (writer (open-buffered-writer #f buf)))
      (write-command (Command c m) writer)
      (check-equal? (symbolic->command cmd-sym) (u8vector-ref buf 0))
      (check-equal? (u8vector-length m) (u8vector-ref buf 1))
      (check-equal? (read-command reader) (Command c m)))))

(def 01-command-test
  (test-suite "Unit tests for command marshaling"
    (test-case "GET-TOPICS"
      (test-command get-topics 'get-topics))
    (test-case "POST"
      (with ((Command c m) (post "test-topic" "test-message"))
        (let* ((buf (make-u8vector 40 0))
               (reader (open-buffered-reader buf))
               (writer (open-buffered-writer #f buf)))
          (write-command (Command c m) writer)
          (check-equal? (symbolic->command 'post) (u8vector-ref buf 0))
          (check-equal? (u8vector-length m) (+ 2 (+ (string-length "test-topic") (string-length "test-message")))))))
    (test-case "ADD-PEER"
      (test-command (add-peer "localhost:8000") 'add-peer))
    (test-case "HELLO"
      (test-command (hello "test_id") 'hello))
    (test-case "ADD-TOPIC"
      (test-command (add-topic "test") 'add-topic))
    (test-case "STOP"
      (test-command stop 'stop))))

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
        (let (th (spawn/name 'test (cut {node.run})))
          (thread-sleep! 1)
          {client.send (hello "test client")}
          {client.send (post "test-topic" "kek")}
          (thread-sleep! 1)
          {node.stop}
          (try
           (thread-join! th)
           (catch (e) (displayln e))))))))
