;;; -*- Gerbil -*-
(import :tcpubsub/pubsub/lib
        :std/logger ; logger
        :std/sugar
        :std/cli/getopt ; cli options
        :std/io ; socket stuff and other
        :std/iter ; for*
        :std/misc/evector ; evector
        :std/net/address) ; resolve-address

(export main)

(deflogger pubsub)

(def (main . args)
  (call-with-getopt pubsub-main args
    program: "pubsub"
    help: "Simple tcp pubsub peer"
    (argument 'laddr help: "local address")))

(def (pubsub-main opt)
  (start-logger! (current-output-port))
  (current-logger-options 5)
  (run (hash-get opt 'laddr)))

(def (run addr)
  (let* ((laddr (resolve-address addr))
         (sock (tcp-listen laddr))
         (messages (make-evector #(#f) 0)) ; shared message buffer
         (messages-mx (make-mutex))
         (nodes (make-evector #(#f) 0)) ; shared node buffer
         (nodes-mx (make-mutex)))
    (while #t
      (try
        (let (client (ServerSocket-accept sock))
          (when client
            (debugf "Accepted connection from: ~a" (StreamSocket-peer-address client))
            (spawn command-reader client messages messages-mx nodes nodes-mx)))
        (catch (e)
          (errorf "Error accepting connection: ~a" e))))))

(def (command-reader client messages messages-mx nodes nodes-mx)
  (debugf "Spawned new command-reader for node: ~a" (StreamSocket-peer-address client))
  (thread-sleep! 1)
  (while #t
    ;; Wait for the next command
    (let (cm (read-command (open-buffered-reader (StreamSocket-reader client))))
      (match cm
        ;; SYNC command
        ((Command 0 "")
          (debugf "Received SYNC command from: ~a" (StreamSocket-peer-address client))
          (with-lock nodes-mx (lambda ()
            (for* ((i (in-range (evector-fill-pointer nodes))))
              (let (n (evector-ref nodes i))
                (spawn send-messages messages-mx messages (StreamSocket-writer client)))))))
        ;; POST command
        ;; TODO resend POST to other nodes
        ((Command 1 m)
          (debugf "Received POST command from: ~a" (StreamSocket-peer-address client))
          (with-lock messages-mx (lambda () 
            (debugf "Saving new message: ~a" (bytes->string m))
            (evector-push! messages m))))
        ;; ADD-NODE command
        ((Command 2 m)
          (debugf "Received ADD-PEER command from: ~a" (StreamSocket-peer-address client))
          (let* (node-sock (tcp-connect (resolve-address (bytes->string m))))
            (debugf "node-sock: ~a" node-sock)
            (write-command (sync) (open-buffered-writer (current-output-port)))
            (debugf "after sync in stdout")
            (write-command (sync) (StreamSocket-writer node-sock))
            (debugf "Sent sync command to the: ~a" (StreamSocket-peer-address node-sock))
            (with-lock nodes-mx (lambda () 
              (debugf "Added new node to the internal list of nodes")
              (evector-push! nodes m)))
            (spawn command-reader node-sock messages messages-mx nodes nodes-mx)))
        ((Command c m)
          (errorf "Received incorrect command: (Command ~a ~a)" c m))))))

(def (send-messages mx evec (writer : BufferedWriter))
  (for* ((i (in-range (evector-fill-pointer evec))))
      (let ((el (with-lock mx (lambda () (evector-ref evec i)))))
        (debugf "Sending message: ~a" (bytes->string el))
        (write-command (post el) writer))))

;; A simple struct that represents possible command for peer
(defstruct Command (command message))

(def (read-u8/blocking (reader :- BufferedReader))
  (let* (buf (make-u8vector 1 0))
    (reader.read buf 0 1 1)
    (u8vector-ref buf 0)))

(def (read-command (reader :- BufferedReader))
  (debugf "lol")
  (let* ((command (read-u8/blocking reader))
         (len (read-u8/blocking reader))
         (buffer (make-u8vector len 0)))
    (unless (zero? len)
      (reader.read buffer 0 len len))
    (Command command buffer)))

(def (write-command command (writer :- BufferedWriter))
  (debugf "kek")
  (match command
    ((Command c m)
      (debugf "(Command ~a ~a)" c m)
      (let ((len (u8vector-length m)))
        (writer.write-u8 c)
        (writer.write-u8 len)
        (unless (zero? len)
          (writer.write m 0 len))))))

;; Valid message must be a u8vector with length less than 256 bytes
(def (valid-message? m)
  (and (u8vector? m) (< (u8vector-length m) 256)))

(def (string->message t str)
  (let ((m (string->bytes str)))
    (check-argument (valid-message? m) "u8vector and len < 256" m)
    (Command t m)))

;; sync messages from other node
(def (sync)
  (string->message 0 ""))

;; post message to other node
(def (post message)
  (string->message 1 message))

;; add peer to the network
(def (add-peer message)
  (string->message 2 message))

