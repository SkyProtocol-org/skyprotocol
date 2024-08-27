;;; -*- Gerbil -*-
(import :std/logger
        :std/sugar
        :std/cli/getopt
        :std/io
        :std/net/address)

(export main)

(deflogger tcp)

(def (main . args)
  (call-with-getopt pubsub-main args
    program: "pubsub"
    help: "Simple tcp pubsub peer"
    (argument 'addr help: "local address")
    (optional-argument 'peer help: "peer to connect to" default: #f)))

(def (pubsub-main opt)
  (start-logger!)
  (run (hash-get opt 'addr) (hash-get opt 'peer)))

(def (run addr paddr)
  (let* ((laddr (resolve-address addr))
         (sock (tcp-listen laddr)))
    (while #t
      (try
        (let ((client (ServerSocket-accept sock)))
          (when client
            (debugf "Accepted connection from: ~a" (StreamSocket-peer-address client))
            (if paddr
              (let (paddr2 (resolve-address paddr))
                (spawn pubsub client paddr2))
              (spawn peer client))))
        (catch (e)
          (errorf "Error accepting connection: ~a" e))))))

(def (pubsub client paddr)
  (try
    (let (remote (tcp-connect paddr))
      (spawn pubsub-io! (StreamSocket-reader client) (StreamSocket-writer remote)))
    (catch (e)
      (errorf "Error connecting to peer: ~a" e)
      (StreamSocket-close client))))

(def (pubsub-io! reader writer)
  (io-copy! reader writer)
  (Writer-close writer)
  (Reader-close reader))

(def (peer client)
  (try
    (spawn peer-io! (StreamSocket-reader client) (open-buffered-writer (current-output-port)))
    (catch (e)
      (errorf "Error establishing connection: ~a" e))))

(def (peer-io! reader writer)
  (io-copy! reader writer)
  (Reader-close reader))

(defstruct Command (command len message))

(def (read-command (reader :- BufferedReader))
  (let* ((command (reader.read-u8))
         (len (reader.read-u8))
         (buffer (make-u8vector len 0)))
    (reader.read buffer 0 len len)
    (Command command len buffer)))

(def (write-command command (writer :- BufferedWriter))
  (match command
    ((Command c l m)
      (writer.write-u8 c)
      (writer.write-u8 l)
      (writer.write m 0 l))))


