;;; -*- Gerbil -*-
(import :std/logger
        :std/sugar
        :std/cli/getopt
        :std/io
        :std/net/address)

(export main)

;; build manifest; generated during the build
;; defines version-manifest which you can use for exact versioning
(include "../manifest.ss")

(deflogger tcp)

(def (main . args)
  (call-with-getopt pubsub-main args
    program: "pubsub"
    help: "A one line description of your program"
    (argument 'addr help: "address")
    ))

(def (pubsub-main opt)
  (start-logger!)
  (run (hash-get opt 'addr)))

(def (run raddr)
  (let* ((addr (resolve-address raddr))
         (sock (tcp-listen addr)))
    (while #t
      (try
        (let ((client (ServerSocket-accept sock)))
          (when client
            (debugf "Accepted connection from ~a" (StreamSocket-peer-address client))
            (spawn echo client)))
        (catch (e)
          (errorf "Error accepting connection: ~a" e))))))

(def (echo client)
  (try
    (spawn echo-io! (StreamSocket-reader client) (StreamSocket-writer client))
    (catch (e)
      (errorf "Error establishing connections: ~a" e)
      (StreamSocket-close client))))

(def (echo-io! reader writer)
  (io-copy! reader writer)
  (Writer-close writer)
  (Reader-close reader))

