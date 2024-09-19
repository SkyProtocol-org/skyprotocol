;;; -*- Gerbil -*-
(import :std/error ; check-argument
        :std/sugar
        :std/logger
        :std/io)
(export Command
        Node
        create-node
        read-command
        read-command/try
        write-command
        write-command/try
        incorrect
        sync
        post
        add-peer)

(deflogger pubsub/lib)
(start-logger! (current-output-port))
(current-logger-options 5)

(def id-source 0)

(def (get-id)
  (lambda (id)
    (set! id-source (+ id-source 1))
    (id)) id-source)

;; Node holds reader and writer for the socket connection, as well as a socket itself, for later reuse
(defstruct Node (id (sock : StreamSocket) (reader : BufferedReader) (writer : BufferedWriter)))

;; Handy function for creating a Node from StreamSocket
(def (create-node (sock : StreamSocket))
  (Node (get-id) sock (open-buffered-reader (sock.reader)) (open-buffered-writer (sock.writer))))

;; A simple struct that represents possible command for peer
(defstruct Command (command message))

(def (read-command/try (reader :- BufferedReader))
  (try
    (read-command reader)
    (catch (e)
      (errorf "Cannot read command: ~a" e))))

(def (read-command (reader :- BufferedReader))
  (debugf "Trying to read command")
  (let (command (reader.read-u8))
    (if (eof-object? command)
      (error "Received EOF while trying to read from socket" "read-command")
      (let* ((len (reader.read-u8))
             (buffer (make-u8vector len 0)))
        (unless (zero? len)
          (reader.read buffer 0 len len))
        (Command command buffer)))))

(def (write-command/try command (writer :- BufferedWriter))
  (try
    (write-command command writer)
    (catch (e)
      (errorf "Cannot write command: ~a" e))))

(def (write-command command (writer :- BufferedWriter))
  (match command
    ((Command c m)
      (debugf "Writing command: (Command ~a ~a)" c m)
      (let* ((msg-len (u8vector-length m))
             (buf (open-buffered-writer #f)))
        (using (buf :- BufferedWriter)
          (buf.write-u8 c)
          (buf.write-u8 msg-len)
          (unless (zero? msg-len)
            (buf.write m 0 msg-len))
          (let* ((cmd (get-buffer-output-u8vector buf))
                 (cmd-len (u8vector-length cmd)))
            (writer.write cmd 0 cmd-len)
            (writer.flush)))))))

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

;; incorrect command
(def (incorrect)
  (string->message 255 ""))
