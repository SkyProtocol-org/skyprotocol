;;; -*- Gerbil -*-
(import :std/error ; check-argument
        :std/sugar
        :std/logger
        :std/misc/string
        :std/io)
(export Command
        PostMessage
        command->symbolic
        symbolic->command
        read-command
        write-command)

(deflogger pubsub/command)
(start-logger! (current-output-port))
(current-logger-options 5)

;; A simple struct that represents possible command for peer
(defstruct Command (command message)
  transparent: #t)

(defstruct PostMessage (topic message)
  transparent: #t)

(def (read-command (reader :- BufferedReader))
  (let (command (reader.read-u8))
    (if (eof-object? command)
      (error "Received EOF while trying to read from socket" "read-command")
      (let* ((cmd (command->symbolic command))
             (len (reader.read-u8))
             (buffer (make-u8vector len)))
        (unless (zero? len)
          (reader.read buffer 0 len len))
        (message->command cmd buffer)))))

(def (write-command command (writer :- BufferedWriter))
  (with ((Command c m) command)
    (let* ((msg-len (u8vector-length m))
           (buf (open-buffered-writer #f)))
      (using (buf :- BufferedWriter)
        (buf.write-u8 (symbolic->command c))
        (buf.write-u8 msg-len)
        (unless (zero? msg-len)
          (buf.write m 0 msg-len))
        (let* ((cmd (get-buffer-output-u8vector buf))
               (cmd-len (u8vector-length cmd)))
          (writer.write cmd 0 cmd-len)
          (writer.flush))))))

;; Valid message must be a u8vector with length less than 256 bytes
(def (valid-message? m)
  (and (u8vector? m) (< (u8vector-length m) 256)))

(def (message->command c buf)
  (case c
    ('post
     (let* ((tpc-len (u8vector-ref buf 0))
            (msg-len (u8vector-ref buf 1))
            (tpc-buf (make-u8vector tpc-len))
            (msg-buf (make-u8vector msg-len))
            (reader (open-buffered-reader buf)))
       (using (reader : BufferedReader)
          (reader.read tpc-buf 2 tpc-len tpc-len)
          (reader.read msg-buf (+ 2 tpc-len) msg-len msg-len))
       (Command 'post (PostMessage tpc-buf msg-buf))))
    (else (Command c buf))))

(def (string->message str)
  (let ((m (string->bytes str)))
    (check-argument (valid-message? m) "u8vector and len < 256" m)
    m))

(def commands (hash))
(def symbolic-commands (hash))
(def next-id 0)

(def (command->symbolic cmd)
  (hash-ref commands cmd 'unknown))

(def (symbolic->command sym)
  (hash-ref symbolic-commands sym 'unknown))

(def (add-command sym)
  (let (id next-id)
    (set! next-id (+ next-id 1))
    (hash-put! commands id sym)
    (hash-put! symbolic-commands sym id)
    id))

(defsyntax make-command
  (syntax-rules ()
    ;; for commands that have static or fixed message
    ((make-command 'cmd msg) 
     (begin 
       (add-command 'cmd)
       (def cmd
         (Command 'cmd (string->message msg)))
       (export cmd)))
    ;; for commands that have dynamic message
    ((make-command 'cmd)
     (begin
       (add-command 'cmd)
       (def (cmd msg)
         (Command 'cmd (string->message msg)))
       (export cmd)))))

(make-command 'sync "")
(make-command 'add-peer)
(make-command 'hello)
(make-command 'add-topic)
(make-command 'stop "")

(add-command 'post)
(def (post topic message)
  (let* ((tpc-len (string-length topic))
         (msg-len (string-length message))
         (buf (make-u8vector (+ 2 (+ tpc-len msg-len)) 0))
         (writer (open-buffered-writer #f buf)))
    (using (writer : BufferedWriter)
      (writer.write-u8 tpc-len)
      (writer.write-u8 msg-len)
      (writer.write (string->bytes (str topic message)) 0 (+ tpc-len msg-len)))
    (Command 'post buf)))
(export post)
