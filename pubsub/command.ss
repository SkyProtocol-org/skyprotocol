;;; -*- Gerbil -*-
(import :std/error ; check-argument
        :std/sugar
        :std/logger
        :std/io)
(export Command
        CommandProxy
        read-command
        write-command)

(deflogger pubsub/command)
(start-logger! (current-output-port))
(current-logger-options 5)

;; A simple struct that represents possible command for peer
(defstruct Command (command message))

(def (read-command (reader :- BufferedReader))
  (debugf "Trying to read command")
  (let (command (reader.read-u8))
    (if (eof-object? command)
      (error "Received EOF while trying to read from socket" "read-command")
      (let* ((len (reader.read-u8))
             (buffer (make-u8vector len 0)))
        (unless (zero? len)
          (reader.read buffer 0 len len))
        (Command (command->symbolic command) buffer)))))

(def (write-command command (writer :- BufferedWriter))
  (with ((Command c m) command)
    (debugf "Writing command: (Command ~a ~a)" c m)
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

(interface CommandProxy
  ;; receive command
  (recv)
  ;; send command
  (send (cmd :~ Command?)))

;; Valid message must be a u8vector with length less than 256 bytes
(def (valid-message? m)
  (and (u8vector? m) (< (u8vector-length m) 256)))

(def (string->message t str)
  (let ((m (string->bytes str)))
    (check-argument (valid-message? m) "u8vector and len < 256" m)
    (Command (symbolic->command t) m)))

(def commands (hash))
(def symbolic-commands (hash))
(def next-id 0)

(def (command->symbolic cmd)
  (hash-ref commands cmd 'UKNOWN))

(def (symbolic->command sym)
  (hash-ref symbolic-commands sym 'UKNOWN))

(def (add-command sym)
  (let (id next-id)
    (set! next-id (+ next-id 1))
    (hash-put! commands sym id)
    (hash-put! symbolic-commands id sym)
    id))

(defsyntax make-command
  (syntax-rules ()
    ;; for commands that have static or fixed message
    ((make-command 'cmd msg) 
     (begin 
       (add-command 'cmd)
       (def cmd
         (string->message 'cmd msg))
       (export cmd)))
    ;; for commands that have dynamic message
    ((make-command 'cmd)
     (begin
       (add-command 'cmd)
       (def (cmd msg)
         (string->message 'cmd msg))
       (export cmd)))))

(make-command 'sync "")
(make-command 'post)
(make-command 'add-peer)
(make-command 'hello)
