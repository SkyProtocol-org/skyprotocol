#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("pubsub/lib"
    "pubsub/command"
    "pubsub/node"
    "pubsub/misc"
    "pubsub/handler"
    (exe: "pubsub/main" bin: "pubsub")))
