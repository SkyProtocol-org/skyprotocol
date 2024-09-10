#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("pubsub/lib"
    "pubsub/command"
    "pubsub/node"
    (exe: "pubsub/main" bin: "pubsub")))
