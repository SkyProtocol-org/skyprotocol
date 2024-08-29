#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("pubsub/lib"
    (exe: "pubsub/main" bin: "pubsub")))
