#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss
;; You can even run tests without first building with ./build.ss !

(import (only-in :clan/testing init-test-environment!))
(init-test-environment!) ;; sets up the path to load things in skyprotocol

(import
  (only-in :clan/path-config source-path)

  ;; Import for the REPL interactive environment?
  ;; :skyprotocol/all-sky
  )
