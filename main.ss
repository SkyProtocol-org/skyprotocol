#!/usr/bin/env gxi

(export #t)

(import
  :skyprotocol/all-sky ;; required for the side-effect of importing everything that matters
  :std/misc/ports
  (only-in :std/cli/multicall define-entry-point call-entry-point current-program))

(current-program "sky")
;;(initialize-sky-path!)

(def (main . args)
  (gerbil-load-expander!) ;; NB: it will fail if called at the toplevel
  (apply call-entry-point args))

(define-entry-point (selftest)
  (help: "Self Test command"
   getopt: [])
  (writeln (build-manifest/head))
  (display-build-manifest (build-manifest/head)))
