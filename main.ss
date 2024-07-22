#!/usr/bin/env gxi

(export #t)

(import
  :skyprotocol/all-sky ;; required for the side-effect of importing everything that matters
  (only-in :std/cli/multicall call-entry-point current-program))

(current-program "sky")
;;(initialize-sky-path!)

(def (main . args)
  (gerbil-load-expander!) ;; NB: it will fail if called at the toplevel
  (apply call-entry-point args))
