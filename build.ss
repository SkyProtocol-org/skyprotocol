#!/usr/bin/env gxi
;; This is the main build file for Sky Protocol. Invoke it using
;;     ./build.ss [cmd]
;; where [cmd] is typically left empty (same as "compile")
;; Note that may you need to first:
;;     gxpkg install github.com/fare/gerbil-ethereum
;; See HACKING.md for details.

(import :std/cli/multicall :std/misc/process
        :clan/base :clan/building :clan/debug :clan/git-fu)

(def (files)
  (!> (all-gerbil-modules)
      #;(cut remove-build-file <> "main.ss")
      #;(cut cons [exe: "main.ss" bin: "sky-main"] <>)))

(init-build-environment!
 name: "Sky"
 ;; NB: missing versions for drewc/smug-gerbil and vyzo/libp2p
 deps: '("clan" "clan/crypto" "clan/poo" "clan/persist" "clan/ethereum")
 spec: files)

;; TODO: create version files for all overridden dependencies, too
(define-entry-point (nix)
  (help: "Build using nix-build" getopt: [])
  (create-version-file)
  (run-process ["nix-build"] stdin-redirection: #f stdout-redirection: #f)
  (void))
