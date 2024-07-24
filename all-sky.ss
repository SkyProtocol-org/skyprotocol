;; -*- Gerbil -*-
;; Use this file by include'ing it in your gxi REPL:
;;   (eval `(include ,(path-expand "all-sky.ss" (getenv "SKYPROTOCOL_SRC"))))
;; Or if you prefer without eval, but computing the absolute path yourself, something like:
;;   (include "~/src/skyprotocol/all-sky.ss")
;; Or just run ./ggxi

;; TODO: maybe have build.ss extract the list of files from this file (?)

(import

  ;; Gerbil
  (group-in :gerbil gambit expander)
  (group-in :std interactive actor assert coroutine error format io iter logger
            pregexp sort source stxutil sugar test)
  (group-in :std/cli getopt multicall print-exit shell)
  (group-in :std/debug DBG heap memleak threads)
  (group-in :std/misc bytes decimal deque evector hash list number path ports process
            queue repr string vector)
  (group-in :std/net request)
  (group-in :std/parser ll1)
  (group-in :std/srfi 1 13 133 141)
  (except-in :std/srfi/19 time)
  (group-in :std/text basic-printers char-set csv hex json)

  ;; swank
  ;; NB: until https://github.com/ecraven/r7rs-swank/pull/10 is merged,
  ;; use https://github.com/fare-patches/r7rs-swank as your checkout
  ;; :ecraven/gerbil-swank

  ;; gerbil-utils
  (group-in :clan/net tcp websocket whois)
  (group-in :clan assert base cli concurrency config diceware
            error failure ffi files filesystem generator git-fu
            hash json list logger maybe memo option order path-config
            peekable-iterator ports random simple-actor-client source
            stateful-avl-map string syntax temporary-files timestamp versioning watch)

  ;; gerbil-poo
  (group-in :clan/poo cli debug object io trie fq polynomial)
  (prefix-in :clan/poo/mop poo.)
  (prefix-in :clan/poo/type poo.)
  (only-in :clan/poo/mop
           Type Type. Class Class. Slot Lens Function Fun
           .defgeneric .method proto validate element? slot-lens sexp<-)
  (only-in :clan/poo/number Number Real JsInt IntSet)

  ;; gerbil-crypto
  (group-in :clan/crypto keccak secp256k1)

  ;; gerbil-persist
  (group-in :clan/persist content-addressing db persist merkle-trie)

  ;; gerbil-ethereum
  #;(group-in :clan/ethereum abi assembly assets cli contract-config erc20 ethereum
            evm-runtime hex json-rpc known-addresses logger meta-create2 network-config
            nonce-tracker presigned rlp simple-apps #;testing transaction tx-tracker
            types watch)

  (group-in :skyprotocol erasure-coding))

(import :clan/poo/brace)

;; COPY THE LINE BELOW to files you try to debug
(import :clan/debug)

;;(extern namespace: #f add-load-path) (add-load-path (application-source-directory))

;;(displayln "Welcome, Sky Protocol hacker")

;;(##set-debug-settings! 15 3) ;; TODO: remember what this is about

;; Force the stdout port width to 218… the port width is used by the pretty printer to determine when to break lines… the Gambit rtlib should export a higher level API than that hack
(##vector-set! ##stdout-port 37 (lambda (port) 218))
