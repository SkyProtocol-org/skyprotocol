(export #t)

(import
  (group-in :std/cli getopt multicall print-exit)
  (group-in :std error iter sugar)
  (group-in :std/misc bytes list number ports)
  (group-in :std/parser ll1)
  (group-in :std/srfi 1)
  (group-in :std/text basic-printers char-set)
  (group-in :std/debug DBG)


  ;; gerbil-utils
  (group-in :clan base cli)
  ;; gerbil-poo
  (group-in :clan/poo brace cli object number fq polynomial))

(def PolyFq {(:: @ Polynomial.) .Ring: F_2^256})

;; Split a block of data into shards with erasure coding.
;; To reconstitute the block of data, you need know its length, the two shard number parameters,
;; and have kept a sufficient number of shards, each with its index.
(def (split-u8vector sufficient-shards total-shards data)
  ;; Compute how many "words" there are
  (def datalen (u8vector-length data))
  (def wordlen (* sufficient-shards 32))
  (def n-words (ceiling-quotient datalen wordlen))

  ;; Initialize shards
  (def shards (make-vector total-shards #u8()))
  (for (i (iota total-shards)) (vector-set! shards i (make-u8vector (* n-words 32) 0)))

  ;; For each word, fill in each shard
  (def word (make-vector sufficient-shards 0))
  (for/collect (i (iota n-words))
    ;; Gather the word data as integers encoding the cofficients in F_2^256
    ;; of a polynomial of degree sufficient-shards - 1.
    (for ((j (iota sufficient-shards)))
      (let* ((k (+ (* i wordlen) (* j 32)))
             (l (- datalen k)))
        (vector-set! word j
                     (if (positive? l) (u8vector-uint-ref data k 'little (min l 32)) 0))))
    ;; Fill the shards with the values of polynomial at various points
    ;; (For now, points from 1 to total-shards, but could instead be powers
    ;; of a generator with sufficiently low order, to enable FFT)
    (for ((m (iota total-shards 0)))
      (u8vector-uint-set! (vector-ref shards m) (* i 32)
                          (.call PolyFq .apply word (1+ m)) 'little 32)))
  shards)


;; shards is an alist [[index . u8vector] ...] of at least sufficient-shards shards
(def (recover-split-u8vector sufficient-shards total-shards datalen shards)
  (def wordlen (* sufficient-shards 32))
  (def n-words (ceiling-quotient datalen wordlen))
  (def shardlen (* n-words 32))
  (check-argument (length>=n? shards sufficient-shards)
                  "Insufficient number of shards to recover data" [sufficient-shards shards])
  (check-argument (andmap (lambda (ib) (and (pair? ib)
                                       (exact-integer? (car ib))
                                       (<= 1 (car ib) total-shards)
                                       (u8vector? (cdr ib))
                                       (= (u8vector-length (cdr ib)) shardlen)))
                          shards)
                  "incorrect shard argument (wrong index? wrong length?)" shards)
  (def data (make-u8vector datalen 0))

  ;; For each word, fill in each shard
  (defvalues (interpolated-shards checksum-shards) (split-at shards sufficient-shards))
  (def interpolated-xs (map car interpolated-shards))
  (def interpolated-data (map cdr interpolated-shards))
  (def checksum-xs (map car checksum-shards))
  (def checksum-data (map cdr checksum-shards))

  (def decode (lagrange-interpolation PolyFq interpolated-xs))

  (for/collect (i (iota n-words))
    ;; Gather the word data as integers encoding the values ys at the given xs
    ;; in F_2^256 of a polynomial of degree sufficient-shards - 1.
    (def j (* i 32))
    (def ys (for/collect (v interpolated-data) (u8vector-uint-ref v j 'little 32)))
    (def word (decode ys))
    (unless (andmap (lambda (x ck) (= (.call PolyFq .apply word x)
                                 (u8vector-uint-ref ck j 'little 32)))
                    checksum-xs checksum-data)
      (error "redundant shards fail to checksum"))
    (for ((w word)
          (k (iota sufficient-shards (* i wordlen) 32)))
      (let ((l (- datalen k)))
        (when (positive? l)
          (u8vector-uint-set! data k w 'little (min l 32))))))
  data)


(def (->int x)
  (cond
   ((exact-integer? x) x)
   ((string? x) (let ((n (string->number x)))
                  (check-argument-exact-integer n)
                  n))
   (else (error "not an integer" x))))

;; TODO: accept alternative ethereum networks, etc
;; TODO: Option spec should be able to take in parsers in option spec
;; to parse the options, using `getopt-parse`.
(define-entry-point (split-data
                     sufficient-shards: (sufficient-shards 34)
                     total-shards: (total-shards 100)
                     datafile)
  (help: "split a data file into shards"
   getopt: (make-options
            [(option 'sufficient-shards "-S" "--sufficient-shards" default: "34"
                     help: "shards sufficient to reconstitute data")
             (option 'total-shards "-T" "--total-shards" default: "100"
                     help: "total number of shards")
             (argument 'datafile
                     help: "data file")]))
  (set! sufficient-shards (->int sufficient-shards))
  (set! total-shards (->int total-shards))
  (check-argument (<= 1 sufficient-shards total-shards) "bad shard spec"
                  [sufficient-shards total-shards])
  (check-argument (file-exists? datafile) "file does not exist" datafile)
  (def n-digits (1+ (inexact->exact (floor (log total-shards 10)))))
  (def shards (split-u8vector sufficient-shards total-shards (read-file-u8vector datafile)))
  (for ((i (iota total-shards)) (s shards))
    (let ((f (string-append datafile "." (display-integer/fit (1+ i) n-digits #f))))
      (with-output (o f) (write-u8vector s o)))))

(define-entry-point (recover-split-data
                     sufficient-shards: (sufficient-shards 34)
                     total-shards: (total-shards 100)
                     datalen
                     datafile)
  (help: "recover data file that was split into shards"
   getopt: (make-options
            [(option 'sufficient-shards "-S" "--sufficient-shards" default: "34"
                     help: "shards sufficient to reconstitute data")
             (option 'total-shards "-T" "--total-shards" default: "100"
                     help: "total number of shards")
             (argument 'datalen
                     help: "length of the data file")
             (argument 'datafile
                     help: "name of the data file")]))
  (set! sufficient-shards (->int sufficient-shards))
  (set! total-shards (->int total-shards))
  (set! datalen (->int datalen))
  (check-argument (<= 1 sufficient-shards total-shards) "bad shard spec"
                  [sufficient-shards total-shards])
  (def n-digits (1+ (inexact->exact (floor (log total-shards 10)))))
  (def dir (path-directory datafile))
  (def parser
    (ll1-to-eof (ll1-begin (ll1-string (path-strip-directory datafile))
                           (ll1-char #\.)
                           (ll1-n-chars n-digits char-ascii-digit))))
  (def (file->shard-index f)
    (with-catch false (cut string->number (ll1/string parser f))))
  (def shard-files (filter file->shard-index (directory-files dir)))

  (def shards (for/collect (f shard-files)
                (cons (file->shard-index f) (read-file-u8vector f))))
  (def data (recover-split-u8vector sufficient-shards total-shards datalen shards))
  (with-output (o datafile) (write-u8vector data o)))
