(import :clan/poo/object
        :clan/poo/type
        :clan/poo/mop
        :clan/poo/number
        :clan/poo/brace
        :clan/persist/content-addressing
        :clan/persist/merkle-trie
        )
(export #t)

(define-type (UInt6 @ (UIntN 6)))
(define-type (UInt8 @ (UIntN 8)))
(define-type (UInt64 @ (UIntN 64)))
;;(define-type (UInt256 @ (UIntN 256)))

(define-type PeerAddress String) ;; the peer
(define-type PeerId Bytes) ;; the id
(define-type TopicId UInt256) ;; topic id ;; (Hash TopicDescription)
(define-type TopicDescription Bytes)
(define-type Certificate Bytes) ;; TODO is this a merkle proof?
(define-type TopicTop
  (Record
    height: [UInt]
    certificate: [Certificate]))
(define-type BlockData Bytes)
(define-type TopicIndex
  (Record
    topic: [TopicId]
    height: [UInt]))


;; TODO:
;; 1. define blake2b-addressing and replace keccak-addressing by it.
;; 2. Find a better type for Bytes?
;; 3. add support in MerkleTrie for "forgetting" old nodes but still having their hash
;; (some methods will throw an exception if they have to look inside those forgotten nodes)
;; Maybe "just" make the wrapper a maybe-forgetful one.
(define-type MessageTrie (MerkleTrie Key: UInt64 Height: UInt6 Value: Bytes
                                     Digesting: keccak-addressing))

(define-type TopicTrie (MerkleTrie Key: TopicId Height: UInt8 Value: MessageTrie
                                   Digesting: keccak-addressing))
