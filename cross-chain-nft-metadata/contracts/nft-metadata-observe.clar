;; Cross-Chain NFT Metadata Observatory
;; A decentralized platform that tracks, aggregates, and standardizes NFT metadata across multiple blockchains

;; ---------- Constants ----------

;; Contract owner
(define-constant CONTRACT_OWNER tx-sender)

;; Error codes
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_ALREADY_REGISTERED (err u101))
(define-constant ERR_NOT_FOUND (err u102))
(define-constant ERR_INVALID_CHAIN (err u103))
(define-constant ERR_INVALID_METADATA (err u104))
(define-constant ERR_INVALID_TOKEN (err u105))
(define-constant ERR_INVALID_SIGNATURE (err u106))
(define-constant ERR_EXPIRED (err u107))

;; Chain identifiers
(define-constant CHAIN_STACKS u1)
(define-constant CHAIN_ETHEREUM u2)
(define-constant CHAIN_POLYGON u3)
(define-constant CHAIN_SOLANA u4)
(define-constant CHAIN_AVALANCHE u5)

;; ---------- Data Structures ----------

;; Chain Registry - keeps track of supported chains
(define-map chains uint {
  name: (string-ascii 64),
  active: bool,
  metadata-format: (string-ascii 64),
  bridge-contract: (optional (string-ascii 64))
})

;; Authorized Oracles - entities that can verify cross-chain metadata
(define-map authorized-oracles principal {
  name: (string-ascii 64),
  active: bool,
  verification-count: uint
})

;; NFT Registry - maps NFT identifiers to their metadata
(define-map nft-registry 
  { chain-id: uint, contract-address: (string-ascii 64), token-id: (string-ascii 64) }
  { 
    metadata-uri: (string-ascii 255), 
    metadata-hash: (buff 32),
    registered-by: principal,
    verified: bool,
    verified-by: (optional principal),
    last-updated: uint,
    version: uint
  }
)

;; Cross-chain metadata verification requests
(define-map verification-requests uint {
  chain-id: uint,
  contract-address: (string-ascii 64),
  token-id: (string-ascii 64),
  requested-by: principal,
  oracle: principal,
  status: (string-ascii 16),
  created-at: uint,
  expires-at: uint
})

;; ---------- Variables ----------

;; Counter for verification requests
(define-data-var request-counter uint u0)

;; Contract admin for future upgrades
(define-data-var contract-admin principal CONTRACT_OWNER)

;; ---------- Authorization Functions ----------

;; Check if caller is contract owner
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT_OWNER))

;; Check if caller is contract admin
(define-private (is-contract-admin)
  (is-eq tx-sender (var-get contract-admin)))

;; Check if caller is an authorized oracle
(define-private (is-authorized-oracle (caller principal))
  (default-to false (get active (map-get? authorized-oracles caller))))

;; ---------- Admin Functions ----------

;; Set new contract admin
(define-public (set-contract-admin (new-admin principal))
  (begin
    (asserts! (or (is-contract-owner) (is-contract-admin)) ERR_UNAUTHORIZED)
    (ok (var-set contract-admin new-admin))))

;; Initialize supported chain
(define-public (register-chain 
  (chain-id uint) 
  (chain-name (string-ascii 64)) 
  (metadata-format (string-ascii 64))
  (bridge-contract (optional (string-ascii 64))))
  (begin
    (asserts! (or (is-contract-owner) (is-contract-admin)) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? chains chain-id)) ERR_ALREADY_REGISTERED)
    (ok (map-set chains chain-id {
      name: chain-name,
      active: true,
      metadata-format: metadata-format,
      bridge-contract: bridge-contract
    }))))

;; Update chain status (enable/disable)
(define-public (update-chain-status (chain-id uint) (active bool))
  (begin
    (asserts! (or (is-contract-owner) (is-contract-admin)) ERR_UNAUTHORIZED)
    (asserts! (is-some (map-get? chains chain-id)) ERR_NOT_FOUND)
    (let ((chain (unwrap! (map-get? chains chain-id) ERR_NOT_FOUND)))
      (ok (map-set chains chain-id (merge chain { active: active }))))))

;; Register new oracle
(define-public (register-oracle (oracle principal) (name (string-ascii 64)))
  (begin
    (asserts! (or (is-contract-owner) (is-contract-admin)) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? authorized-oracles oracle)) ERR_ALREADY_REGISTERED)
    (ok (map-set authorized-oracles oracle {
      name: name,
      active: true,
      verification-count: u0
    }))))

;; Update oracle status
(define-public (update-oracle-status (oracle principal) (active bool))
  (begin
    (asserts! (or (is-contract-owner) (is-contract-admin)) ERR_UNAUTHORIZED)
    (asserts! (is-some (map-get? authorized-oracles oracle)) ERR_NOT_FOUND)
    (let ((oracle-data (unwrap! (map-get? authorized-oracles oracle) ERR_NOT_FOUND)))
      (ok (map-set authorized-oracles oracle (merge oracle-data { active: active }))))))


;; ---------- NFT Registration Functions ----------

;; Register new NFT metadata
(define-public (register-nft-metadata
  (chain-id uint)
  (contract-address (string-ascii 64))
  (token-id (string-ascii 64))
  (metadata-uri (string-ascii 255))
  (metadata-hash (buff 32)))
  (begin
    ;; Verify the chain is supported and active
    (asserts! (is-chain-active chain-id) ERR_INVALID_CHAIN)
    
    ;; Create the NFT identifier
    (let ((nft-id { 
            chain-id: chain-id, 
            contract-address: contract-address, 
            token-id: token-id 
          }))
      ;; Check if NFT is already registered
      (if (is-some (map-get? nft-registry nft-id))
        ;; Only allow updates by original registrar or admin
        (let ((existing-data (unwrap! (map-get? nft-registry nft-id) ERR_NOT_FOUND)))
          (asserts! (or 
            (is-eq tx-sender (get registered-by existing-data))
            (is-contract-admin)
            (is-contract-owner)) 
            ERR_UNAUTHORIZED)
          ;; Update the existing entry with new version
          (ok (map-set nft-registry nft-id {
            metadata-uri: metadata-uri,
            metadata-hash: metadata-hash,
            registered-by: (get registered-by existing-data),
            verified: false,  ;; Reset verification status on update
            verified-by: none,
            last-updated: block-height,
            version: (+ u1 (get version existing-data))
          })))
        ;; New registration
        (ok (map-set nft-registry nft-id {
          metadata-uri: metadata-uri,
          metadata-hash: metadata-hash,
          registered-by: tx-sender,
          verified: false,
          verified-by: none,
          last-updated: block-height,
          version: u1
        }))))))

;; Check if an NFT is registered
(define-read-only (is-nft-registered 
  (chain-id uint) 
  (contract-address (string-ascii 64)) 
  (token-id (string-ascii 64)))
  (is-some (map-get? nft-registry { 
    chain-id: chain-id, 
    contract-address: contract-address, 
    token-id: token-id 
  })))

;; Get NFT metadata
(define-read-only (get-nft-metadata
  (chain-id uint)
  (contract-address (string-ascii 64))
  (token-id (string-ascii 64)))
  (map-get? nft-registry { 
    chain-id: chain-id, 
    contract-address: contract-address, 
    token-id: token-id 
  }))

;; Check if chain is active
(define-read-only (is-chain-active (chain-id uint))
  (default-to false (get active (map-get? chains chain-id))))

;; Batch registration of multiple NFTs (for efficiency)
(define-public (batch-register-nft-metadata
  (entries (list 10 {
    chain-id: uint,
    contract-address: (string-ascii 64),
    token-id: (string-ascii 64),
    metadata-uri: (string-ascii 255),
    metadata-hash: (buff 32)
  })))
  (begin
    (fold register-nft-batch entries (ok true))))

;; Helper function for batch registration
(define-private (register-nft-batch 
  (entry {
    chain-id: uint,
    contract-address: (string-ascii 64),
    token-id: (string-ascii 64),
    metadata-uri: (string-ascii 255),
    metadata-hash: (buff 32)
  })
  (previous-result (response bool uint)))
  (match previous-result
    success (register-nft-metadata 
              (get chain-id entry)
              (get contract-address entry)
              (get token-id entry)
              (get metadata-uri entry)
              (get metadata-hash entry))
    failure (err failure)))

;; Update metadata URI for existing NFT
(define-public (update-metadata-uri
  (chain-id uint)
  (contract-address (string-ascii 64))
  (token-id (string-ascii 64))
  (new-metadata-uri (string-ascii 255))
  (new-metadata-hash (buff 32)))
  (let ((nft-id { 
          chain-id: chain-id, 
          contract-address: contract-address, 
          token-id: token-id 
        })
        (existing-data (unwrap! (map-get? nft-registry nft-id) ERR_NOT_FOUND)))
    ;; Only allow updates by original registrar or admin
    (asserts! (or 
      (is-eq tx-sender (get registered-by existing-data))
      (is-contract-admin)
      (is-contract-owner)) 
      ERR_UNAUTHORIZED)
    ;; Update the metadata URI and hash, increment version
    (ok (map-set nft-registry nft-id (merge existing-data {
      metadata-uri: new-metadata-uri,
      metadata-hash: new-metadata-hash,
      last-updated: block-height,
      verified: false,  ;; Reset verification on update
      verified-by: none,
      version: (+ u1 (get version existing-data))
    })))))

;; Delete NFT metadata (admin only)
(define-public (delete-nft-metadata
  (chain-id uint)
  (contract-address (string-ascii 64))
  (token-id (string-ascii 64)))
  (begin
    (asserts! (or (is-contract-owner) (is-contract-admin)) ERR_UNAUTHORIZED)
    (let ((nft-id { 
            chain-id: chain-id, 
            contract-address: contract-address, 
            token-id: token-id 
          }))
      (asserts! (is-some (map-get? nft-registry nft-id)) ERR_NOT_FOUND)
      (ok (map-delete nft-registry nft-id)))))