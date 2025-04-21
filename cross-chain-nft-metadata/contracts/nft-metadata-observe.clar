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


;; Initialize subscription prices
(map-set subscription-prices TIER_FREE u0)          ;; Free tier: 0 STX
(map-set subscription-prices TIER_BASIC u10000000)  ;; Basic tier: 10 STX
(map-set subscription-prices TIER_PREMIUM u50000000) ;; Premium tier: 50 STX
(map-set subscription-prices TIER_ENTERPRISE u100000000) ;; Enterprise tier: 100 STX

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


;; ---------- Verification System ----------

;; Create a verification request
(define-public (request-verification
  (chain-id uint)
  (contract-address (string-ascii 64))
  (token-id (string-ascii 64))
  (oracle principal))
  (begin
    ;; Check if the NFT is registered
    (let ((nft-id { 
            chain-id: chain-id, 
            contract-address: contract-address, 
            token-id: token-id 
          }))
      (asserts! (is-some (map-get? nft-registry nft-id)) ERR_NOT_FOUND)
      ;; Check if the oracle is authorized
      (asserts! (is-authorized-oracle oracle) ERR_UNAUTHORIZED)
      ;; Increment request counter
      (var-set request-counter (+ (var-get request-counter) u1))
      ;; Create verification request with expiration (100 blocks from now)
      (ok (map-set verification-requests (var-get request-counter) {
        chain-id: chain-id,
        contract-address: contract-address,
        token-id: token-id,
        requested-by: tx-sender,
        oracle: oracle,
        status: "pending",
        created-at: block-height,
        expires-at: (+ block-height u100)
      })))))

;; Get verification request by ID
(define-read-only (get-verification-request (request-id uint))
  (map-get? verification-requests request-id))

;; Get current request counter
(define-read-only (get-request-counter)
  (var-get request-counter))

;; Fulfill verification request (called by oracle)
(define-public (verify-nft-metadata
  (request-id uint)
  (is-valid bool)
  (signature (buff 65)))
  (let ((request (unwrap! (map-get? verification-requests request-id) ERR_NOT_FOUND)))
    ;; Check if caller is the assigned oracle
    (asserts! (is-eq tx-sender (get oracle request)) ERR_UNAUTHORIZED)
    ;; Check if request is pending
    (asserts! (is-eq (get status request) "pending") ERR_INVALID_TOKEN)
    ;; Check if request is not expired
    (asserts! (<= block-height (get expires-at request)) ERR_EXPIRED)
    
    ;; Verify the signature (simplified, in practice would validate with chain-specific logic)
    (asserts! (verify-signature-format signature) ERR_INVALID_SIGNATURE)
    
    ;; Get the NFT data
    (let ((nft-id { 
            chain-id: (get chain-id request), 
            contract-address: (get contract-address request), 
            token-id: (get token-id request) 
          })
          (nft-data (unwrap! (map-get? nft-registry nft-id) ERR_NOT_FOUND))
          (oracle-data (unwrap! (map-get? authorized-oracles tx-sender) ERR_UNAUTHORIZED)))
      
      ;; Update verification request status
      (map-set verification-requests request-id (merge request {
        status: (if is-valid "verified" "rejected")
      }))
      
      ;; Update oracle verification count
      (map-set authorized-oracles tx-sender (merge oracle-data {
        verification-count: (+ u1 (get verification-count oracle-data))
      }))
      
      ;; Update NFT verification status if valid
      (if is-valid
        (map-set nft-registry nft-id (merge nft-data {
          verified: true,
          verified-by: (some tx-sender)
        }))
        false)
      
      (ok is-valid))))

;; Cancel verification request (by requester or admin)
(define-public (cancel-verification-request (request-id uint))
  (let ((request (unwrap! (map-get? verification-requests request-id) ERR_NOT_FOUND)))
    ;; Only original requester or admin can cancel
    (asserts! (or 
      (is-eq tx-sender (get requested-by request))
      (is-contract-admin)
      (is-contract-owner)) 
      ERR_UNAUTHORIZED)
    ;; Check if request is pending
    (asserts! (is-eq (get status request) "pending") ERR_INVALID_TOKEN)
    
    ;; Update status to cancelled
    (ok (map-set verification-requests request-id (merge request {
      status: "cancelled"
    })))))

;; Simple signature format verification (placeholder)
;; In production, this would perform proper cryptographic validation
(define-private (verify-signature-format (signature (buff 65)))
  (is-eq (len signature) u65))

;; Get all verification requests for a specific NFT
(define-read-only (get-verification-history
  (chain-id uint)
  (contract-address (string-ascii 64))
  (token-id (string-ascii 64)))
  ;; Note: In practice, this would require off-chain indexing
  ;; This is a placeholder function that would rely on event indexing
  (ok { chain-id: chain-id, total-verifications: u0 }))

;; Check if an NFT is verified
(define-read-only (is-nft-verified
  (chain-id uint)
  (contract-address (string-ascii 64))
  (token-id (string-ascii 64)))
  (let ((nft-data (default-to 
                    { 
                      metadata-uri: "", 
                      metadata-hash: 0x, 
                      registered-by: CONTRACT_OWNER, 
                      verified: false, 
                      verified-by: none, 
                      last-updated: u0, 
                      version: u0 
                    }
                    (map-get? nft-registry { 
                      chain-id: chain-id, 
                      contract-address: contract-address, 
                      token-id: token-id 
                    }))))
    (get verified nft-data)))

;; ---------- Cross-Chain Bridge Interface ----------

;; Cross-chain message structure for bridge communication
(define-map bridge-messages uint {
  source-chain-id: uint,
  target-chain-id: uint,
  message-type: (string-ascii 32),
  payload: (buff 1024),
  status: (string-ascii 16),
  txid: (buff 32),
  created-at: uint,
  processed-at: (optional uint)
})

;; Bridge message counter
(define-data-var bridge-message-counter uint u0)

;; Bridge protocol version
(define-data-var bridge-protocol-version (string-ascii 16) "1.0.0")

;; Bridge fee (in microSTX)
(define-data-var bridge-fee uint u1000000) ;; 1 STX

;; Submit metadata to external chain through bridge
(define-public (submit-to-external-chain
  (target-chain-id uint)
  (contract-address (string-ascii 64))
  (token-id (string-ascii 64))
  (payload-data (buff 1024)))
  (begin
    ;; Check if the target chain is supported
    (let ((chain (unwrap! (map-get? chains target-chain-id) ERR_INVALID_CHAIN)))
      ;; Check if chain has a bridge contract configured
      (asserts! (is-some (get bridge-contract chain)) ERR_INVALID_CHAIN)
      ;; Check if chain is active
      (asserts! (get active chain) ERR_INVALID_CHAIN)
      
      ;; Check if the STX fee is provided
      (asserts! (>= (stx-get-balance tx-sender) (var-get bridge-fee)) ERR_UNAUTHORIZED)
      
      ;; Increment bridge message counter
      (var-set bridge-message-counter (+ (var-get bridge-message-counter) u1))
      
      ;; Create bridge message
      (map-set bridge-messages (var-get bridge-message-counter) {
        source-chain-id: CHAIN_STACKS,
        target-chain-id: target-chain-id,
        message-type: "metadata-update",
        payload: payload-data,
        status: "pending",
        txid: (sha256 payload-data), ;; Simplified transaction ID
        created-at: block-height,
        processed-at: none
      })
      
      ;; Charge the bridge fee
      (try! (stx-transfer? (var-get bridge-fee) tx-sender CONTRACT_OWNER))
      
      (ok (var-get bridge-message-counter)))))

;; Confirm bridge message processed on target chain
(define-public (confirm-bridge-message
  (message-id uint)
  (target-chain-txid (buff 32))
  (proof (buff 65)))
  (begin
    ;; Only contract admin or oracle can confirm bridge messages
    (asserts! (or 
      (is-contract-admin) 
      (is-contract-owner)
      (is-authorized-oracle tx-sender)) 
      ERR_UNAUTHORIZED)
      
    (let ((message (unwrap! (map-get? bridge-messages message-id) ERR_NOT_FOUND)))
      ;; Check if message is in pending state
      (asserts! (is-eq (get status message) "pending") ERR_INVALID_TOKEN)
      
      ;; Verify the proof (simplified)
      (asserts! (verify-bridge-proof proof target-chain-txid) ERR_INVALID_SIGNATURE)
      
      ;; Update message status
      (ok (map-set bridge-messages message-id (merge message {
        status: "completed",
        processed-at: (some block-height)
      }))))))

;; Register incoming metadata from external chain
(define-public (register-external-metadata
  (source-chain-id uint)
  (external-txid (buff 32))
  (contract-address (string-ascii 64))
  (token-id (string-ascii 64))
  (metadata-uri (string-ascii 255))
  (metadata-hash (buff 32))
  (proof (buff 65)))
  (begin
    ;; Only authorized oracles can register external metadata
    (asserts! (is-authorized-oracle tx-sender) ERR_UNAUTHORIZED)
    
    ;; Verify the external chain is supported
    (asserts! (is-chain-active source-chain-id) ERR_INVALID_CHAIN)
    
    ;; Verify the proof (simplified)
    (asserts! (verify-external-proof 
      proof 
      source-chain-id 
      external-txid 
      contract-address 
      token-id) 
      ERR_INVALID_SIGNATURE)
    
    ;; Register the NFT metadata with verified status
    (let ((nft-id { 
            chain-id: source-chain-id, 
            contract-address: contract-address, 
            token-id: token-id 
          }))
      (if (is-some (map-get? nft-registry nft-id))
        ;; Update existing entry
        (let ((existing-data (unwrap! (map-get? nft-registry nft-id) ERR_NOT_FOUND)))
          (map-set nft-registry nft-id (merge existing-data {
            metadata-uri: metadata-uri,
            metadata-hash: metadata-hash,
            verified: true,
            verified-by: (some tx-sender),
            last-updated: block-height,
            version: (+ u1 (get version existing-data))
          })))
        ;; New registration
        (map-set nft-registry nft-id {
          metadata-uri: metadata-uri,
          metadata-hash: metadata-hash,
          registered-by: tx-sender,
          verified: true,
          verified-by: (some tx-sender),
          last-updated: block-height,
          version: u1
        }))
      
      ;; Return the transaction hash to track this update
      (ok external-txid))))

;; Set bridge fee
(define-public (set-bridge-fee (new-fee uint))
  (begin
    (asserts! (or (is-contract-owner) (is-contract-admin)) ERR_UNAUTHORIZED)
    (ok (var-set bridge-fee new-fee))))

;; Get bridge fee
(define-read-only (get-bridge-fee)
  (var-get bridge-fee))

;; Get bridge protocol version
(define-read-only (get-bridge-protocol-version)
  (var-get bridge-protocol-version))

;; Update bridge protocol version (admin only)
(define-public (update-bridge-protocol-version (new-version (string-ascii 16)))
  (begin
    (asserts! (or (is-contract-owner) (is-contract-admin)) ERR_UNAUTHORIZED)
    (ok (var-set bridge-protocol-version new-version))))

;; Get bridge message by ID
(define-read-only (get-bridge-message (message-id uint))
  (map-get? bridge-messages message-id))

;; Simplified bridge proof verification
;; In production, this would use proper cryptographic verification
(define-private (verify-bridge-proof (proof (buff 65)) (txid (buff 32)))
  (and (is-eq (len proof) u65) (> (len txid) u0)))

;; Simplified external proof verification
;; In production, this would verify the proof against the source chain
(define-private (verify-external-proof 
  (proof (buff 65)) 
  (source-chain-id uint)
  (txid (buff 32))
  (contract-address (string-ascii 64))
  (token-id (string-ascii 64)))
  (and 
    (is-eq (len proof) u65) 
    (> (len txid) u0)
    (is-chain-active source-chain-id)))

;; ---------- Subscription and Governance System ----------

;; Subscription tiers
(define-constant TIER_FREE u0)
(define-constant TIER_BASIC u1)
(define-constant TIER_PREMIUM u2)
(define-constant TIER_ENTERPRISE u3)

;; Subscription prices (in microSTX)
(define-map subscription-prices uint uint)

;; User subscriptions
(define-map user-subscriptions principal {
  tier: uint,
  start-block: uint,
  end-block: uint,
  auto-renew: bool
})

;; Governance proposals
(define-map governance-proposals uint {
  title: (string-ascii 64),
  description: (string-ascii 255),
  proposed-by: principal,
  status: (string-ascii 16),
  created-at: uint,
  expires-at: uint,
  votes-for: uint,
  votes-against: uint,
  execution-params: (optional (buff 1024))
})

;; Governance votes
(define-map governance-votes 
  { proposal-id: uint, voter: principal } 
  { vote: bool, weight: uint }
)

;; Proposal counter
(define-data-var proposal-counter uint u0)

;; ---------- Subscription Functions ----------

;; Subscribe to a tier
(define-public (subscribe (tier uint) (duration uint) (auto-renew bool))
  (begin
    ;; Check if tier exists
    (asserts! (is-some (map-get? subscription-prices tier)) ERR_INVALID_TOKEN)
    ;; Calculate subscription cost
    (let ((price (unwrap! (map-get? subscription-prices tier) ERR_INVALID_TOKEN))
          (total-cost (* price duration)))
      ;; Check if user has enough STX
      (asserts! (>= (stx-get-balance tx-sender) total-cost) ERR_UNAUTHORIZED)
      
      ;; Process payment
      (try! (stx-transfer? total-cost tx-sender CONTRACT_OWNER))
      
      ;; Update or create subscription
      (ok (map-set user-subscriptions tx-sender {
        tier: tier,
        start-block: block-height,
        end-block: (+ block-height (* duration u144)), ;; Approx. blocks per 24h * days
        auto-renew: auto-renew
      })))))

;; Get user subscription
(define-read-only (get-user-subscription (user principal))
  (map-get? user-subscriptions user))

;; Check if user has active subscription
(define-read-only (has-active-subscription (user principal))
  (match (map-get? user-subscriptions user)
    subscription (>= (get end-block subscription) block-height)
    false))

;; Get subscription tier
(define-read-only (get-user-tier (user principal))
  (let ((subscription (default-to 
                        { tier: TIER_FREE, start-block: u0, end-block: u0, auto-renew: false }
                        (map-get? user-subscriptions user))))
    (get tier subscription)))

;; Set subscription price (admin only)
(define-public (set-subscription-price (tier uint) (price uint))
  (begin
    (asserts! (or (is-contract-owner) (is-contract-admin)) ERR_UNAUTHORIZED)
    (ok (map-set subscription-prices tier price))))

;; Cancel subscription
(define-public (cancel-subscription)
  (begin
    (asserts! (is-some (map-get? user-subscriptions tx-sender)) ERR_NOT_FOUND)
    (let ((subscription (unwrap! (map-get? user-subscriptions tx-sender) ERR_NOT_FOUND)))
      (ok (map-set user-subscriptions tx-sender (merge subscription {
        auto-renew: false
      }))))))

;; ---------- Governance Functions ----------

;; Create a new governance proposal
(define-public (create-proposal 
  (title (string-ascii 64)) 
  (description (string-ascii 255))
  (duration uint)
  (execution-params (optional (buff 1024))))
  (begin
    ;; Only subscribers above FREE tier can create proposals
    (asserts! (> (get-user-tier tx-sender) TIER_FREE) ERR_UNAUTHORIZED)
    (asserts! (has-active-subscription tx-sender) ERR_UNAUTHORIZED)
    
    ;; Increment proposal counter
    (var-set proposal-counter (+ (var-get proposal-counter) u1))
    
    ;; Create new proposal
    (ok (map-set governance-proposals (var-get proposal-counter) {
      title: title,
      description: description,
      proposed-by: tx-sender,
      status: "active",
      created-at: block-height,
      expires-at: (+ block-height (* duration u144)), ;; Approx. blocks per 24h * days
      votes-for: u0,
      votes-against: u0,
      execution-params: execution-params
    }))))

;; Vote on a governance proposal
(define-public (vote-on-proposal (proposal-id uint) (vote bool))
  (let ((proposal (unwrap! (map-get? governance-proposals proposal-id) ERR_NOT_FOUND)))
    ;; Check if proposal is active
    (asserts! (is-eq (get status proposal) "active") ERR_INVALID_TOKEN)
    ;; Check if proposal has not expired
    (asserts! (<= block-height (get expires-at proposal)) ERR_EXPIRED)
    ;; Check if user has active subscription
    (asserts! (has-active-subscription tx-sender) ERR_UNAUTHORIZED)
    
    ;; Calculate vote weight based on tier
    (let ((vote-weight (pow u2 (get-user-tier tx-sender))))
      ;; Check if user has already voted
      (match (map-get? governance-votes { proposal-id: proposal-id, voter: tx-sender })
        previous-vote 
          ;; Update existing vote
          (begin
            ;; Adjust vote counts
            (map-set governance-proposals proposal-id 
              (merge proposal {
                votes-for: (if vote 
                              (+ (get votes-for proposal) vote-weight)
                              (get votes-for proposal)),
                votes-against: (if vote 
                                  (get votes-against proposal)
                                  (+ (get votes-against proposal) vote-weight))
              }))
            (ok (map-set governance-votes 
                  { proposal-id: proposal-id, voter: tx-sender }
                  { vote: vote, weight: vote-weight })))
        ;; First vote from this user
        (begin
          ;; Adjust vote counts
          (map-set governance-proposals proposal-id 
            (merge proposal {
              votes-for: (if vote 
                            (+ (get votes-for proposal) vote-weight)
                            (get votes-for proposal)),
              votes-against: (if vote 
                                (get votes-against proposal)
                                (+ (get votes-against proposal) vote-weight))
            }))
          (ok (map-set governance-votes 
                { proposal-id: proposal-id, voter: tx-sender }
                { vote: vote, weight: vote-weight })))))))

;; Execute a passed proposal (admin only)
(define-public (execute-proposal (proposal-id uint))
  (begin
    (asserts! (or (is-contract-owner) (is-contract-admin)) ERR_UNAUTHORIZED)
    (let ((proposal (unwrap! (map-get? governance-proposals proposal-id) ERR_NOT_FOUND)))
      ;; Check if proposal is active and expired (voting period over)
      (asserts! (and (is-eq (get status proposal) "active")
                    (> block-height (get expires-at proposal))) 
                ERR_INVALID_TOKEN)
      
      ;; Check if proposal passed (more votes for than against)
      (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR_UNAUTHORIZED)
      
      ;; Update proposal status
      (ok (map-set governance-proposals proposal-id (merge proposal {
        status: "executed"
      }))))))

;; ---------- API Integration and Advanced Features ----------

;; API usage tracking per user
(define-map api-usage principal {
  daily-requests: uint,
  monthly-requests: uint,
  last-reset-day: uint,
  last-reset-month: uint
})

;; API rate limits per subscription tier
(define-map tier-rate-limits uint {
  daily-limit: uint,
  monthly-limit: uint,
  concurrent-requests: uint
})

;; Metadata standard registry
(define-map metadata-standards uint {
  name: (string-ascii 64),
  schema: (string-ascii 255),
  supported-chains: (list 10 uint),
  created-by: principal,
  created-at: uint,
  active: bool
})

;; Standard counter
(define-data-var standard-counter uint u0)

;; Chain migration mappings
(define-map chain-mappings {
    source-chain: uint,
    target-chain: uint,
    source-contract: (string-ascii 64)
  } {
    target-contract: (string-ascii 64),
    migration-status: (string-ascii 16),
    migrated-count: uint
})

;; ---------- API Rate Limiting Functions ----------

;; Track API usage
(define-public (track-api-request (user principal))
  (begin
    (let ((current-day (/ block-height u144))  ;; Approximate day number
          (current-month (/ block-height u4320)))  ;; Approximate month number
      
      (match (map-get? api-usage user)
        existing-usage 
          (if (is-eq (get last-reset-day existing-usage) current-day)
            ;; Same day, increment counter
            (map-set api-usage user (merge existing-usage {
              daily-requests: (+ (get daily-requests existing-usage) u1),
              monthly-requests: (+ (get monthly-requests existing-usage) u1)
            }))
            ;; New day, reset daily counter
            (map-set api-usage user {
              daily-requests: u1,
              monthly-requests: (if (is-eq (get last-reset-month existing-usage) current-month)
                                  (+ (get monthly-requests existing-usage) u1)
                                  u1),
              last-reset-day: current-day,
              last-reset-month: (if (is-eq (get last-reset-month existing-usage) current-month)
                                  (get last-reset-month existing-usage)
                                  current-month)
            }))
        ;; New user
        (map-set api-usage user {
          daily-requests: u1,
          monthly-requests: u1,
          last-reset-day: current-day,
          last-reset-month: current-month
        })))
    
    (ok true)))

;; Check if user is within rate limits
(define-read-only (check-rate-limit (user principal))
  (let ((tier (get-user-tier user))
        (usage (default-to 
                { 
                  daily-requests: u0, 
                  monthly-requests: u0, 
                  last-reset-day: u0, 
                  last-reset-month: u0 
                } 
                (map-get? api-usage user)))
        (limits (default-to 
                { 
                  daily-limit: u0, 
                  monthly-limit: u0, 
                  concurrent-requests: u0 
                } 
                (map-get? tier-rate-limits tier))))
    
    (and (<= (get daily-requests usage) (get daily-limit limits))
         (<= (get monthly-requests usage) (get monthly-limit limits)))))

;; Set rate limits for a tier
(define-public (set-rate-limits 
  (tier uint) 
  (daily-limit uint) 
  (monthly-limit uint)
  (concurrent-requests uint))
  (begin
    (asserts! (or (is-contract-owner) (is-contract-admin)) ERR_UNAUTHORIZED)
    (ok (map-set tier-rate-limits tier {
      daily-limit: daily-limit,
      monthly-limit: monthly-limit,
      concurrent-requests: concurrent-requests
    }))))

;; ---------- Metadata Standard Functions ----------

;; Register a new metadata standard
(define-public (register-metadata-standard 
  (name (string-ascii 64))
  (schema (string-ascii 255))
  (supported-chains (list 10 uint)))
  (begin
    ;; Only premium or enterprise subscribers can register standards
    (asserts! (>= (get-user-tier tx-sender) TIER_PREMIUM) ERR_UNAUTHORIZED)
    (asserts! (has-active-subscription tx-sender) ERR_UNAUTHORIZED)
    
    ;; Validate all chains in the list
    (asserts! (fold check-chain-exists supported-chains true) ERR_INVALID_CHAIN)
    
    ;; Increment standard counter
    (var-set standard-counter (+ (var-get standard-counter) u1))
    
    ;; Register the standard
    (ok (map-set metadata-standards (var-get standard-counter) {
      name: name,
      schema: schema,
      supported-chains: supported-chains,
      created-by: tx-sender,
      created-at: block-height,
      active: true
    }))))

;; Helper function to check if a chain exists
(define-private (check-chain-exists (chain-id uint) (previous-result bool))
  (and previous-result (is-some (map-get? chains chain-id))))

;; Update metadata standard (creator or admin only)
(define-public (update-metadata-standard 
  (standard-id uint)
  (schema (string-ascii 255))
  (supported-chains (list 10 uint)))
  (let ((standard (unwrap! (map-get? metadata-standards standard-id) ERR_NOT_FOUND)))
    ;; Only creator or admin can update
    (asserts! (or 
      (is-eq tx-sender (get created-by standard))
      (is-contract-admin)
      (is-contract-owner)) 
      ERR_UNAUTHORIZED)
    
    ;; Validate all chains in the list
    (asserts! (fold check-chain-exists supported-chains true) ERR_INVALID_CHAIN)
    
    ;; Update the standard
    (ok (map-set metadata-standards standard-id (merge standard {
      schema: schema,
      supported-chains: supported-chains
    })))))

;; Get metadata standard by ID
(define-read-only (get-metadata-standard (standard-id uint))
  (map-get? metadata-standards standard-id))

;; ---------- Chain Migration Functions ----------

;; Register a chain mapping for NFT migrations
(define-public (register-chain-mapping 
  (source-chain uint)
  (target-chain uint)
  (source-contract (string-ascii 64))
  (target-contract (string-ascii 64)))
  (begin
    ;; Only enterprise subscribers can register chain mappings
    (asserts! (is-eq (get-user-tier tx-sender) TIER_ENTERPRISE) ERR_UNAUTHORIZED)
    (asserts! (has-active-subscription tx-sender) ERR_UNAUTHORIZED)
    
    ;; Validate chains
    (asserts! (is-chain-active source-chain) ERR_INVALID_CHAIN)
    (asserts! (is-chain-active target-chain) ERR_INVALID_CHAIN)
    
    ;; Register the mapping
    (ok (map-set chain-mappings 
      { 
        source-chain: source-chain, 
        target-chain: target-chain, 
        source-contract: source-contract 
      } 
      {
        target-contract: target-contract,
        migration-status: "pending",
        migrated-count: u0
      }))))

;; Record a successful NFT migration
(define-public (record-nft-migration
  (source-chain uint)
  (target-chain uint)
  (source-contract (string-ascii 64))
  (source-token-id (string-ascii 64))
  (target-token-id (string-ascii 64)))
  (begin
    ;; Only authorized oracles can record migrations
    (asserts! (is-authorized-oracle tx-sender) ERR_UNAUTHORIZED)
    
    ;; Check if mapping exists
    (let ((mapping (unwrap! (map-get? chain-mappings 
                            { 
                              source-chain: source-chain, 
                              target-chain: target-chain, 
                              source-contract: source-contract 
                            }) 
                          ERR_NOT_FOUND))
          (source-nft-id { 
            chain-id: source-chain, 
            contract-address: source-contract, 
            token-id: source-token-id 
          })
          (target-nft-id { 
            chain-id: target-chain, 
            contract-address: (get target-contract mapping), 
            token-id: target-token-id 
          }))
      
      ;; Check if source NFT exists
      (asserts! (is-some (map-get? nft-registry source-nft-id)) ERR_NOT_FOUND)
      (let ((source-data (unwrap! (map-get? nft-registry source-nft-id) ERR_NOT_FOUND)))
        ;; Register target NFT with source metadata
        (map-set nft-registry target-nft-id {
          metadata-uri: (get metadata-uri source-data),
          metadata-hash: (get metadata-hash source-data),
          registered-by: tx-sender,
          verified: true,
          verified-by: (some tx-sender),
          last-updated: block-height,
          version: u1
        })
        
        ;; Update migration count
        (ok (map-set chain-mappings 
          { 
            source-chain: source-chain, 
            target-chain: target-chain, 
            source-contract: source-contract 
          } 
          (merge mapping {
            migration-status: "active",
            migrated-count: (+ u1 (get migrated-count mapping))
          })))))))

;; ---------- Initialize API Rate Limits ----------

;; Initialize rate limits for each tier
(map-set tier-rate-limits TIER_FREE {
  daily-limit: u100,
  monthly-limit: u1000,
  concurrent-requests: u2
})

(map-set tier-rate-limits TIER_BASIC {
  daily-limit: u1000,
  monthly-limit: u10000,
  concurrent-requests: u5
})

(map-set tier-rate-limits TIER_PREMIUM {
  daily-limit: u10000,
  monthly-limit: u100000,
  concurrent-requests: u20
})

(map-set tier-rate-limits TIER_ENTERPRISE {
  daily-limit: u100000,
  monthly-limit: u1000000,
  concurrent-requests: u50
})