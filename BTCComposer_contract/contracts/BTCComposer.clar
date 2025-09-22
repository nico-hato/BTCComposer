
;; title: BTCComposer
;; version: 1.0.0
;; summary: Cross-chain AMM liquidity pool for composable Bitcoin DeFi protocols
;; description: A decentralized AMM that enables cross-chain Bitcoin liquidity provisioning and swapping

;; traits
(define-trait sip-010-trait
  ((transfer (uint principal principal (optional (buff 34))) (response bool uint))
   (get-name () (response (string-ascii 32) uint))
   (get-symbol () (response (string-ascii 32) uint))
   (get-decimals () (response uint uint))
   (get-balance (principal) (response uint uint))
   (get-total-supply () (response uint uint))
   (get-token-uri () (response (optional (string-utf8 256)) uint))))

;; token definitions
(define-fungible-token btc-composer-lp)

;; constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INSUFFICIENT-LIQUIDITY (err u102))
(define-constant ERR-SLIPPAGE-TOO-HIGH (err u103))
(define-constant ERR-POOL-NOT-EXISTS (err u104))
(define-constant ERR-INVALID-TOKEN (err u105))
(define-constant ERR-PAUSED (err u106))
(define-constant ERR-DEADLINE-EXCEEDED (err u107))
(define-constant ERR-INSUFFICIENT-BALANCE (err u108))

(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-LIQUIDITY u1000)
(define-constant FEE-RATE u30) ;; 0.3% = 30/10000
(define-constant FEE-DENOMINATOR u10000)
(define-constant MAX-SLIPPAGE u1000) ;; 10% = 1000/10000

;; data vars
(define-data-var contract-paused bool false)
(define-data-var protocol-fee-recipient principal CONTRACT-OWNER)
(define-data-var total-pools uint u0)

;; data maps
(define-map pools
  { pool-id: uint }
  {
    token-a: principal,
    token-b: principal,
    reserve-a: uint,
    reserve-b: uint,
    lp-token-supply: uint,
    fee-rate: uint,
    last-update-block: uint
  }
)

(define-map user-liquidity
  { user: principal, pool-id: uint }
  { lp-tokens: uint }
)

(define-map pool-lookup
  { token-a: principal, token-b: principal }
  { pool-id: uint }
)

(define-map authorized-bridges
  { bridge-contract: principal }
  { authorized: bool, chain-id: uint }
)

;; public functions

;; Initialize a new liquidity pool
(define-public (create-pool (token-a <sip-010-trait>) (token-b <sip-010-trait>) (initial-a uint) (initial-b uint))
  (let (
    (pool-id (+ (var-get total-pools) u1))
    (token-a-contract (contract-of token-a))
    (token-b-contract (contract-of token-b))
    (sorted-tokens (if (< (buff-to-uint (as-max-len? (unwrap-panic (to-consensus-buff? token-a-contract)) u20))
                          (buff-to-uint (as-max-len? (unwrap-panic (to-consensus-buff? token-b-contract)) u20)))
                      { ta: token-a-contract, tb: token-b-contract, ra: initial-a, rb: initial-b }
                      { ta: token-b-contract, tb: token-a-contract, ra: initial-b, rb: initial-a }))
  )
    (asserts! (not (var-get contract-paused)) ERR-PAUSED)
    (asserts! (> initial-a u0) ERR-INVALID-AMOUNT)
    (asserts! (> initial-b u0) ERR-INVALID-AMOUNT)
    (asserts! (is-none (map-get? pool-lookup { token-a: (get ta sorted-tokens), token-b: (get tb sorted-tokens) })) ERR-POOL-NOT-EXISTS)

    ;; Transfer tokens from user to contract
    (try! (contract-call? token-a transfer initial-a tx-sender (as-contract tx-sender) none))
    (try! (contract-call? token-b transfer initial-b tx-sender (as-contract tx-sender) none))

    ;; Calculate initial LP tokens (geometric mean)
    (let ((initial-liquidity (sqrti (* (get ra sorted-tokens) (get rb sorted-tokens)))))
      (asserts! (>= initial-liquidity MIN-LIQUIDITY) ERR-INSUFFICIENT-LIQUIDITY)

      ;; Create pool
      (map-set pools
        { pool-id: pool-id }
        {
          token-a: (get ta sorted-tokens),
          token-b: (get tb sorted-tokens),
          reserve-a: (get ra sorted-tokens),
          reserve-b: (get rb sorted-tokens),
          lp-token-supply: initial-liquidity,
          fee-rate: FEE-RATE,
          last-update-block: block-height
        }
      )

      ;; Set lookup
      (map-set pool-lookup
        { token-a: (get ta sorted-tokens), token-b: (get tb sorted-tokens) }
        { pool-id: pool-id }
      )

      ;; Mint LP tokens to user
      (try! (ft-mint? btc-composer-lp initial-liquidity tx-sender))
      (map-set user-liquidity
        { user: tx-sender, pool-id: pool-id }
        { lp-tokens: initial-liquidity }
      )

      ;; Update total pools
      (var-set total-pools pool-id)

      (ok pool-id)
    )
  )
)

;; Add liquidity to existing pool
(define-public (add-liquidity (pool-id uint) (token-a <sip-010-trait>) (token-b <sip-010-trait>)
                             (amount-a-desired uint) (amount-b-desired uint)
                             (amount-a-min uint) (amount-b-min uint))
  (let (
    (pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-EXISTS))
    (reserve-a (get reserve-a pool-data))
    (reserve-b (get reserve-b pool-data))
    (total-supply (get lp-token-supply pool-data))
  )
    (asserts! (not (var-get contract-paused)) ERR-PAUSED)
    (asserts! (> amount-a-desired u0) ERR-INVALID-AMOUNT)
    (asserts! (> amount-b-desired u0) ERR-INVALID-AMOUNT)

    ;; Calculate optimal amounts
    (let (
      (amount-b-optimal (/ (* amount-a-desired reserve-b) reserve-a))
      (amount-a-final (if (<= amount-b-optimal amount-b-desired)
                        amount-a-desired
                        (/ (* amount-b-desired reserve-a) reserve-b)))
      (amount-b-final (if (<= amount-b-optimal amount-b-desired)
                        amount-b-optimal
                        amount-b-desired))
    )
      (asserts! (>= amount-a-final amount-a-min) ERR-SLIPPAGE-TOO-HIGH)
      (asserts! (>= amount-b-final amount-b-min) ERR-SLIPPAGE-TOO-HIGH)

      ;; Transfer tokens
      (try! (contract-call? token-a transfer amount-a-final tx-sender (as-contract tx-sender) none))
      (try! (contract-call? token-b transfer amount-b-final tx-sender (as-contract tx-sender) none))

      ;; Calculate LP tokens to mint
      (let ((liquidity (min (/ (* amount-a-final total-supply) reserve-a)
                           (/ (* amount-b-final total-supply) reserve-b))))
        ;; Update pool
        (map-set pools
          { pool-id: pool-id }
          (merge pool-data {
            reserve-a: (+ reserve-a amount-a-final),
            reserve-b: (+ reserve-b amount-b-final),
            lp-token-supply: (+ total-supply liquidity),
            last-update-block: block-height
          })
        )

        ;; Mint LP tokens
        (try! (ft-mint? btc-composer-lp liquidity tx-sender))

        ;; Update user liquidity
        (let ((current-lp (default-to u0 (get lp-tokens (map-get? user-liquidity { user: tx-sender, pool-id: pool-id })))))
          (map-set user-liquidity
            { user: tx-sender, pool-id: pool-id }
            { lp-tokens: (+ current-lp liquidity) }
          )
        )

        (ok { liquidity: liquidity, amount-a: amount-a-final, amount-b: amount-b-final })
      )
    )
  )
)

;; Remove liquidity from pool
(define-public (remove-liquidity (pool-id uint) (liquidity uint) (amount-a-min uint) (amount-b-min uint))
  (let (
    (pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-EXISTS))
    (user-lp-data (unwrap! (map-get? user-liquidity { user: tx-sender, pool-id: pool-id }) ERR-INSUFFICIENT-BALANCE))
    (user-lp-tokens (get lp-tokens user-lp-data))
    (total-supply (get lp-token-supply pool-data))
    (reserve-a (get reserve-a pool-data))
    (reserve-b (get reserve-b pool-data))
  )
    (asserts! (not (var-get contract-paused)) ERR-PAUSED)
    (asserts! (> liquidity u0) ERR-INVALID-AMOUNT)
    (asserts! (>= user-lp-tokens liquidity) ERR-INSUFFICIENT-BALANCE)

    ;; Calculate amounts to return
    (let (
      (amount-a (/ (* liquidity reserve-a) total-supply))
      (amount-b (/ (* liquidity reserve-b) total-supply))
    )
      (asserts! (>= amount-a amount-a-min) ERR-SLIPPAGE-TOO-HIGH)
      (asserts! (>= amount-b amount-b-min) ERR-SLIPPAGE-TOO-HIGH)

      ;; Burn LP tokens
      (try! (ft-burn? btc-composer-lp liquidity tx-sender))

      ;; Update pool
      (map-set pools
        { pool-id: pool-id }
        (merge pool-data {
          reserve-a: (- reserve-a amount-a),
          reserve-b: (- reserve-b amount-b),
          lp-token-supply: (- total-supply liquidity),
          last-update-block: block-height
        })
      )

      ;; Update user liquidity
      (map-set user-liquidity
        { user: tx-sender, pool-id: pool-id }
        { lp-tokens: (- user-lp-tokens liquidity) }
      )

      ;; Transfer tokens back to user (need to implement proper token transfer mechanism)
      ;; Note: In production, this would need proper token contract references
      (print { event: "liquidity-removed", amount-a: amount-a, amount-b: amount-b, user: tx-sender })

      (ok { amount-a: amount-a, amount-b: amount-b })
    )
  )
)

;; Swap tokens in AMM pool
(define-public (swap-exact-tokens-for-tokens (pool-id uint) (token-in <sip-010-trait>) (token-out <sip-010-trait>)
                                           (amount-in uint) (amount-out-min uint) (deadline uint))
  (let (
    (pool-data (unwrap! (map-get? pools { pool-id: pool-id }) ERR-POOL-NOT-EXISTS))
    (token-in-contract (contract-of token-in))
    (token-out-contract (contract-of token-out))
  )
    (asserts! (not (var-get contract-paused)) ERR-PAUSED)
    (asserts! (> amount-in u0) ERR-INVALID-AMOUNT)
    (asserts! (<= block-height deadline) ERR-DEADLINE-EXCEEDED)

    ;; Determine which token is which in the pool
    (let (
      (is-token-a-in (is-eq token-in-contract (get token-a pool-data)))
      (reserve-in (if is-token-a-in (get reserve-a pool-data) (get reserve-b pool-data)))
      (reserve-out (if is-token-a-in (get reserve-b pool-data) (get reserve-a pool-data)))
    )
      (asserts! (or (is-eq token-in-contract (get token-a pool-data)) (is-eq token-in-contract (get token-b pool-data))) ERR-INVALID-TOKEN)
      (asserts! (or (is-eq token-out-contract (get token-a pool-data)) (is-eq token-out-contract (get token-b pool-data))) ERR-INVALID-TOKEN)
      (asserts! (not (is-eq token-in-contract token-out-contract)) ERR-INVALID-TOKEN)

      ;; Calculate output amount
      (let ((amount-out (unwrap! (get-amount-out amount-in reserve-in reserve-out) ERR-INVALID-AMOUNT)))
        (asserts! (>= amount-out amount-out-min) ERR-SLIPPAGE-TOO-HIGH)
        (asserts! (< amount-out reserve-out) ERR-INSUFFICIENT-LIQUIDITY)

        ;; Transfer input tokens from user to contract
        (try! (contract-call? token-in transfer amount-in tx-sender (as-contract tx-sender) none))

        ;; Update pool reserves
        (map-set pools
          { pool-id: pool-id }
          (merge pool-data {
            reserve-a: (if is-token-a-in (+ (get reserve-a pool-data) amount-in) (- (get reserve-a pool-data) amount-out)),
            reserve-b: (if is-token-a-in (- (get reserve-b pool-data) amount-out) (+ (get reserve-b pool-data) amount-in)),
            last-update-block: block-height
          })
        )

        ;; Transfer output tokens from contract to user
        (try! (as-contract (contract-call? token-out transfer amount-out tx-sender tx-sender none)))

        (ok { amount-in: amount-in, amount-out: amount-out })
      )
    )
  )
)

;; Cross-chain bridge functions
(define-public (bridge-tokens-out (token <sip-010-trait>) (amount uint) (recipient-chain uint) (recipient-address (buff 64)))
  (let (
    (token-contract (contract-of token))
  )
    (asserts! (not (var-get contract-paused)) ERR-PAUSED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)

    ;; Transfer tokens to contract (lock them)
    (try! (contract-call? token transfer amount tx-sender (as-contract tx-sender) none))

    ;; Emit bridge event (in practice, this would trigger bridge infrastructure)
    (print {
      event: "bridge-out",
      token: token-contract,
      amount: amount,
      sender: tx-sender,
      recipient-chain: recipient-chain,
      recipient-address: recipient-address,
      block-height: block-height
    })

    (ok true)
  )
)

;; Bridge tokens in (only authorized bridge contracts can call this)
(define-public (bridge-tokens-in (token <sip-010-trait>) (amount uint) (recipient principal) (source-chain uint) (bridge-tx-hash (buff 32)))
  (let (
    (bridge-data (unwrap! (map-get? authorized-bridges { bridge-contract: tx-sender }) ERR-NOT-AUTHORIZED))
  )
    (asserts! (not (var-get contract-paused)) ERR-PAUSED)
    (asserts! (get authorized bridge-data) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)

    ;; Transfer tokens from contract to recipient (unlock them)
    (try! (as-contract (contract-call? token transfer amount tx-sender recipient none)))

    ;; Emit bridge event
    (print {
      event: "bridge-in",
      token: (contract-of token),
      amount: amount,
      recipient: recipient,
      source-chain: source-chain,
      bridge-tx-hash: bridge-tx-hash,
      block-height: block-height
    })

    (ok true)
  )
)

;; Admin functions (only contract owner)
(define-public (set-contract-paused (paused bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set contract-paused paused)
    (ok true)
  )
)

(define-public (set-protocol-fee-recipient (new-recipient principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set protocol-fee-recipient new-recipient)
    (ok true)
  )
)

(define-public (authorize-bridge (bridge-contract principal) (chain-id uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set authorized-bridges
      { bridge-contract: bridge-contract }
      { authorized: true, chain-id: chain-id }
    )
    (ok true)
  )
)

(define-public (deauthorize-bridge (bridge-contract principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-delete authorized-bridges { bridge-contract: bridge-contract })
    (ok true)
  )
)

(define-public (emergency-withdraw (token <sip-010-trait>) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (var-get contract-paused) ERR-NOT-AUTHORIZED)
    (try! (as-contract (contract-call? token transfer amount tx-sender CONTRACT-OWNER none)))
    (ok true)
  )
)

;; read only functions

;; Get pool information
(define-read-only (get-pool-info (pool-id uint))
  (map-get? pools { pool-id: pool-id })
)

;; Get user liquidity position
(define-read-only (get-user-liquidity (user principal) (pool-id uint))
  (map-get? user-liquidity { user: user, pool-id: pool-id })
)

;; Get pool by token pair
(define-read-only (get-pool-by-tokens (token-a principal) (token-b principal))
  (let (
    (sorted-tokens (if (< (buff-to-uint (as-max-len? (unwrap-panic (to-consensus-buff? token-a)) u20))
                          (buff-to-uint (as-max-len? (unwrap-panic (to-consensus-buff? token-b)) u20)))
                      { ta: token-a, tb: token-b }
                      { ta: token-b, tb: token-a }))
  )
    (map-get? pool-lookup { token-a: (get ta sorted-tokens), token-b: (get tb sorted-tokens) })
  )
)

;; Calculate swap output amount
(define-read-only (get-amount-out (amount-in uint) (reserve-in uint) (reserve-out uint))
  (if (and (> amount-in u0) (> reserve-in u0) (> reserve-out u0))
    (let (
      (amount-in-with-fee (* amount-in (- FEE-DENOMINATOR FEE-RATE)))
      (numerator (* amount-in-with-fee reserve-out))
      (denominator (+ (* reserve-in FEE-DENOMINATOR) amount-in-with-fee))
    )
      (ok (/ numerator denominator))
    )
    ERR-INVALID-AMOUNT
  )
)

;; Get contract status
(define-read-only (get-contract-info)
  {
    paused: (var-get contract-paused),
    total-pools: (var-get total-pools),
    protocol-fee-recipient: (var-get protocol-fee-recipient),
    fee-rate: FEE-RATE,
    min-liquidity: MIN-LIQUIDITY
  }
)

;; Check if bridge is authorized
(define-read-only (is-bridge-authorized (bridge-contract principal))
  (default-to false (get authorized (map-get? authorized-bridges { bridge-contract: bridge-contract })))
)

;; Get bridge info
(define-read-only (get-bridge-info (bridge-contract principal))
  (map-get? authorized-bridges { bridge-contract: bridge-contract })
)

;; Calculate input amount needed for desired output
(define-read-only (get-amount-in (amount-out uint) (reserve-in uint) (reserve-out uint))
  (if (and (> amount-out u0) (> reserve-in u0) (> reserve-out u0) (< amount-out reserve-out))
    (let (
      (numerator (* (* reserve-in amount-out) FEE-DENOMINATOR))
      (denominator (* (- reserve-out amount-out) (- FEE-DENOMINATOR FEE-RATE)))
    )
      (ok (+ (/ numerator denominator) u1))
    )
    ERR-INVALID-AMOUNT
  )
)

;; Get LP token information
(define-read-only (get-lp-token-info)
  {
    name: "BTCComposer LP",
    symbol: "BTC-LP",
    decimals: u6
  }
)

;; private functions

;; Square root calculation using Newton's method
(define-private (sqrti (x uint))
  (if (<= x u1)
    x
    (let ((initial-guess (/ x u2)))
      (sqrt-iter x initial-guess)
    )
  )
)

(define-private (sqrt-iter (x uint) (guess uint))
  (let ((new-guess (/ (+ guess (/ x guess)) u2)))
    (if (< (abs-diff guess new-guess) u1)
      new-guess
      (sqrt-iter x new-guess)
    )
  )
)

(define-private (abs-diff (a uint) (b uint))
  (if (>= a b) (- a b) (- b a))
)

(define-private (min (a uint) (b uint))
  (if (<= a b) a b)
)

(define-private (max (a uint) (b uint))
  (if (>= a b) a b)
)

(define-private (buff-to-uint (buffer (buff 20)))
  (fold + (map byte-to-uint (as-list buffer)) u0)
)

(define-private (byte-to-uint (byte (buff 1)))
  (unwrap-panic (index-of 0x000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff byte))
)
