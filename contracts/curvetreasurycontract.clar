;; title: CurveTreasury
;; version: 1.0.0
;; summary: Programmatic DAO treasury with bonding-curve token issuance
;; description: A bonding curve implementation for sustainable DAO treasury management
;;              with continuous token mint/burn, governance controls, and circuit breakers

;; traits
(define-trait governance-trait
  (
    (is-dao-or-extension () (response bool uint))
  )
)

;; token definitions
(define-fungible-token curve-token)

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_PAUSED (err u103))
(define-constant ERR_INVALID_PARAMS (err u104))
(define-constant ERR_SLIPPAGE_EXCEEDED (err u105))
(define-constant ERR_CIRCUIT_BREAKER (err u106))
(define-constant ERR_INSUFFICIENT_RESERVE (err u107))

(define-constant PRECISION u1000000) ;; 6 decimal precision
(define-constant MAX_SUPPLY u1000000000000) ;; 1M tokens max
(define-constant MIN_RESERVE u1000000) ;; 1 STX minimum reserve
(define-constant FEE_BASIS_POINTS u300) ;; 3% total fee
(define-constant TREASURY_FEE_SHARE u200) ;; 2% to treasury
(define-constant BUYBACK_FEE_SHARE u100) ;; 1% for buyback

;; data vars
(define-data-var contract-paused bool false)
(define-data-var dao-address (optional principal) none)
(define-data-var treasury-address principal CONTRACT_OWNER)

;; Bonding curve parameters (upgradable by governance)
(define-data-var curve-slope uint u1000) ;; Linear curve slope
(define-data-var curve-power uint u2) ;; Power for exponential curves
(define-data-var reserve-ratio uint u500000) ;; 50% reserve ratio

;; Circuit breaker parameters
(define-data-var max-buy-amount uint u10000000000) ;; Max 10K STX per buy
(define-data-var max-sell-amount uint u10000000000) ;; Max 10K tokens per sell
(define-data-var daily-volume-limit uint u100000000000) ;; 100K STX daily limit
(define-data-var last-reset-block uint u0)
(define-data-var daily-volume uint u0)
;; Internal monotonically increasing counter used for transaction indexing
(define-data-var tx-counter uint u0)

;; Treasury metrics
(define-data-var total-fees-collected uint u0)
(define-data-var buyback-pool uint u0)

;; data maps
(define-map user-balances principal uint)
(define-map authorized-operators principal bool)
(define-map transaction-history 
  { user: principal, block: uint }
  { amount: uint, action: (string-ascii 10), price: uint }
)

;; public functions

;; Initialize the contract with DAO address
(define-public (initialize (dao principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set dao-address (some dao))
    (ok true)
  )
)

;; Buy tokens with STX (mint new tokens via bonding curve)
(define-public (buy-tokens (stx-amount uint) (min-tokens uint))
  (let (
    (current-supply (ft-get-supply curve-token))
    (reserve-balance (stx-get-balance (as-contract tx-sender)))
    (tokens-to-mint (calculate-tokens-for-stx stx-amount current-supply))
    (fee-amount (/ (* stx-amount FEE_BASIS_POINTS) u10000))
    (net-stx-amount (- stx-amount fee-amount))
    (treasury-fee (/ (* fee-amount TREASURY_FEE_SHARE) FEE_BASIS_POINTS))
    (buyback-fee (/ (* fee-amount BUYBACK_FEE_SHARE) FEE_BASIS_POINTS))
  )
    ;; Validations
    (asserts! (not (var-get contract-paused)) ERR_PAUSED)
    (asserts! (> stx-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= stx-amount (var-get max-buy-amount)) ERR_CIRCUIT_BREAKER)
    (asserts! (>= tokens-to-mint min-tokens) ERR_SLIPPAGE_EXCEEDED)
    (asserts! (<= (+ current-supply tokens-to-mint) MAX_SUPPLY) ERR_INVALID_AMOUNT)
    
    ;; Check daily volume limit
    (try! (check-daily-volume stx-amount))
    
    ;; Transfer STX from user
    (try! (stx-transfer? stx-amount tx-sender (as-contract tx-sender)))
    
    ;; Handle fees
    (try! (stx-transfer? treasury-fee (as-contract tx-sender) (var-get treasury-address)))
    (var-set buyback-pool (+ (var-get buyback-pool) buyback-fee))
    (var-set total-fees-collected (+ (var-get total-fees-collected) fee-amount))
    
    ;; Mint tokens to user
    (try! (ft-mint? curve-token tokens-to-mint tx-sender))
    
    ;; Record transaction
    (var-set tx-counter (+ (var-get tx-counter) u1))
    (map-set transaction-history
      { user: tx-sender, block: (var-get tx-counter) }
      { amount: tokens-to-mint, action: "buy", price: (/ (* stx-amount PRECISION) tokens-to-mint) }
    )
    
    (ok tokens-to-mint)
  )
)

;; Sell tokens for STX (burn tokens via bonding curve)
(define-public (sell-tokens (token-amount uint) (min-stx uint))
  (let (
    (current-supply (ft-get-supply curve-token))
    (reserve-balance (stx-get-balance (as-contract tx-sender)))
    (stx-to-return (calculate-stx-for-tokens token-amount current-supply))
    (fee-amount (/ (* stx-to-return FEE_BASIS_POINTS) u10000))
    (net-stx-amount (- stx-to-return fee-amount))
    (treasury-fee (/ (* fee-amount TREASURY_FEE_SHARE) FEE_BASIS_POINTS))
    (buyback-fee (/ (* fee-amount BUYBACK_FEE_SHARE) FEE_BASIS_POINTS))
  )
    ;; Validations
    (asserts! (not (var-get contract-paused)) ERR_PAUSED)
    (asserts! (> token-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= token-amount (var-get max-sell-amount)) ERR_CIRCUIT_BREAKER)
    (asserts! (>= net-stx-amount min-stx) ERR_SLIPPAGE_EXCEEDED)
    (asserts! (>= (ft-get-balance curve-token tx-sender) token-amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (>= reserve-balance stx-to-return) ERR_INSUFFICIENT_RESERVE)
    (asserts! (>= (- current-supply token-amount) u0) ERR_INVALID_AMOUNT)
    
    ;; Check daily volume limit
    (try! (check-daily-volume stx-to-return))
    
    ;; Burn tokens from user
    (try! (ft-burn? curve-token token-amount tx-sender))
    
    ;; Handle fees
    (try! (stx-transfer? treasury-fee (as-contract tx-sender) (var-get treasury-address)))
    (var-set buyback-pool (+ (var-get buyback-pool) buyback-fee))
    (var-set total-fees-collected (+ (var-get total-fees-collected) fee-amount))
    
    ;; Transfer STX to user
    (try! (as-contract (stx-transfer? net-stx-amount tx-sender tx-sender)))
    
    ;; Record transaction
    (var-set tx-counter (+ (var-get tx-counter) u1))
    (map-set transaction-history
      { user: tx-sender, block: (var-get tx-counter) }
      { amount: token-amount, action: "sell", price: (/ (* stx-to-return PRECISION) token-amount) }
    )
    
    (ok net-stx-amount)
  )
)

;; Governance functions
(define-public (set-curve-parameters (new-slope uint) (new-power uint) (new-ratio uint))
  (begin
    (try! (check-governance))
    (asserts! (and (> new-slope u0) (<= new-power u10) (and (> new-ratio u0) (<= new-ratio PRECISION))) ERR_INVALID_PARAMS)
    (var-set curve-slope new-slope)
    (var-set curve-power new-power)
    (var-set reserve-ratio new-ratio)
    (ok true)
  )
)

(define-public (set-circuit-breaker-limits (max-buy uint) (max-sell uint) (daily-limit uint))
  (begin
    (try! (check-governance))
    (var-set max-buy-amount max-buy)
    (var-set max-sell-amount max-sell)
    (var-set daily-volume-limit daily-limit)
    (ok true)
  )
)

(define-public (pause-contract)
  (begin
    (try! (check-governance))
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (try! (check-governance))
    (var-set contract-paused false)
    (ok true)
  )
)

(define-public (set-treasury-address (new-treasury principal))
  (begin
    (try! (check-governance))
    (var-set treasury-address new-treasury)
    (ok true)
  )
)

;; Execute buyback with accumulated fees
(define-public (execute-buyback)
  (let (
    (buyback-amount (var-get buyback-pool))
    (current-supply (ft-get-supply curve-token))
    (tokens-to-burn (calculate-tokens-for-stx buyback-amount current-supply))
  )
    (asserts! (> buyback-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (ft-get-supply curve-token) tokens-to-burn) ERR_INSUFFICIENT_BALANCE)
    
    ;; Use buyback pool to buy and burn tokens
    (try! (ft-mint? curve-token tokens-to-burn (as-contract tx-sender)))
    (try! (as-contract (ft-burn? curve-token tokens-to-burn tx-sender)))
    
    (var-set buyback-pool u0)
    (ok tokens-to-burn)
  )
)

;; Emergency withdrawal (governance only)
(define-public (emergency-withdraw (amount uint) (recipient principal))
  (begin
    (try! (check-governance))
    (asserts! (var-get contract-paused) ERR_UNAUTHORIZED)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (ok true)
  )
)
;; read only functions

(define-read-only (get-buy-price (stx-amount uint))
  (let (
    (current-supply (ft-get-supply curve-token))
  )
    (calculate-tokens-for-stx stx-amount current-supply)
  )
)

(define-read-only (get-sell-price (token-amount uint))
  (let (
    (current-supply (ft-get-supply curve-token))
  )
    (calculate-stx-for-tokens token-amount current-supply)
  )
)

(define-read-only (get-current-price)
  (let (
    (current-supply (ft-get-supply curve-token))
  )
    (if (is-eq current-supply u0)
      u0
      (/ (* (var-get curve-slope) current-supply) PRECISION)
    )
  )
)

(define-read-only (get-contract-stats)
  {
    supply: (ft-get-supply curve-token),
    reserve: (stx-get-balance (as-contract tx-sender)),
    paused: (var-get contract-paused),
    total-fees: (var-get total-fees-collected),
    buyback-pool: (var-get buyback-pool),
    current-price: (get-current-price)
  }
)

(define-read-only (get-curve-parameters)
  {
    slope: (var-get curve-slope),
    power: (var-get curve-power),
    reserve-ratio: (var-get reserve-ratio)
  }
)

(define-read-only (get-user-transaction (user principal) (block uint))
  (map-get? transaction-history { user: user, block: block })
)

(define-read-only (is-paused)
  (var-get contract-paused)
)
;; private functions

(define-private (calculate-tokens-for-stx (stx-amount uint) (current-supply uint))
  ;; Linear bonding curve: tokens = stx_amount / (slope * (supply + 1))
  ;; This creates increasing price as supply grows
  (let (
    (base-price (+ (var-get curve-slope) (/ (* (var-get curve-slope) current-supply) PRECISION)))
  )
    (/ (* stx-amount PRECISION) base-price)
  )
)

(define-private (calculate-stx-for-tokens (token-amount uint) (current-supply uint))
  ;; Reverse calculation for selling
  (let (
    (new-supply (- current-supply token-amount))
    (avg-price (+ (var-get curve-slope) (/ (* (var-get curve-slope) (+ current-supply new-supply)) (* u2 PRECISION))))
  )
    (/ (* token-amount avg-price) PRECISION)
  )
)

(define-private (check-governance)
  (let (
    (dao (var-get dao-address))
  )
    (if (is-some dao)
      (if (is-eq tx-sender (unwrap-panic dao))
        (ok true)
        ERR_UNAUTHORIZED)
      (if (is-eq tx-sender CONTRACT_OWNER)
        (ok true)
        ERR_UNAUTHORIZED)
    )
  )
)

(define-private (check-daily-volume (amount uint))
  (let (
    (current-count (var-get tx-counter))
    (last-reset (var-get last-reset-block))
    (current-volume (var-get daily-volume))
    (blocks-per-day u144) ;; Approximate window using tx count as proxy for blocks
  )
    (if (>= (- current-count last-reset) blocks-per-day)
      ;; Reset daily volume window
      (begin
        (var-set last-reset-block current-count)
        (var-set daily-volume amount)
        (ok true)
      )
      ;; Check if adding amount exceeds limit
      (if (<= (+ current-volume amount) (var-get daily-volume-limit))
        (begin
          (var-set daily-volume (+ current-volume amount))
          (ok true)
        )
        ERR_CIRCUIT_BREAKER
      )
    )
  )
)
