;; RailShine - Cross-game achievement verification and reward distribution protocol
;; Clarity Version 2, Epoch 2.1

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-EXISTS (err u101))
(define-constant ERR-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-INVALID-PARAMS (err u104))
(define-constant ERR-ALREADY-CLAIMED (err u105))
(define-constant ERR-TIMELOCK-ACTIVE (err u106))

;; Governance timelock in blocks (approx 24h at 10min/block)
(define-constant TIMELOCK-BLOCKS u144)

;; Max reputation score
(define-constant MAX-REPUTATION u10000)

;; ============================================================
;; FUNGIBLE TOKENS
;; ============================================================

;; RAIL: governance and staking token
(define-fungible-token rail-token)

;; SHINE: achievement-based reward token
(define-fungible-token shine-token)

;; ============================================================
;; DATA MAPS AND VARS
;; ============================================================

;; Total staked RAIL per user
(define-map staked-rail
  { staker: principal }
  { amount: uint, since-block: uint }
)

;; Registered game integrations
(define-map games
  { game-id: uint }
  {
    name: (string-ascii 64),
    oracle: principal,
    active: bool
  }
)

;; Achievement definitions
(define-map achievements
  { game-id: uint, achievement-id: uint }
  {
    name: (string-ascii 64),
    difficulty: uint,    ;; 1 (easy) to 10 (legendary)
    shine-reward: uint,
    active: bool
  }
)

;; Tracks whether a user has claimed a specific achievement
(define-map claimed-achievements
  { user: principal, game-id: uint, achievement-id: uint }
  { claimed-at: uint }
)

;; Per-user reputation score
(define-map reputation-scores
  { user: principal }
  { score: uint }
)

;; Governance proposals
(define-map proposals
  { proposal-id: uint }
  {
    proposer: principal,
    description: (string-ascii 256),
    execute-at: uint,       ;; block height after timelock
    executed: bool,
    votes-for: uint,
    votes-against: uint
  }
)

;; Track votes per user per proposal
(define-map votes
  { voter: principal, proposal-id: uint }
  { support: bool }
)

;; Auto-increment counters
(define-data-var next-game-id uint u1)
(define-data-var next-achievement-id uint u1)
(define-data-var next-proposal-id uint u1)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

(define-private (is-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (is-oracle (game-id uint))
  (match (map-get? games { game-id: game-id })
    game (is-eq tx-sender (get oracle game))
    false
  )
)

(define-private (get-reputation (user principal))
  (default-to u0
    (get score (map-get? reputation-scores { user: user }))
  )
)

(define-private (add-reputation (user principal) (points uint))
  (let ((current (get-reputation user))
        (new-score (+ current points)))
    (map-set reputation-scores
      { user: user }
      { score: (if (> new-score MAX-REPUTATION) MAX-REPUTATION new-score) }
    )
  )
)

;; ============================================================
;; ADMIN: TOKEN MINTING
;; ============================================================

;; Mint RAIL tokens (owner only, for bootstrapping)
(define-public (mint-rail (recipient principal) (amount uint))
  (begin
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)
    (ft-mint? rail-token amount recipient)
  )
)

;; ============================================================
;; STAKING
;; ============================================================

;; Stake RAIL tokens for governance voting power
(define-public (stake-rail (amount uint))
  (begin
    (asserts! (> amount u0) ERR-INVALID-PARAMS)
    (try! (ft-transfer? rail-token amount tx-sender (as-contract tx-sender)))
    (map-set staked-rail
      { staker: tx-sender }
      {
        amount: (+ amount
          (default-to u0 (get amount (map-get? staked-rail { staker: tx-sender })))),
        since-block: block-height
      }
    )
    (ok true)
  )
)

;; Unstake RAIL tokens
(define-public (unstake-rail (amount uint))
  (let ((stake-info (unwrap! (map-get? staked-rail { staker: tx-sender }) ERR-NOT-FOUND))
        (staked (get amount stake-info)))
    (asserts! (>= staked amount) ERR-INSUFFICIENT-BALANCE)
    (try! (as-contract (ft-transfer? rail-token amount tx-sender tx-sender)))
    (map-set staked-rail
      { staker: tx-sender }
      { amount: (- staked amount), since-block: (get since-block stake-info) }
    )
    (ok true)
  )
)

;; ============================================================
;; GAME REGISTRY
;; ============================================================

;; Register a new game integration (owner only)
(define-public (register-game (name (string-ascii 64)) (oracle principal))
  (let ((game-id (var-get next-game-id)))
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)
    (map-set games
      { game-id: game-id }
      { name: name, oracle: oracle, active: true }
    )
    (var-set next-game-id (+ game-id u1))
    (ok game-id)
  )
)

;; Deactivate a game integration
(define-public (deactivate-game (game-id uint))
  (let ((game (unwrap! (map-get? games { game-id: game-id }) ERR-NOT-FOUND)))
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)
    (map-set games
      { game-id: game-id }
      (merge game { active: false })
    )
    (ok true)
  )
)

;; ============================================================
;; ACHIEVEMENT REGISTRY
;; ============================================================

;; Define an achievement for a game (oracle only)
(define-public (define-achievement
    (game-id uint)
    (name (string-ascii 64))
    (difficulty uint)
    (shine-reward uint))
  (let ((achievement-id (var-get next-achievement-id)))
    (asserts! (is-oracle game-id) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= difficulty u1) (<= difficulty u10)) ERR-INVALID-PARAMS)
    (asserts! (> shine-reward u0) ERR-INVALID-PARAMS)
    (map-set achievements
      { game-id: game-id, achievement-id: achievement-id }
      { name: name, difficulty: difficulty, shine-reward: shine-reward, active: true }
    )
    (var-set next-achievement-id (+ achievement-id u1))
    (ok achievement-id)
  )
)

;; ============================================================
;; ACHIEVEMENT VERIFICATION AND REWARD
;; ============================================================

;; Called by an authorized oracle to verify and reward an achievement
(define-public (verify-achievement
    (user principal)
    (game-id uint)
    (achievement-id uint))
  (let (
    (game (unwrap! (map-get? games { game-id: game-id }) ERR-NOT-FOUND))
    (ach  (unwrap! (map-get? achievements { game-id: game-id, achievement-id: achievement-id }) ERR-NOT-FOUND))
  )
    (asserts! (is-oracle game-id) ERR-NOT-AUTHORIZED)
    (asserts! (get active game) ERR-INVALID-PARAMS)
    (asserts! (get active ach) ERR-INVALID-PARAMS)
    (asserts!
      (is-none (map-get? claimed-achievements { user: user, game-id: game-id, achievement-id: achievement-id }))
      ERR-ALREADY-CLAIMED
    )
    ;; Mark as claimed
    (map-set claimed-achievements
      { user: user, game-id: game-id, achievement-id: achievement-id }
      { claimed-at: block-height }
    )
    ;; Mint SHINE reward
    (try! (ft-mint? shine-token (get shine-reward ach) user))
    ;; Add reputation: base points = difficulty * 100
    (add-reputation user (* (get difficulty ach) u100))
    (ok true)
  )
)

;; ============================================================
;; GOVERNANCE
;; ============================================================

;; Create a governance proposal (requires staked RAIL)
(define-public (create-proposal (description (string-ascii 256)))
  (let (
    (proposal-id (var-get next-proposal-id))
    (stake-info (unwrap! (map-get? staked-rail { staker: tx-sender }) ERR-INSUFFICIENT-BALANCE))
  )
    (asserts! (> (get amount stake-info) u0) ERR-INSUFFICIENT-BALANCE)
    (map-set proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        description: description,
        execute-at: (+ block-height TIMELOCK-BLOCKS),
        executed: false,
        votes-for: u0,
        votes-against: u0
      }
    )
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

;; Vote on a proposal using staked RAIL as voting weight
(define-public (vote (proposal-id uint) (support bool))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NOT-FOUND))
    (stake-info (unwrap! (map-get? staked-rail { staker: tx-sender }) ERR-INSUFFICIENT-BALANCE))
    (voting-power (get amount stake-info))
  )
    (asserts! (> voting-power u0) ERR-INSUFFICIENT-BALANCE)
    (asserts! (not (get executed proposal)) ERR-INVALID-PARAMS)
    (asserts! (is-none (map-get? votes { voter: tx-sender, proposal-id: proposal-id })) ERR-ALREADY-EXISTS)
    (map-set votes { voter: tx-sender, proposal-id: proposal-id } { support: support })
    (if support
      (map-set proposals { proposal-id: proposal-id }
        (merge proposal { votes-for: (+ (get votes-for proposal) voting-power) }))
      (map-set proposals { proposal-id: proposal-id }
        (merge proposal { votes-against: (+ (get votes-against proposal) voting-power) }))
    )
    (ok true)
  )
)

;; Mark proposal as executed (owner executes after timelock passes)
(define-public (execute-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NOT-FOUND)))
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)
    (asserts! (not (get executed proposal)) ERR-INVALID-PARAMS)
    (asserts! (>= block-height (get execute-at proposal)) ERR-TIMELOCK-ACTIVE)
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR-INVALID-PARAMS)
    (map-set proposals { proposal-id: proposal-id }
      (merge proposal { executed: true })
    )
    (ok true)
  )
)

;; ============================================================
;; READ-ONLY FUNCTIONS
;; ============================================================

(define-read-only (get-rail-balance (user principal))
  (ft-get-balance rail-token user)
)

(define-read-only (get-shine-balance (user principal))
  (ft-get-balance shine-token user)
)

(define-read-only (get-staked-amount (user principal))
  (default-to u0 (get amount (map-get? staked-rail { staker: user })))
)

(define-read-only (get-user-reputation (user principal))
  (get-reputation user)
)

(define-read-only (get-game (game-id uint))
  (map-get? games { game-id: game-id })
)

(define-read-only (get-achievement (game-id uint) (achievement-id uint))
  (map-get? achievements { game-id: game-id, achievement-id: achievement-id })
)

(define-read-only (is-achievement-claimed (user principal) (game-id uint) (achievement-id uint))
  (is-some (map-get? claimed-achievements { user: user, game-id: game-id, achievement-id: achievement-id }))
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (voter principal) (proposal-id uint))
  (map-get? votes { voter: voter, proposal-id: proposal-id })
)
