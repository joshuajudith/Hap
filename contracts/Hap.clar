
;; Users bet on whether the next Bitcoin block metric will be higher or lower.
;; Note: burn-block-difficulty is not exposed, so header-hash is used as a proxy.

(define-map bets 
  { bet-id: uint } 
  { user: principal, amount: uint, prediction: bool, placed-hash: (buff 32), settled: bool }
)

(define-data-var next-bet-id uint u0)

(define-public (place-bet (amount uint) (will-increase bool))
  (let (
    (current-hash (unwrap-panic (get-burn-block-info? header-hash u0)))
    (bet-id (var-get next-bet-id))
  )
    (asserts! (> amount u0) (err u400))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set bets
      { bet-id: bet-id }
      { user: tx-sender, amount: amount, prediction: will-increase, placed-hash: current-hash, settled: false }
    )
    (var-set next-bet-id (+ bet-id u1))
    (ok bet-id)
  )
)

(define-public (settle-bet (bet-id uint))
  (let (
    (bet (unwrap! (map-get? bets { bet-id: bet-id }) (err u404)))
    (current-hash (unwrap! (get-burn-block-info? header-hash u0) (err u500)))
    (did-increase (> current-hash (get placed-hash bet)))
    (did-win (if (get prediction bet) did-increase (< current-hash (get placed-hash bet))))
  )
    (asserts! (not (get settled bet)) (err u409))
    (if did-win
      (try! (as-contract (stx-transfer? (get amount bet) tx-sender (get user bet))))
      true
    )
    (map-set bets
      { bet-id: bet-id }
      {
        user: (get user bet),
        amount: (get amount bet),
        prediction: (get prediction bet),
        placed-hash: (get placed-hash bet),
        settled: true
      }
    )
    (ok did-win)
  )
)
