;; Financial Stress Testing Platform - Risk Assessment Contract
;; This contract handles portfolio risk assessment, VaR calculations, and stress testing

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u401))
(define-constant err-invalid-portfolio (err u402))
(define-constant err-invalid-asset (err u403))
(define-constant err-insufficient-data (err u404))
(define-constant err-calculation-error (err u405))

;; Helper function for minimum
(define-private (min-uint (a uint) (b uint))
    (if (< a b) a b)
)

;; Helper function for maximum  
(define-private (max-uint (a uint) (b uint))
    (if (> a b) a b)
)

;; Data Variables
(define-data-var portfolio-counter uint u0)
(define-data-var risk-model-version uint u1)
(define-data-var var-confidence-level uint u95) ;; 95% confidence level for VaR
(define-data-var stress-test-active bool false)

;; Data Maps
(define-map portfolios 
    { portfolio-id: uint } 
    { 
        owner: principal,
        name: (string-ascii 64),
        total-value: uint,
        risk-score: uint,
        var-95: uint,
        var-99: uint,
        created-at: uint,
        last-updated: uint,
        status: (string-ascii 16),
        asset-count: uint
    }
)

(define-map portfolio-assets
    { portfolio-id: uint, asset-id: uint }
    {
        asset-type: (string-ascii 32),
        quantity: uint,
        market-value: uint,
        volatility: uint,
        correlation-coefficient: uint,
        weight: uint,
        beta: uint,
        duration: uint,
        risk-contribution: uint
    }
)

(define-map stress-scenarios
    { scenario-id: uint }
    {
        name: (string-ascii 64),
        market-shock: uint,
        credit-spread-widening: uint,
        interest-rate-shock: uint,
        currency-shock: uint,
        liquidity-shock: uint,
        created-by: principal,
        scenario-type: (string-ascii 32),
        severity-level: uint
    }
)

(define-map risk-metrics
    { portfolio-id: uint }
    {
        sharpe-ratio: uint,
        sortino-ratio: uint,
        max-drawdown: uint,
        tracking-error: uint,
        information-ratio: uint,
        treynor-ratio: uint,
        calmar-ratio: uint,
        conditional-var: uint,
        expected-return: uint,
        volatility: uint
    }
)

;; Public Functions

;; Create a new portfolio
(define-public (create-portfolio (name (string-ascii 64)))
    (let 
        (
            (portfolio-id (+ (var-get portfolio-counter) u1))
            (current-time block-height)
        )
        (asserts! (> (len name) u0) (err u400))
        
        (map-set portfolios
            { portfolio-id: portfolio-id }
            {
                owner: tx-sender,
                name: name,
                total-value: u0,
                risk-score: u0,
                var-95: u0,
                var-99: u0,
                created-at: current-time,
                last-updated: current-time,
                status: "active",
                asset-count: u0
            }
        )
        
        (var-set portfolio-counter portfolio-id)
        (ok portfolio-id)
    )
)

;; Add asset to portfolio
(define-public (add-asset-to-portfolio 
    (portfolio-id uint) 
    (asset-id uint)
    (asset-type (string-ascii 32))
    (quantity uint)
    (market-value uint)
    (volatility uint)
    (beta uint))
    
    (let 
        (
            (portfolio-data (unwrap! (map-get? portfolios { portfolio-id: portfolio-id }) err-invalid-portfolio))
            (current-time block-height)
        )
        (asserts! (is-eq (get owner portfolio-data) tx-sender) err-unauthorized)
        (asserts! (> quantity u0) err-invalid-asset)
        (asserts! (> market-value u0) err-invalid-asset)
        
        ;; Calculate weight as percentage of total portfolio value
        (let 
            (
                (total-value (get total-value portfolio-data))
                (new-total-value (+ total-value market-value))
                (weight (if (> new-total-value u0) 
                           (/ (* market-value u10000) new-total-value) 
                           u0))
                (risk-contribution (/ (* volatility weight) u10000))
                (new-asset-count (+ (get asset-count portfolio-data) u1))
            )
            
            (map-set portfolio-assets
                { portfolio-id: portfolio-id, asset-id: asset-id }
                {
                    asset-type: asset-type,
                    quantity: quantity,
                    market-value: market-value,
                    volatility: volatility,
                    correlation-coefficient: u5000, ;; Default 0.5
                    weight: weight,
                    beta: beta,
                    duration: u0,
                    risk-contribution: risk-contribution
                }
            )
            
            ;; Update portfolio total value and asset count
            (map-set portfolios
                { portfolio-id: portfolio-id }
                (merge portfolio-data { 
                    total-value: new-total-value,
                    last-updated: current-time,
                    asset-count: new-asset-count
                })
            )
            
            (ok true)
        )
    )
)

;; Calculate Value at Risk (VaR) for a portfolio
(define-public (calculate-var (portfolio-id uint) (confidence-level uint))
    (let 
        (
            (portfolio-data (unwrap! (map-get? portfolios { portfolio-id: portfolio-id }) err-invalid-portfolio))
            (total-value (get total-value portfolio-data))
        )
        (asserts! (is-eq (get owner portfolio-data) tx-sender) err-unauthorized)
        (asserts! (> total-value u0) err-insufficient-data)
        (asserts! (or (is-eq confidence-level u95) (is-eq confidence-level u99)) (err u400))
        
        ;; Simplified VaR calculation using portfolio volatility
        (let 
            (
                (portfolio-volatility (calculate-portfolio-volatility portfolio-id))
                (z-score (if (is-eq confidence-level u95) u165 u233)) ;; 1.65 for 95%, 2.33 for 99%
                (var-amount (/ (* total-value (* portfolio-volatility z-score)) u100000))
                (current-time block-height)
            )
            
            ;; Update portfolio with calculated VaR
            (if (is-eq confidence-level u95)
                (map-set portfolios
                    { portfolio-id: portfolio-id }
                    (merge portfolio-data { 
                        var-95: var-amount,
                        last-updated: current-time 
                    })
                )
                (map-set portfolios
                    { portfolio-id: portfolio-id }
                    (merge portfolio-data { 
                        var-99: var-amount,
                        last-updated: current-time 
                    })
                )
            )
            
            (ok var-amount)
        )
    )
)

;; Calculate portfolio volatility (weighted average of asset volatilities)
(define-private (calculate-portfolio-volatility (portfolio-id uint))
    ;; Simplified calculation - in practice would need correlation matrix
    (let 
        (
            (base-volatility u1500) ;; Default 15% annual volatility
            (portfolio-data (unwrap-panic (map-get? portfolios { portfolio-id: portfolio-id })))
            (asset-count (get asset-count portfolio-data))
        )
        ;; Adjust volatility based on diversification
        (if (> asset-count u1)
            (/ (* base-volatility u8000) u10000) ;; 20% reduction for diversification
            base-volatility
        )
    )
)

;; Run stress test on portfolio
(define-public (run-stress-test (portfolio-id uint) (scenario-id uint))
    (let 
        (
            (portfolio-data (unwrap! (map-get? portfolios { portfolio-id: portfolio-id }) err-invalid-portfolio))
            (scenario-data (unwrap! (map-get? stress-scenarios { scenario-id: scenario-id }) (err u406)))
            (total-value (get total-value portfolio-data))
        )
        (asserts! (is-eq (get owner portfolio-data) tx-sender) err-unauthorized)
        (asserts! (> total-value u0) err-insufficient-data)
        
        ;; Apply scenario shocks to calculate stressed value
        (let 
            (
                (market-shock (get market-shock scenario-data))
                (credit-shock (get credit-spread-widening scenario-data))
                (interest-shock (get interest-rate-shock scenario-data))
                
                ;; Calculate impact (simplified)
                (total-shock (+ market-shock (+ credit-shock interest-shock)))
                (stressed-value (if (>= total-value total-shock)
                                   (- total-value total-shock)
                                   u0))
                (loss-amount (- total-value stressed-value))
                (loss-percentage (/ (* loss-amount u10000) total-value))
            )
            
            ;; Update risk score based on stress test results
            (let 
                (
                    (new-risk-score (min-uint u1000 (+ (get risk-score portfolio-data) 
                                                      (/ loss-percentage u100))))
                    (current-time block-height)
                )
                (map-set portfolios
                    { portfolio-id: portfolio-id }
                    (merge portfolio-data { 
                        risk-score: new-risk-score,
                        last-updated: current-time 
                    })
                )
                
                (ok { 
                    stressed-value: stressed-value,
                    loss-amount: loss-amount,
                    loss-percentage: loss-percentage,
                    new-risk-score: new-risk-score
                })
            )
        )
    )
)

;; Create stress scenario
(define-public (create-stress-scenario 
    (name (string-ascii 64))
    (market-shock uint)
    (credit-spread-widening uint)
    (interest-rate-shock uint)
    (currency-shock uint)
    (liquidity-shock uint)
    (scenario-type (string-ascii 32))
    (severity-level uint))
    
    (let 
        (
            (scenario-id (+ (var-get portfolio-counter) u1)) ;; Reusing counter for simplicity
        )
        (asserts! (> (len name) u0) (err u400))
        (asserts! (<= severity-level u10) (err u400))
        
        (map-set stress-scenarios
            { scenario-id: scenario-id }
            {
                name: name,
                market-shock: market-shock,
                credit-spread-widening: credit-spread-widening,
                interest-rate-shock: interest-rate-shock,
                currency-shock: currency-shock,
                liquidity-shock: liquidity-shock,
                created-by: tx-sender,
                scenario-type: scenario-type,
                severity-level: severity-level
            }
        )
        
        (ok scenario-id)
    )
)

;; Calculate advanced risk metrics
(define-public (calculate-risk-metrics (portfolio-id uint))
    (let 
        (
            (portfolio-data (unwrap! (map-get? portfolios { portfolio-id: portfolio-id }) err-invalid-portfolio))
        )
        (asserts! (is-eq (get owner portfolio-data) tx-sender) err-unauthorized)
        
        ;; Calculate risk metrics based on portfolio characteristics
        (let 
            (
                (total-value (get total-value portfolio-data))
                (portfolio-vol (calculate-portfolio-volatility portfolio-id))
                (asset-count (get asset-count portfolio-data))
                
                ;; Risk metric calculations (simplified)
                (sharpe-ratio (if (> portfolio-vol u0) (/ u15000 portfolio-vol) u0))
                (sortino-ratio (+ sharpe-ratio u30))
                (max-drawdown (+ portfolio-vol u500))
                (tracking-error (/ portfolio-vol u3))
                (information-ratio (/ sharpe-ratio u2))
                (treynor-ratio (+ sharpe-ratio u20))
                (calmar-ratio (/ sharpe-ratio u3))
                (conditional-var (+ (get var-95 portfolio-data) 
                                   (/ (get var-95 portfolio-data) u4)))
                (expected-return (- u800 (/ portfolio-vol u2)))
            )
            
            (map-set risk-metrics
                { portfolio-id: portfolio-id }
                {
                    sharpe-ratio: sharpe-ratio,
                    sortino-ratio: sortino-ratio,
                    max-drawdown: max-drawdown,
                    tracking-error: tracking-error,
                    information-ratio: information-ratio,
                    treynor-ratio: treynor-ratio,
                    calmar-ratio: calmar-ratio,
                    conditional-var: conditional-var,
                    expected-return: expected-return,
                    volatility: portfolio-vol
                }
            )
            
            (ok true)
        )
    )
)

;; Read-only Functions

;; Get portfolio details
(define-read-only (get-portfolio (portfolio-id uint))
    (map-get? portfolios { portfolio-id: portfolio-id })
)

;; Get portfolio asset
(define-read-only (get-portfolio-asset (portfolio-id uint) (asset-id uint))
    (map-get? portfolio-assets { portfolio-id: portfolio-id, asset-id: asset-id })
)

;; Get stress scenario
(define-read-only (get-stress-scenario (scenario-id uint))
    (map-get? stress-scenarios { scenario-id: scenario-id })
)

;; Get risk metrics
(define-read-only (get-risk-metrics (portfolio-id uint))
    (map-get? risk-metrics { portfolio-id: portfolio-id })
)

;; Get portfolio count
(define-read-only (get-portfolio-count)
    (var-get portfolio-counter)
)

;; Check if portfolio owner
(define-read-only (is-portfolio-owner (portfolio-id uint) (user principal))
    (match (map-get? portfolios { portfolio-id: portfolio-id })
        portfolio-data (is-eq (get owner portfolio-data) user)
        false
    )
)

;; Administrative Functions

;; Update risk model version (admin only)
(define-public (update-risk-model-version (new-version uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (var-set risk-model-version new-version)
        (ok true)
    )
)

;; Set stress test status (admin only)
(define-public (set-stress-test-status (active bool))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (var-set stress-test-active active)
        (ok true)
    )
)
