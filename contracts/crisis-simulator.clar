;; Financial Stress Testing Platform - Crisis Simulator Contract
;; This contract handles emergency response planning and crisis simulation

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u401))
(define-constant err-invalid-crisis (err u402))
(define-constant err-invalid-response (err u403))
(define-constant err-simulation-failed (err u404))
(define-constant err-recovery-failed (err u405))

;; Crisis severity levels
(define-constant severity-low u1)
(define-constant severity-moderate u2)
(define-constant severity-high u3)
(define-constant severity-severe u4)
(define-constant severity-extreme u5)

;; Helper functions
(define-private (min-uint (a uint) (b uint))
    (if (< a b) a b)
)

(define-private (max-uint (a uint) (b uint))
    (if (> a b) a b)
)

;; Data Variables
(define-data-var crisis-counter uint u0)
(define-data-var simulation-counter uint u0)
(define-data-var early-warning-threshold uint u7000) ;; 70% threshold for early warning
(define-data-var systemic-risk-threshold uint u8500) ;; 85% threshold for systemic risk

;; Data Maps
(define-map crisis-scenarios
    { crisis-id: uint }
    {
        name: (string-ascii 64),
        crisis-type: (string-ascii 32),
        description: (string-ascii 256),
        severity-level: uint,
        duration-months: uint,
        geographic-scope: (string-ascii 32),
        trigger-events: (string-ascii 256),
        created-by: principal,
        created-at: uint,
        status: (string-ascii 16)
    }
)

(define-map crisis-parameters
    { crisis-id: uint }
    {
        market-volatility-increase: uint,
        liquidity-stress-factor: uint,
        credit-spread-widening: uint,
        unemployment-increase: uint,
        gdp-contraction: uint,
        currency-devaluation: uint,
        real-estate-decline: uint,
        commodity-shock: uint,
        supply-chain-disruption: uint,
        financial-contagion-risk: uint
    }
)

(define-map emergency-responses
    { crisis-id: uint }
    {
        response-plan-id: uint,
        implementation-timeline: uint,
        resource-requirements: uint,
        communication-strategy: (string-ascii 128),
        coordination-level: uint,
        policy-effectiveness: uint,
        estimated-success-rate: uint
    }
)

(define-map market-stability-indicators
    { indicator-date: uint }
    {
        vix-level: uint,
        credit-spreads: uint,
        interbank-rates: uint,
        currency-volatility: uint,
        liquidity-measures: uint,
        system-stress-index: uint,
        alert-level: (string-ascii 16),
        trend-direction: (string-ascii 16)
    }
)

(define-map recovery-scenarios
    { crisis-id: uint, recovery-id: uint }
    {
        recovery-type: (string-ascii 32),
        recovery-timeline: uint,
        policy-effectiveness: uint,
        market-confidence-restoration: uint,
        financial-sector-health: uint,
        success-probability: uint,
        long-term-impacts: (string-ascii 256)
    }
)

(define-map liquidity-stress-tests
    { institution-id: uint, stress-date: uint }
    {
        cash-position: uint,
        available-facilities: uint,
        maturing-liabilities: uint,
        deposit-outflow-rate: uint,
        funding-gap: uint,
        survival-period: uint,
        liquidity-risk-rating: (string-ascii 16),
        stress-severity: uint
    }
)

;; Public Functions

;; Create crisis scenario
(define-public (create-crisis-scenario
    (name (string-ascii 64))
    (crisis-type (string-ascii 32))
    (description (string-ascii 256))
    (severity-level uint)
    (duration-months uint)
    (geographic-scope (string-ascii 32))
    (trigger-events (string-ascii 256)))
    
    (let 
        (
            (crisis-id (+ (var-get crisis-counter) u1))
            (current-time block-height)
        )
        (asserts! (> (len name) u0) (err u400))
        (asserts! (> (len crisis-type) u0) (err u400))
        (asserts! (and (>= severity-level severity-low) (<= severity-level severity-extreme)) (err u400))
        (asserts! (> duration-months u0) (err u400))
        
        (map-set crisis-scenarios
            { crisis-id: crisis-id }
            {
                name: name,
                crisis-type: crisis-type,
                description: description,
                severity-level: severity-level,
                duration-months: duration-months,
                geographic-scope: geographic-scope,
                trigger-events: trigger-events,
                created-by: tx-sender,
                created-at: current-time,
                status: "draft"
            }
        )
        
        (var-set crisis-counter crisis-id)
        (ok crisis-id)
    )
)

;; Configure crisis parameters
(define-public (configure-crisis-parameters
    (crisis-id uint)
    (market-volatility-increase uint)
    (liquidity-stress-factor uint)
    (credit-spread-widening uint)
    (unemployment-increase uint)
    (gdp-contraction uint)
    (currency-devaluation uint)
    (real-estate-decline uint)
    (commodity-shock uint)
    (supply-chain-disruption uint)
    (financial-contagion-risk uint))
    
    (let 
        (
            (crisis-data (unwrap! (map-get? crisis-scenarios { crisis-id: crisis-id }) err-invalid-crisis))
        )
        (asserts! (is-eq (get created-by crisis-data) tx-sender) err-unauthorized)
        (asserts! (<= market-volatility-increase u10000) (err u400)) ;; Max 100x increase
        (asserts! (<= liquidity-stress-factor u10000) (err u400))
        
        (map-set crisis-parameters
            { crisis-id: crisis-id }
            {
                market-volatility-increase: market-volatility-increase,
                liquidity-stress-factor: liquidity-stress-factor,
                credit-spread-widening: credit-spread-widening,
                unemployment-increase: unemployment-increase,
                gdp-contraction: gdp-contraction,
                currency-devaluation: currency-devaluation,
                real-estate-decline: real-estate-decline,
                commodity-shock: commodity-shock,
                supply-chain-disruption: supply-chain-disruption,
                financial-contagion-risk: financial-contagion-risk
            }
        )
        
        ;; Update crisis status to configured
        (map-set crisis-scenarios
            { crisis-id: crisis-id }
            (merge crisis-data { status: "configured" })
        )
        
        (ok true)
    )
)

;; Create emergency response plan
(define-public (create-emergency-response
    (crisis-id uint)
    (communication-strategy (string-ascii 128))
    (implementation-timeline uint)
    (resource-requirements uint)
    (coordination-level uint)
    (policy-effectiveness uint)
    (estimated-success-rate uint))
    
    (let 
        (
            (crisis-data (unwrap! (map-get? crisis-scenarios { crisis-id: crisis-id }) err-invalid-crisis))
            (response-plan-id u1)
        )
        (asserts! (is-eq (get created-by crisis-data) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status crisis-data) "configured") (err u406))
        (asserts! (> implementation-timeline u0) (err u400))
        (asserts! (<= estimated-success-rate u10000) (err u400)) ;; Max 100%
        
        (map-set emergency-responses
            { crisis-id: crisis-id }
            {
                response-plan-id: response-plan-id,
                implementation-timeline: implementation-timeline,
                resource-requirements: resource-requirements,
                communication-strategy: communication-strategy,
                coordination-level: coordination-level,
                policy-effectiveness: policy-effectiveness,
                estimated-success-rate: estimated-success-rate
            }
        )
        
        ;; Update crisis status to ready
        (map-set crisis-scenarios
            { crisis-id: crisis-id }
            (merge crisis-data { status: "ready" })
        )
        
        (ok response-plan-id)
    )
)

;; Run crisis simulation
(define-public (run-crisis-simulation (crisis-id uint))
    (let 
        (
            (crisis-data (unwrap! (map-get? crisis-scenarios { crisis-id: crisis-id }) err-invalid-crisis))
            (crisis-params (unwrap! (map-get? crisis-parameters { crisis-id: crisis-id }) (err u406)))
            (response-plan (unwrap! (map-get? emergency-responses { crisis-id: crisis-id }) (err u407)))
            (simulation-id (+ (var-get simulation-counter) u1))
        )
        (asserts! (is-eq (get created-by crisis-data) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status crisis-data) "ready") err-invalid-crisis)
        
        ;; Execute crisis simulation
        (let 
            (
                (simulation-results (execute-crisis-simulation crisis-params response-plan))
            )
            
            ;; Update crisis status to simulated
            (map-set crisis-scenarios
                { crisis-id: crisis-id }
                (merge crisis-data { status: "simulated" })
            )
            
            (var-set simulation-counter simulation-id)
            (ok simulation-results)
        )
    )
)

;; Execute crisis simulation (simplified)
(define-private (execute-crisis-simulation 
    (params (tuple (market-volatility-increase uint) (liquidity-stress-factor uint) (credit-spread-widening uint) (unemployment-increase uint) (gdp-contraction uint) (currency-devaluation uint) (real-estate-decline uint) (commodity-shock uint) (supply-chain-disruption uint) (financial-contagion-risk uint))) 
    (response (tuple (response-plan-id uint) (implementation-timeline uint) (resource-requirements uint) (communication-strategy (string-ascii 128)) (coordination-level uint) (policy-effectiveness uint) (estimated-success-rate uint))))
    
    (let 
        (
            ;; Calculate crisis impact
            (market-impact (get market-volatility-increase params))
            (liquidity-impact (get liquidity-stress-factor params))
            (credit-impact (get credit-spread-widening params))
            (total-crisis-severity (+ market-impact (+ liquidity-impact credit-impact)))
            
            ;; Calculate response effectiveness
            (response-speed (/ u10000 (get implementation-timeline response))) ;; Faster = more effective
            (resource-adequacy (min-uint u10000 (get resource-requirements response)))
            (coordination-effectiveness (get coordination-level response))
            
            ;; Simulate recovery
            (recovery-rate (calculate-recovery-rate total-crisis-severity response-speed))
            (market-stabilization-time (calculate-stabilization-time total-crisis-severity recovery-rate))
            (economic-recovery-time (* market-stabilization-time u2)) ;; Economic recovery takes longer
        )
        
        {
            crisis-severity: total-crisis-severity,
            response-effectiveness: (/ (+ response-speed resource-adequacy) u2),
            market-stabilization-time: market-stabilization-time,
            economic-recovery-time: economic-recovery-time,
            success-probability: recovery-rate,
            residual-risks: u2500 ;; 25% residual risk
        }
    )
)

;; Calculate recovery rate (simplified)
(define-private (calculate-recovery-rate (crisis-severity uint) (response-speed uint))
    (let 
        (
            (base-recovery u5000) ;; 50% base recovery rate
            (severity-penalty (/ crisis-severity u100))
            (speed-bonus (/ response-speed u100))
        )
        (let 
            (
                (adjusted-recovery (+ base-recovery (+ speed-bonus (- u0 severity-penalty))))
            )
            (max-uint u1000 (min-uint u9000 adjusted-recovery)) ;; Between 10% and 90%
        )
    )
)

;; Calculate market stabilization time (simplified)
(define-private (calculate-stabilization-time (crisis-severity uint) (recovery-rate uint))
    (let 
        (
            (base-time u12) ;; 12 months baseline
            (severity-factor (/ crisis-severity u1000))
            (recovery-factor (/ u10000 recovery-rate))
        )
        (min-uint u60 (+ base-time (/ (* severity-factor recovery-factor) u1000))) ;; Max 60 months
    )
)

;; Update market stability indicators
(define-public (update-market-indicators
    (vix-level uint)
    (credit-spreads uint)
    (interbank-rates uint)
    (currency-volatility uint)
    (liquidity-measures uint))
    
    (let 
        (
            (current-time block-height)
            (stress-index (calculate-system-stress-index vix-level credit-spreads interbank-rates currency-volatility liquidity-measures))
            (alert-level (determine-alert-level stress-index))
            (trend (determine-trend stress-index))
        )
        
        (map-set market-stability-indicators
            { indicator-date: current-time }
            {
                vix-level: vix-level,
                credit-spreads: credit-spreads,
                interbank-rates: interbank-rates,
                currency-volatility: currency-volatility,
                liquidity-measures: liquidity-measures,
                system-stress-index: stress-index,
                alert-level: alert-level,
                trend-direction: trend
            }
        )
        
        (ok stress-index)
    )
)

;; Calculate system stress index
(define-private (calculate-system-stress-index (vix uint) (spreads uint) (rates uint) (volatility uint) (liquidity uint))
    (let 
        (
            (vix-component (/ vix u4)) ;; 25% weight
            (spread-component (/ spreads u4)) ;; 25% weight
            (rate-component (/ rates u4)) ;; 25% weight
            (volatility-component (/ volatility u8)) ;; 12.5% weight
            (liquidity-component (/ liquidity u8)) ;; 12.5% weight
        )
        (+ vix-component (+ spread-component (+ rate-component (+ volatility-component liquidity-component))))
    )
)

;; Determine alert level
(define-private (determine-alert-level (stress-index uint))
    (if (>= stress-index (var-get systemic-risk-threshold))
        "red"
        (if (>= stress-index (var-get early-warning-threshold))
            "yellow"
            "green"
        )
    )
)

;; Determine trend direction
(define-private (determine-trend (stress-index uint))
    (if (> stress-index u5000)
        "deteriorating"
        "stable"
    )
)

;; Run liquidity stress test
(define-public (run-liquidity-stress-test
    (institution-id uint)
    (cash-position uint)
    (available-facilities uint)
    (maturing-liabilities uint)
    (deposit-outflow-rate uint))
    
    (let 
        (
            (current-time block-height)
            (funding-gap (calculate-funding-gap maturing-liabilities deposit-outflow-rate cash-position available-facilities))
            (survival-period (calculate-survival-period cash-position maturing-liabilities deposit-outflow-rate))
            (risk-rating (determine-liquidity-risk-rating funding-gap survival-period))
            (stress-severity (+ (/ funding-gap u1000000) (if (< survival-period u30) u3 u1)))
        )
        
        (map-set liquidity-stress-tests
            { institution-id: institution-id, stress-date: current-time }
            {
                cash-position: cash-position,
                available-facilities: available-facilities,
                maturing-liabilities: maturing-liabilities,
                deposit-outflow-rate: deposit-outflow-rate,
                funding-gap: funding-gap,
                survival-period: survival-period,
                liquidity-risk-rating: risk-rating,
                stress-severity: stress-severity
            }
        )
        
        (ok {
            funding-gap: funding-gap,
            survival-period: survival-period,
            risk-rating: risk-rating
        })
    )
)

;; Calculate funding gap
(define-private (calculate-funding-gap (maturing-liabilities uint) (outflow-rate uint) (cash uint) (facilities uint))
    (let 
        (
            (total-outflows (+ maturing-liabilities (/ (* cash outflow-rate) u10000)))
            (total-resources (+ cash facilities))
        )
        (if (> total-outflows total-resources) (- total-outflows total-resources) u0)
    )
)

;; Calculate survival period
(define-private (calculate-survival-period (cash uint) (maturing-liabilities uint) (outflow-rate uint))
    (let 
        (
            (daily-outflow (/ (* cash outflow-rate) u3650)) ;; Approximate daily outflow
        )
        (if (> daily-outflow u0) (/ cash daily-outflow) u365) ;; Days of survival
    )
)

;; Determine liquidity risk rating
(define-private (determine-liquidity-risk-rating (funding-gap uint) (survival-period uint))
    (if (> funding-gap u0)
        (if (< survival-period u30) "critical" "high")
        (if (< survival-period u90) "moderate" "low")
    )
)

;; Create recovery scenario
(define-public (create-recovery-scenario
    (crisis-id uint)
    (recovery-type (string-ascii 32))
    (recovery-timeline uint)
    (policy-effectiveness uint)
    (market-confidence-restoration uint)
    (financial-sector-health uint)
    (success-probability uint)
    (long-term-impacts (string-ascii 256)))
    
    (let 
        (
            (crisis-data (unwrap! (map-get? crisis-scenarios { crisis-id: crisis-id }) err-invalid-crisis))
            (recovery-id u1)
        )
        (asserts! (is-eq (get created-by crisis-data) tx-sender) err-unauthorized)
        (asserts! (> recovery-timeline u0) (err u400))
        (asserts! (<= success-probability u10000) (err u400)) ;; Max 100%
        
        (map-set recovery-scenarios
            { crisis-id: crisis-id, recovery-id: recovery-id }
            {
                recovery-type: recovery-type,
                recovery-timeline: recovery-timeline,
                policy-effectiveness: policy-effectiveness,
                market-confidence-restoration: market-confidence-restoration,
                financial-sector-health: financial-sector-health,
                success-probability: success-probability,
                long-term-impacts: long-term-impacts
            }
        )
        
        (ok recovery-id)
    )
)

;; Trigger early warning system
(define-public (trigger-early-warning (alert-message (string-ascii 128)) (urgency-level uint))
    (let 
        (
            (current-time block-height)
            (stress-index (var-get early-warning-threshold))
        )
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (asserts! (<= urgency-level u5) (err u400))
        
        ;; Update market indicators with warning
        (map-set market-stability-indicators
            { indicator-date: current-time }
            {
                vix-level: (* urgency-level u2000),
                credit-spreads: (* urgency-level u500),
                interbank-rates: (* urgency-level u300),
                currency-volatility: (* urgency-level u400),
                liquidity-measures: (* urgency-level u600),
                system-stress-index: stress-index,
                alert-level: "warning",
                trend-direction: "deteriorating"
            }
        )
        
        (ok true)
    )
)

;; Read-only Functions

;; Get crisis scenario
(define-read-only (get-crisis-scenario (crisis-id uint))
    (map-get? crisis-scenarios { crisis-id: crisis-id })
)

;; Get crisis parameters
(define-read-only (get-crisis-parameters (crisis-id uint))
    (map-get? crisis-parameters { crisis-id: crisis-id })
)

;; Get emergency response
(define-read-only (get-emergency-response (crisis-id uint))
    (map-get? emergency-responses { crisis-id: crisis-id })
)

;; Get market stability indicators
(define-read-only (get-market-indicators (indicator-date uint))
    (map-get? market-stability-indicators { indicator-date: indicator-date })
)

;; Get recovery scenario
(define-read-only (get-recovery-scenario (crisis-id uint) (recovery-id uint))
    (map-get? recovery-scenarios { crisis-id: crisis-id, recovery-id: recovery-id })
)

;; Get liquidity stress test results
(define-read-only (get-liquidity-stress-test (institution-id uint) (stress-date uint))
    (map-get? liquidity-stress-tests { institution-id: institution-id, stress-date: stress-date })
)

;; Get crisis count
(define-read-only (get-crisis-count)
    (var-get crisis-counter)
)

;; Get simulation count
(define-read-only (get-simulation-count)
    (var-get simulation-counter)
)

;; Check crisis ownership
(define-read-only (is-crisis-owner (crisis-id uint) (user principal))
    (match (map-get? crisis-scenarios { crisis-id: crisis-id })
        crisis-data (is-eq (get created-by crisis-data) user)
        false
    )
)

;; Administrative Functions

;; Update early warning threshold (admin only)
(define-public (update-early-warning-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (asserts! (<= new-threshold u10000) (err u400))
        (var-set early-warning-threshold new-threshold)
        (ok true)
    )
)

;; Update systemic risk threshold (admin only)
(define-public (update-systemic-risk-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (asserts! (<= new-threshold u10000) (err u400))
        (var-set systemic-risk-threshold new-threshold)
        (ok true)
    )
)
