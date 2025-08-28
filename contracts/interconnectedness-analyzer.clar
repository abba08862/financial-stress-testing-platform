;; Financial Stress Testing Platform - Interconnectedness Analyzer Contract
;; This contract handles network analysis and contagion modeling between financial institutions

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u401))
(define-constant err-invalid-institution (err u402))
(define-constant err-invalid-network (err u403))
(define-constant err-contagion-analysis-failed (err u404))
(define-constant err-insufficient-data (err u405))

;; Helper functions
(define-private (min-uint (a uint) (b uint))
    (if (< a b) a b)
)

;; Data Variables
(define-data-var institution-counter uint u0)
(define-data-var network-counter uint u0)
(define-data-var contagion-threshold uint u2500) ;; 25% default threshold
(define-data-var systemic-importance-threshold uint u1000) ;; 10% threshold

;; Data Maps
(define-map financial-institutions
    { institution-id: uint }
    {
        name: (string-ascii 64),
        institution-type: (string-ascii 32),
        jurisdiction: (string-ascii 32),
        total-assets: uint,
        tier1-capital: uint,
        leverage-ratio: uint,
        liquidity-coverage-ratio: uint,
        systemic-importance-score: uint,
        risk-category: (string-ascii 16),
        created-at: uint,
        last-updated: uint,
        connection-count: uint
    }
)

(define-map institution-connections
    { from-institution: uint, to-institution: uint }
    {
        connection-type: (string-ascii 32),
        exposure-amount: uint,
        relationship-strength: uint,
        risk-weight: uint,
        maturity: uint,
        collateral-posted: uint,
        netting-agreement: bool,
        created-at: uint
    }
)

(define-map network-topology
    { network-id: uint }
    {
        network-name: (string-ascii 64),
        institution-count: uint,
        total-connections: uint,
        network-density: uint,
        clustering-coefficient: uint,
        average-path-length: uint,
        centralization-index: uint,
        created-by: principal,
        created-at: uint
    }
)

(define-map contagion-simulations
    { simulation-id: uint }
    {
        network-id: uint,
        initial-failure-institution: uint,
        contagion-rounds: uint,
        total-failures: uint,
        total-losses: uint,
        contagion-percentage: uint,
        simulation-date: uint,
        methodology: (string-ascii 32)
    }
)

(define-map systemic-importance-measures
    { institution-id: uint }
    {
        size-indicator: uint,
        interconnectedness-indicator: uint,
        substitutability-indicator: uint,
        complexity-indicator: uint,
        cross-jurisdiction-indicator: uint,
        gsib-score: uint,
        dsib-score: uint,
        last-calculated: uint
    }
)

(define-map counterparty-exposures
    { institution-id: uint, counterparty-id: uint }
    {
        gross-exposure: uint,
        net-exposure: uint,
        exposure-type: (string-ascii 32),
        credit-equivalent-amount: uint,
        risk-weighted-exposure: uint,
        exposure-percentage: uint,
        concentration-limit: uint,
        breach-indicator: bool
    }
)

;; Public Functions

;; Register financial institution
(define-public (register-institution
    (name (string-ascii 64))
    (institution-type (string-ascii 32))
    (jurisdiction (string-ascii 32))
    (total-assets uint)
    (tier1-capital uint)
    (leverage-ratio uint)
    (liquidity-coverage-ratio uint))
    
    (let 
        (
            (institution-id (+ (var-get institution-counter) u1))
            (current-time block-height)
        )
        (asserts! (> (len name) u0) (err u400))
        (asserts! (> total-assets u0) (err u400))
        (asserts! (> tier1-capital u0) (err u400))
        
        ;; Calculate initial systemic importance score
        (let 
            (
                (size-score (/ total-assets u1000000)) ;; Simplified size calculation
                (initial-score (min-uint size-score u1000))
                (risk-cat (if (>= initial-score u500) "high-risk" "standard"))
            )
            
            (map-set financial-institutions
                { institution-id: institution-id }
                {
                    name: name,
                    institution-type: institution-type,
                    jurisdiction: jurisdiction,
                    total-assets: total-assets,
                    tier1-capital: tier1-capital,
                    leverage-ratio: leverage-ratio,
                    liquidity-coverage-ratio: liquidity-coverage-ratio,
                    systemic-importance-score: initial-score,
                    risk-category: risk-cat,
                    created-at: current-time,
                    last-updated: current-time,
                    connection-count: u0
                }
            )
            
            (var-set institution-counter institution-id)
            (ok institution-id)
        )
    )
)

;; Create connection between institutions
(define-public (create-connection
    (from-institution uint)
    (to-institution uint)
    (connection-type (string-ascii 32))
    (exposure-amount uint)
    (relationship-strength uint)
    (risk-weight uint)
    (maturity uint)
    (collateral-posted uint)
    (netting-agreement bool))
    
    (let 
        (
            (from-inst (unwrap! (map-get? financial-institutions { institution-id: from-institution }) err-invalid-institution))
            (to-inst (unwrap! (map-get? financial-institutions { institution-id: to-institution }) err-invalid-institution))
            (current-time block-height)
        )
        (asserts! (not (is-eq from-institution to-institution)) (err u400))
        (asserts! (> exposure-amount u0) (err u400))
        (asserts! (<= relationship-strength u1000) (err u400)) ;; Max 100%
        
        (map-set institution-connections
            { from-institution: from-institution, to-institution: to-institution }
            {
                connection-type: connection-type,
                exposure-amount: exposure-amount,
                relationship-strength: relationship-strength,
                risk-weight: risk-weight,
                maturity: maturity,
                collateral-posted: collateral-posted,
                netting-agreement: netting-agreement,
                created-at: current-time
            }
        )
        
        ;; Update connection counts
        (map-set financial-institutions
            { institution-id: from-institution }
            (merge from-inst { connection-count: (+ (get connection-count from-inst) u1) })
        )
        
        ;; Update counterparty exposures
        (unwrap! (update-counterparty-exposure from-institution to-institution exposure-amount connection-type) (err u500))
        
        (ok true)
    )
)

;; Update counterparty exposure
(define-private (update-counterparty-exposure (institution-id uint) (counterparty-id uint) (exposure-amount uint) (exposure-type (string-ascii 32)))
    (let 
        (
            (institution-data (unwrap! (map-get? financial-institutions { institution-id: institution-id }) err-invalid-institution))
            (total-assets (get total-assets institution-data))
            (exposure-percentage (/ (* exposure-amount u10000) total-assets))
            (concentration-limit u2500) ;; 25% concentration limit
            (breach (> exposure-percentage concentration-limit))
            
            ;; Calculate risk-weighted exposure (simplified)
            (risk-weighted-exp (/ (* exposure-amount u8000) u10000)) ;; 80% risk weight
        )
        
        (map-set counterparty-exposures
            { institution-id: institution-id, counterparty-id: counterparty-id }
            {
                gross-exposure: exposure-amount,
                net-exposure: (if (> exposure-amount u0) (- exposure-amount u0) u0),
                exposure-type: exposure-type,
                credit-equivalent-amount: exposure-amount,
                risk-weighted-exposure: risk-weighted-exp,
                exposure-percentage: exposure-percentage,
                concentration-limit: concentration-limit,
                breach-indicator: breach
            }
        )
        
        (ok true)
    )
)

;; Calculate systemic importance indicators
(define-public (calculate-systemic-importance (institution-id uint))
    (let 
        (
            (institution-data (unwrap! (map-get? financial-institutions { institution-id: institution-id }) err-invalid-institution))
            (current-time block-height)
        )
        ;; Calculate G-SIB/D-SIB indicators (simplified)
        (let 
            (
                (total-assets (get total-assets institution-data))
                (size-ind (/ total-assets u10000000)) ;; Size indicator
                (interconnect-ind (calculate-interconnectedness-indicator institution-id))
                (substitutability-ind u400) ;; Simplified
                (complexity-ind u300) ;; Simplified
                (cross-juris-ind u200) ;; Simplified
                
                (gsib-score (+ size-ind (+ interconnect-ind (+ substitutability-ind (+ complexity-ind cross-juris-ind)))))
                (dsib-score (/ (* gsib-score u75) u100)) ;; 75% of G-SIB score
            )
            
            (map-set systemic-importance-measures
                { institution-id: institution-id }
                {
                    size-indicator: size-ind,
                    interconnectedness-indicator: interconnect-ind,
                    substitutability-indicator: substitutability-ind,
                    complexity-indicator: complexity-ind,
                    cross-jurisdiction-indicator: cross-juris-ind,
                    gsib-score: gsib-score,
                    dsib-score: dsib-score,
                    last-calculated: current-time
                }
            )
            
            ;; Update institution's systemic importance score
            (map-set financial-institutions
                { institution-id: institution-id }
                (merge institution-data { 
                    systemic-importance-score: gsib-score,
                    last-updated: current-time 
                })
            )
            
            (ok gsib-score)
        )
    )
)

;; Calculate interconnectedness indicator
(define-private (calculate-interconnectedness-indicator (institution-id uint))
    ;; Simplified calculation based on connections
    (let 
        (
            (institution-data (unwrap-panic (map-get? financial-institutions { institution-id: institution-id })))
            (connection-count (get connection-count institution-data))
            (exposure-sum u100000000) ;; Would sum actual exposures
        )
        (+ connection-count (/ exposure-sum u1000000))
    )
)

;; Run contagion simulation
(define-public (run-contagion-simulation (network-id uint) (initial-failure-institution uint))
    (let 
        (
            (network-data (unwrap! (map-get? network-topology { network-id: network-id }) err-invalid-network))
            (simulation-id u1) ;; Simplified counter
            (current-time block-height)
        )
        (asserts! (is-eq (get created-by network-data) tx-sender) err-unauthorized)
        
        ;; Run simplified contagion analysis
        (let 
            (
                (contagion-results (execute-contagion-analysis network-id initial-failure-institution))
                (total-institutions (get institution-count network-data))
            )
            
            (map-set contagion-simulations
                { simulation-id: simulation-id }
                {
                    network-id: network-id,
                    initial-failure-institution: initial-failure-institution,
                    contagion-rounds: (get rounds contagion-results),
                    total-failures: (get failures contagion-results),
                    total-losses: (get losses contagion-results),
                    contagion-percentage: (/ (* (get failures contagion-results) u10000) total-institutions),
                    simulation-date: current-time,
                    methodology: "simplified-model"
                }
            )
            
            (ok simulation-id)
        )
    )
)

;; Execute contagion analysis (simplified)
(define-private (execute-contagion-analysis (network-id uint) (initial-failure uint))
    (let 
        (
            ;; Simplified contagion modeling
            (threshold (var-get contagion-threshold))
            (round1-failures u2) ;; Direct connections fail
            (round2-failures u1) ;; Secondary contagion
            (total-rounds u3)
            (total-failures (+ round1-failures round2-failures u1)) ;; +1 for initial
            (estimated-losses u50000000) ;; $500M estimated losses
        )
        {
            rounds: total-rounds,
            failures: total-failures,
            losses: estimated-losses
        }
    )
)

;; Read-only Functions

;; Get institution details
(define-read-only (get-institution (institution-id uint))
    (map-get? financial-institutions { institution-id: institution-id })
)

;; Get connection details
(define-read-only (get-connection (from-institution uint) (to-institution uint))
    (map-get? institution-connections { from-institution: from-institution, to-institution: to-institution })
)

;; Get network topology
(define-read-only (get-network-topology (network-id uint))
    (map-get? network-topology { network-id: network-id })
)

;; Get contagion simulation results
(define-read-only (get-contagion-simulation (simulation-id uint))
    (map-get? contagion-simulations { simulation-id: simulation-id })
)

;; Get systemic importance measures
(define-read-only (get-systemic-importance-measures (institution-id uint))
    (map-get? systemic-importance-measures { institution-id: institution-id })
)

;; Get counterparty exposure
(define-read-only (get-counterparty-exposure (institution-id uint) (counterparty-id uint))
    (map-get? counterparty-exposures { institution-id: institution-id, counterparty-id: counterparty-id })
)

;; Get institution count
(define-read-only (get-institution-count)
    (var-get institution-counter)
)

;; Get network count
(define-read-only (get-network-count)
    (var-get network-counter)
)

;; Administrative Functions

;; Update contagion threshold (admin only)
(define-public (update-contagion-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (asserts! (<= new-threshold u10000) (err u400)) ;; Max 100%
        (var-set contagion-threshold new-threshold)
        (ok true)
    )
)

;; Update systemic importance threshold (admin only)
(define-public (update-systemic-importance-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (asserts! (<= new-threshold u10000) (err u400)) ;; Max 100%
        (var-set systemic-importance-threshold new-threshold)
        (ok true)
    )
)
