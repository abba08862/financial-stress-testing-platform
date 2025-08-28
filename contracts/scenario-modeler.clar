;; Financial Stress Testing Platform - Scenario Modeler Contract
;; This contract handles stress testing scenarios and Monte Carlo simulations

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u401))
(define-constant err-invalid-scenario (err u402))
(define-constant err-invalid-parameters (err u403))
(define-constant err-simulation-failed (err u404))
(define-constant err-insufficient-data (err u405))

;; Helper functions
(define-private (abs-int (x int))
    (if (>= x 0) (to-uint x) (to-uint (- 0 x)))
)

;; Data Variables
(define-data-var scenario-counter uint u0)
(define-data-var simulation-counter uint u0)
(define-data-var monte-carlo-iterations uint u10000)
(define-data-var simulation-seed uint u12345)

;; Data Maps
(define-map scenarios
    { scenario-id: uint }
    {
        name: (string-ascii 64),
        scenario-type: (string-ascii 32),
        description: (string-ascii 256),
        created-by: principal,
        created-at: uint,
        status: (string-ascii 16),
        regulatory-compliant: bool,
        severity-score: uint
    }
)

(define-map scenario-parameters
    { scenario-id: uint }
    {
        equity-shock: int,
        bond-shock: int,
        fx-shock: int,
        commodity-shock: int,
        real-estate-shock: int,
        credit-spread-shock: int,
        interest-rate-shock: int,
        volatility-shock: int,
        liquidity-factor: uint,
        correlation-breakdown: bool
    }
)

(define-map simulation-results
    { simulation-id: uint }
    {
        scenario-id: uint,
        mean-return: int,
        standard-deviation: uint,
        var-95: uint,
        var-99: uint,
        expected-shortfall: uint,
        maximum-loss: uint,
        probability-of-loss: uint,
        tail-risk-measure: uint,
        stress-test-pass: bool,
        iterations-completed: uint
    }
)

(define-map historical-scenarios
    { scenario-name: (string-ascii 64) }
    {
        scenario-id: uint,
        start-date: uint,
        end-date: uint,
        market-conditions: (string-ascii 128),
        severity-level: uint,
        replication-accuracy: uint,
        economic-impact: uint
    }
)

(define-map regulatory-scenarios
    { regulation-type: (string-ascii 32) }
    {
        scenario-id: uint,
        regulation-name: (string-ascii 64),
        jurisdiction: (string-ascii 32),
        minimum-capital-ratio: uint,
        stress-severity: uint,
        reporting-frequency: uint,
        compliance-threshold: uint
    }
)

;; Public Functions

;; Create new scenario
(define-public (create-scenario 
    (name (string-ascii 64))
    (scenario-type (string-ascii 32))
    (description (string-ascii 256))
    (regulatory-compliant bool))
    
    (let 
        (
            (scenario-id (+ (var-get scenario-counter) u1))
            (current-time block-height)
        )
        (asserts! (> (len name) u0) (err u400))
        (asserts! (> (len scenario-type) u0) (err u400))
        
        (map-set scenarios
            { scenario-id: scenario-id }
            {
                name: name,
                scenario-type: scenario-type,
                description: description,
                created-by: tx-sender,
                created-at: current-time,
                status: "draft",
                regulatory-compliant: regulatory-compliant,
                severity-score: u0
            }
        )
        
        (var-set scenario-counter scenario-id)
        (ok scenario-id)
    )
)

;; Configure scenario parameters
(define-public (configure-scenario-parameters
    (scenario-id uint)
    (equity-shock int)
    (bond-shock int)
    (fx-shock int)
    (commodity-shock int)
    (real-estate-shock int)
    (credit-spread-shock int)
    (interest-rate-shock int)
    (volatility-shock int)
    (liquidity-factor uint)
    (correlation-breakdown bool))
    
    (let 
        (
            (scenario-data (unwrap! (map-get? scenarios { scenario-id: scenario-id }) err-invalid-scenario))
        )
        (asserts! (is-eq (get created-by scenario-data) tx-sender) err-unauthorized)
        (asserts! (<= (abs-int equity-shock) u10000) err-invalid-parameters) ;; Max 100% shock
        (asserts! (<= (abs-int bond-shock) u5000) err-invalid-parameters) ;; Max 50% shock
        
        ;; Calculate severity score based on parameters
        (let 
            (
                (total-equity-impact (abs-int equity-shock))
                (total-bond-impact (abs-int bond-shock))
                (severity-score (+ total-equity-impact total-bond-impact))
            )
            
            (map-set scenario-parameters
                { scenario-id: scenario-id }
                {
                    equity-shock: equity-shock,
                    bond-shock: bond-shock,
                    fx-shock: fx-shock,
                    commodity-shock: commodity-shock,
                    real-estate-shock: real-estate-shock,
                    credit-spread-shock: credit-spread-shock,
                    interest-rate-shock: interest-rate-shock,
                    volatility-shock: volatility-shock,
                    liquidity-factor: liquidity-factor,
                    correlation-breakdown: correlation-breakdown
                }
            )
            
            ;; Update scenario status and severity
            (map-set scenarios
                { scenario-id: scenario-id }
                (merge scenario-data { 
                    status: "configured",
                    severity-score: severity-score
                })
            )
            
            (ok true)
        )
    )
)

;; Run Monte Carlo simulation
(define-public (run-monte-carlo-simulation
    (scenario-id uint)
    (iterations uint))
    
    (let 
        (
            (scenario-data (unwrap! (map-get? scenarios { scenario-id: scenario-id }) err-invalid-scenario))
            (scenario-params (unwrap! (map-get? scenario-parameters { scenario-id: scenario-id }) err-invalid-parameters))
            (simulation-id (+ (var-get simulation-counter) u1))
        )
        (asserts! (is-eq (get status scenario-data) "configured") (err u406))
        (asserts! (> iterations u0) err-invalid-parameters)
        (asserts! (<= iterations u100000) err-invalid-parameters) ;; Max 100k iterations
        
        ;; Run simplified Monte Carlo simulation
        (let 
            (
                (simulation-results-data (execute-monte-carlo scenario-params iterations))
            )
            
            ;; Store results
            (map-set simulation-results
                { simulation-id: simulation-id }
                (merge simulation-results-data { 
                    scenario-id: scenario-id,
                    iterations-completed: iterations
                })
            )
            
            (var-set simulation-counter simulation-id)
            (ok simulation-id)
        )
    )
)

;; Execute Monte Carlo simulation (simplified)
(define-private (execute-monte-carlo (params (tuple (equity-shock int) (bond-shock int) (fx-shock int) (commodity-shock int) (real-estate-shock int) (credit-spread-shock int) (interest-rate-shock int) (volatility-shock int) (liquidity-factor uint) (correlation-breakdown bool))) (iterations uint))
    (let 
        (
            ;; Simplified calculation based on shocks
            (equity-impact (abs-int (get equity-shock params)))
            (bond-impact (abs-int (get bond-shock params)))
            (total-shock (+ equity-impact bond-impact))
            
            ;; Monte Carlo simulation results (simplified)
            (mean-return (- 0 (to-int (/ total-shock u10)))) ;; Negative return
            (std-deviation (+ total-shock (/ total-shock u4))) ;; Volatility increases
            (var-95 (+ total-shock (/ total-shock u2)))
            (var-99 (+ total-shock (/ (* total-shock u3) u4)))
            (expected-shortfall (+ var-99 (/ var-99 u3)))
            (maximum-loss (* total-shock u2))
            (prob-of-loss (if (> total-shock u1000) u8500 u7000)) ;; 85% or 70%
            (tail-risk (/ maximum-loss u2))
            (stress-pass (< total-shock u3000)) ;; Pass if total shock < 30%
        )
        {
            mean-return: mean-return,
            standard-deviation: std-deviation,
            var-95: var-95,
            var-99: var-99,
            expected-shortfall: expected-shortfall,
            maximum-loss: maximum-loss,
            probability-of-loss: prob-of-loss,
            tail-risk-measure: tail-risk,
            stress-test-pass: stress-pass
        }
    )
)

;; Create historical scenario replication
(define-public (create-historical-scenario
    (scenario-name (string-ascii 64))
    (start-date uint)
    (end-date uint)
    (market-conditions (string-ascii 128))
    (severity-level uint))
    
    (let 
        (
            (scenario-id (+ (var-get scenario-counter) u1))
        )
        (asserts! (> (len scenario-name) u0) (err u400))
        (asserts! (< start-date end-date) err-invalid-parameters)
        (asserts! (<= severity-level u10) err-invalid-parameters)
        
        ;; Create base scenario
        (unwrap! (create-scenario scenario-name "historical" market-conditions true) (err u500))
        
        ;; Store historical details
        (map-set historical-scenarios
            { scenario-name: scenario-name }
            {
                scenario-id: scenario-id,
                start-date: start-date,
                end-date: end-date,
                market-conditions: market-conditions,
                severity-level: severity-level,
                replication-accuracy: u8500, ;; 85% accuracy
                economic-impact: (* severity-level u1000)
            }
        )
        
        (ok scenario-id)
    )
)

;; Create regulatory compliance scenario
(define-public (create-regulatory-scenario
    (regulation-type (string-ascii 32))
    (regulation-name (string-ascii 64))
    (jurisdiction (string-ascii 32))
    (minimum-capital-ratio uint)
    (stress-severity uint))
    
    (let 
        (
            (scenario-id (+ (var-get scenario-counter) u1))
        )
        (asserts! (> (len regulation-name) u0) (err u400))
        (asserts! (> minimum-capital-ratio u0) err-invalid-parameters)
        (asserts! (<= stress-severity u10) err-invalid-parameters)
        
        ;; Create regulatory scenario
        (unwrap! (create-scenario regulation-name "regulatory" "Regulatory compliance scenario" true) (err u500))
        
        ;; Store regulatory details
        (map-set regulatory-scenarios
            { regulation-type: regulation-type }
            {
                scenario-id: scenario-id,
                regulation-name: regulation-name,
                jurisdiction: jurisdiction,
                minimum-capital-ratio: minimum-capital-ratio,
                stress-severity: stress-severity,
                reporting-frequency: u4, ;; Quarterly
                compliance-threshold: u8000 ;; 80% threshold
            }
        )
        
        (ok scenario-id)
    )
)

;; Validate scenario against regulatory requirements
(define-public (validate-regulatory-compliance (scenario-id uint) (regulation-type (string-ascii 32)))
    (let 
        (
            (scenario-data (unwrap! (map-get? scenarios { scenario-id: scenario-id }) err-invalid-scenario))
            (regulatory-data (map-get? regulatory-scenarios { regulation-type: regulation-type }))
        )
        (asserts! (is-eq (get created-by scenario-data) tx-sender) err-unauthorized)
        
        (match regulatory-data
            reg-scenario 
            (let 
                (
                    (required-severity (get stress-severity reg-scenario))
                    (minimum-capital (get minimum-capital-ratio reg-scenario))
                    (scenario-severity (get severity-score scenario-data))
                )
                ;; Check compliance criteria
                (ok {
                    compliant: (and (get regulatory-compliant scenario-data) 
                                   (>= minimum-capital u800)), ;; 8% minimum
                    severity-met: (>= scenario-severity (* required-severity u1000)),
                    capital-adequate: (>= minimum-capital u800)
                })
            )
            (ok {
                compliant: false,
                severity-met: false,
                capital-adequate: false
            })
        )
    )
)

;; Read-only Functions

;; Get scenario details
(define-read-only (get-scenario (scenario-id uint))
    (map-get? scenarios { scenario-id: scenario-id })
)

;; Get scenario parameters
(define-read-only (get-scenario-parameters (scenario-id uint))
    (map-get? scenario-parameters { scenario-id: scenario-id })
)

;; Get simulation results
(define-read-only (get-simulation-results (simulation-id uint))
    (map-get? simulation-results { simulation-id: simulation-id })
)

;; Get historical scenario
(define-read-only (get-historical-scenario (scenario-name (string-ascii 64)))
    (map-get? historical-scenarios { scenario-name: scenario-name })
)

;; Get regulatory scenario
(define-read-only (get-regulatory-scenario (regulation-type (string-ascii 32)))
    (map-get? regulatory-scenarios { regulation-type: regulation-type })
)

;; Get scenario count
(define-read-only (get-scenario-count)
    (var-get scenario-counter)
)

;; Get simulation count
(define-read-only (get-simulation-count)
    (var-get simulation-counter)
)

;; Check scenario ownership
(define-read-only (is-scenario-owner (scenario-id uint) (user principal))
    (match (map-get? scenarios { scenario-id: scenario-id })
        scenario-data (is-eq (get created-by scenario-data) user)
        false
    )
)

;; Administrative Functions

;; Update Monte Carlo iterations limit (admin only)
(define-public (update-monte-carlo-iterations (new-limit uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (asserts! (> new-limit u0) err-invalid-parameters)
        (var-set monte-carlo-iterations new-limit)
        (ok true)
    )
)

;; Update simulation seed (admin only)
(define-public (update-simulation-seed (new-seed uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (var-set simulation-seed new-seed)
        (ok true)
    )
)
