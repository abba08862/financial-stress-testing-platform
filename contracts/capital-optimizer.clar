;; Financial Stress Testing Platform - Capital Optimizer Contract
;; This contract handles regulatory capital management and Basel III compliance

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u401))
(define-constant err-invalid-institution (err u402))
(define-constant err-invalid-capital-data (err u403))
(define-constant err-optimization-failed (err u404))
(define-constant err-regulatory-breach (err u405))

;; Basel III minimum requirements (in basis points)
(define-constant min-cet1-ratio u450) ;; 4.5%
(define-constant min-tier1-ratio u600) ;; 6.0%
(define-constant min-total-capital-ratio u800) ;; 8.0%
(define-constant min-leverage-ratio u300) ;; 3.0%
(define-constant min-lcr-ratio u10000) ;; 100%
(define-constant min-nsfr-ratio u10000) ;; 100%

;; Helper functions
(define-private (min-uint (a uint) (b uint))
    (if (< a b) a b)
)

;; Data Variables
(define-data-var optimization-counter uint u0)
(define-data-var buffer-multiplier uint u250) ;; 2.5% capital conservation buffer
(define-data-var stress-buffer uint u200) ;; 2.0% additional stress buffer

;; Data Maps
(define-map capital-positions
    { institution-id: uint }
    {
        cet1-capital: uint,
        tier1-capital: uint,
        total-capital: uint,
        risk-weighted-assets: uint,
        total-assets: uint,
        tier1-leverage-exposure: uint,
        high-quality-liquid-assets: uint,
        net-cash-outflows: uint,
        available-stable-funding: uint,
        required-stable-funding: uint,
        last-updated: uint
    }
)

(define-map capital-ratios
    { institution-id: uint }
    {
        cet1-ratio: uint,
        tier1-ratio: uint,
        total-capital-ratio: uint,
        leverage-ratio: uint,
        lcr-ratio: uint,
        nsfr-ratio: uint,
        capital-adequacy-status: (string-ascii 16),
        regulatory-compliance: bool,
        buffer-requirements: uint
    }
)

(define-map optimization-strategies
    { optimization-id: uint }
    {
        institution-id: uint,
        strategy-type: (string-ascii 32),
        target-cet1-ratio: uint,
        target-leverage-ratio: uint,
        estimated-impact: uint,
        implementation-cost: uint,
        timeline-months: uint,
        regulatory-approval-required: bool,
        created-by: principal,
        created-at: uint
    }
)

(define-map buffer-requirements
    { institution-id: uint }
    {
        capital-conservation-buffer: uint,
        countercyclical-buffer: uint,
        gsib-buffer: uint,
        dsib-buffer: uint,
        pillar2-requirements: uint,
        total-buffer-requirement: uint,
        current-buffer-level: uint,
        buffer-breach: bool
    }
)

(define-map stress-test-results
    { institution-id: uint, stress-test-id: uint }
    {
        pre-stress-cet1-ratio: uint,
        post-stress-cet1-ratio: uint,
        stress-loss-amount: uint,
        minimum-cet1-maintained: uint,
        regulatory-minimum-met: bool,
        capital-shortfall: uint,
        recovery-time-estimate: uint,
        supervisory-action-required: bool
    }
)

(define-map capital-planning
    { institution-id: uint }
    {
        planning-horizon: uint,
        projected-growth: uint,
        dividend-policy: (string-ascii 32),
        regulatory-headroom-target: uint,
        stress-test-buffer: uint,
        management-buffer: uint,
        optimization-target: (string-ascii 32)
    }
)

;; Public Functions

;; Update capital position
(define-public (update-capital-position
    (institution-id uint)
    (cet1-capital uint)
    (tier1-capital uint)
    (total-capital uint)
    (risk-weighted-assets uint)
    (total-assets uint)
    (tier1-leverage-exposure uint)
    (high-quality-liquid-assets uint)
    (net-cash-outflows uint)
    (available-stable-funding uint)
    (required-stable-funding uint))
    
    (let 
        (
            (current-time block-height)
        )
        (asserts! (> institution-id u0) err-invalid-institution)
        (asserts! (>= tier1-capital cet1-capital) err-invalid-capital-data)
        (asserts! (>= total-capital tier1-capital) err-invalid-capital-data)
        (asserts! (> risk-weighted-assets u0) err-invalid-capital-data)
        
        ;; Store capital position
        (map-set capital-positions
            { institution-id: institution-id }
            {
                cet1-capital: cet1-capital,
                tier1-capital: tier1-capital,
                total-capital: total-capital,
                risk-weighted-assets: risk-weighted-assets,
                total-assets: total-assets,
                tier1-leverage-exposure: tier1-leverage-exposure,
                high-quality-liquid-assets: high-quality-liquid-assets,
                net-cash-outflows: net-cash-outflows,
                available-stable-funding: available-stable-funding,
                required-stable-funding: required-stable-funding,
                last-updated: current-time
            }
        )
        
        ;; Calculate and update ratios
        (unwrap! (calculate-capital-ratios institution-id) (err u500))
        
        (ok true)
    )
)

;; Calculate capital ratios
(define-public (calculate-capital-ratios (institution-id uint))
    (let 
        (
            (capital-data (unwrap! (map-get? capital-positions { institution-id: institution-id }) err-invalid-institution))
        )
        (let 
            (
                ;; Calculate ratios (in basis points)
                (cet1-ratio (/ (* (get cet1-capital capital-data) u10000) (get risk-weighted-assets capital-data)))
                (tier1-ratio (/ (* (get tier1-capital capital-data) u10000) (get risk-weighted-assets capital-data)))
                (total-cap-ratio (/ (* (get total-capital capital-data) u10000) (get risk-weighted-assets capital-data)))
                (leverage-ratio (/ (* (get tier1-capital capital-data) u10000) (get tier1-leverage-exposure capital-data)))
                (lcr-ratio (if (> (get net-cash-outflows capital-data) u0)
                              (/ (* (get high-quality-liquid-assets capital-data) u10000) (get net-cash-outflows capital-data))
                              u10000))
                (nsfr-ratio (if (> (get required-stable-funding capital-data) u0)
                               (/ (* (get available-stable-funding capital-data) u10000) (get required-stable-funding capital-data))
                               u10000))
                
                ;; Check regulatory compliance
                (cet1-compliant (>= cet1-ratio min-cet1-ratio))
                (tier1-compliant (>= tier1-ratio min-tier1-ratio))
                (total-cap-compliant (>= total-cap-ratio min-total-capital-ratio))
                (leverage-compliant (>= leverage-ratio min-leverage-ratio))
                (lcr-compliant (>= lcr-ratio min-lcr-ratio))
                (nsfr-compliant (>= nsfr-ratio min-nsfr-ratio))
                
                (fully-compliant (and cet1-compliant (and tier1-compliant (and total-cap-compliant 
                                     (and leverage-compliant (and lcr-compliant nsfr-compliant))))))
                
                (adequacy-status (if fully-compliant "well-cap" 
                                    (if cet1-compliant "adequate" "under-cap")))
                
                ;; Calculate buffer requirements
                (buffer-req (calculate-buffer-requirements institution-id))
            )
            
            (map-set capital-ratios
                { institution-id: institution-id }
                {
                    cet1-ratio: cet1-ratio,
                    tier1-ratio: tier1-ratio,
                    total-capital-ratio: total-cap-ratio,
                    leverage-ratio: leverage-ratio,
                    lcr-ratio: lcr-ratio,
                    nsfr-ratio: nsfr-ratio,
                    capital-adequacy-status: adequacy-status,
                    regulatory-compliance: fully-compliant,
                    buffer-requirements: buffer-req
                }
            )
            
            (ok {
                cet1-ratio: cet1-ratio,
                regulatory-compliance: fully-compliant,
                adequacy-status: adequacy-status
            })
        )
    )
)

;; Calculate buffer requirements
(define-private (calculate-buffer-requirements (institution-id uint))
    (let 
        (
            (conservation-buffer (var-get buffer-multiplier))
            (countercyclical-buffer u0) ;; Simplified - would be set by regulator
            (gsib-buffer u0) ;; Would be calculated based on systemic importance
            (dsib-buffer u100) ;; 1% for domestic systemically important banks
            (pillar2-req u150) ;; 1.5% additional requirement
        )
        ;; Calculate total buffer requirement
        (let 
            (
                (total-buffer (+ conservation-buffer (+ countercyclical-buffer 
                                (+ gsib-buffer (+ dsib-buffer pillar2-req)))))
            )
            
            (map-set buffer-requirements
                { institution-id: institution-id }
                {
                    capital-conservation-buffer: conservation-buffer,
                    countercyclical-buffer: countercyclical-buffer,
                    gsib-buffer: gsib-buffer,
                    dsib-buffer: dsib-buffer,
                    pillar2-requirements: pillar2-req,
                    total-buffer-requirement: total-buffer,
                    current-buffer-level: u0, ;; Would be calculated from actual ratios
                    buffer-breach: false
                }
            )
            
            total-buffer
        )
    )
)

;; Create capital optimization strategy
(define-public (create-optimization-strategy
    (institution-id uint)
    (strategy-type (string-ascii 32))
    (target-cet1-ratio uint)
    (target-leverage-ratio uint)
    (estimated-impact uint)
    (implementation-cost uint)
    (timeline-months uint)
    (regulatory-approval-required bool))
    
    (let 
        (
            (optimization-id (+ (var-get optimization-counter) u1))
            (current-time block-height)
        )
        (asserts! (> institution-id u0) err-invalid-institution)
        (asserts! (>= target-cet1-ratio min-cet1-ratio) (err u400))
        (asserts! (>= target-leverage-ratio min-leverage-ratio) (err u400))
        
        (map-set optimization-strategies
            { optimization-id: optimization-id }
            {
                institution-id: institution-id,
                strategy-type: strategy-type,
                target-cet1-ratio: target-cet1-ratio,
                target-leverage-ratio: target-leverage-ratio,
                estimated-impact: estimated-impact,
                implementation-cost: implementation-cost,
                timeline-months: timeline-months,
                regulatory-approval-required: regulatory-approval-required,
                created-by: tx-sender,
                created-at: current-time
            }
        )
        
        (var-set optimization-counter optimization-id)
        (ok optimization-id)
    )
)

;; Run stress test on capital
(define-public (run-capital-stress-test (institution-id uint) (stress-scenario-id uint) (stress-loss-rate uint))
    (let 
        (
            (capital-data (unwrap! (map-get? capital-positions { institution-id: institution-id }) err-invalid-institution))
            (ratio-data (unwrap! (map-get? capital-ratios { institution-id: institution-id }) err-invalid-capital-data))
        )
        (let 
            (
                (pre-stress-cet1 (get cet1-ratio ratio-data))
                (stress-loss (/ (* (get risk-weighted-assets capital-data) stress-loss-rate) u10000))
                (post-stress-cet1-capital (if (>= (get cet1-capital capital-data) stress-loss)
                                             (- (get cet1-capital capital-data) stress-loss)
                                             u0))
                (post-stress-cet1-ratio (/ (* post-stress-cet1-capital u10000) (get risk-weighted-assets capital-data)))
                (minimum-met (>= post-stress-cet1-ratio min-cet1-ratio))
                (capital-shortfall (if minimum-met u0 (- min-cet1-ratio post-stress-cet1-ratio)))
                (recovery-time (if minimum-met u0 u24)) ;; 24 months recovery estimate
                (supervisory-action (not minimum-met))
            )
            
            (map-set stress-test-results
                { institution-id: institution-id, stress-test-id: stress-scenario-id }
                {
                    pre-stress-cet1-ratio: pre-stress-cet1,
                    post-stress-cet1-ratio: post-stress-cet1-ratio,
                    stress-loss-amount: stress-loss,
                    minimum-cet1-maintained: post-stress-cet1-capital,
                    regulatory-minimum-met: minimum-met,
                    capital-shortfall: capital-shortfall,
                    recovery-time-estimate: recovery-time,
                    supervisory-action-required: supervisory-action
                }
            )
            
            (ok {
                post-stress-ratio: post-stress-cet1-ratio,
                minimum-met: minimum-met,
                shortfall: capital-shortfall
            })
        )
    )
)

;; Create capital planning framework
(define-public (create-capital-plan
    (institution-id uint)
    (planning-horizon uint)
    (projected-growth uint)
    (dividend-policy (string-ascii 32))
    (regulatory-headroom-target uint)
    (stress-test-buffer uint)
    (management-buffer uint)
    (optimization-target (string-ascii 32)))
    
    (begin
        (asserts! (> institution-id u0) err-invalid-institution)
        (asserts! (> planning-horizon u0) (err u400))
        (asserts! (>= regulatory-headroom-target u100) (err u400)) ;; Minimum 1% headroom
        
        (map-set capital-planning
            { institution-id: institution-id }
            {
                planning-horizon: planning-horizon,
                projected-growth: projected-growth,
                dividend-policy: dividend-policy,
                regulatory-headroom-target: regulatory-headroom-target,
                stress-test-buffer: stress-test-buffer,
                management-buffer: management-buffer,
                optimization-target: optimization-target
            }
        )
        
        (ok true)
    )
)

;; Check regulatory compliance
(define-public (check-regulatory-compliance (institution-id uint))
    (let 
        (
            (ratio-data (unwrap! (map-get? capital-ratios { institution-id: institution-id }) err-invalid-institution))
            (buffer-data (unwrap! (map-get? buffer-requirements { institution-id: institution-id }) err-invalid-capital-data))
        )
        (let 
            (
                (compliance-score (calculate-compliance-score ratio-data buffer-data))
                (risk-rating (if (>= compliance-score u8000) "low-risk" 
                               (if (>= compliance-score u6000) "moderate-risk" "high-risk")))
            )
            
            (ok {
                regulatory-compliance: (get regulatory-compliance ratio-data),
                compliance-score: compliance-score,
                risk-rating: risk-rating,
                cet1-status: (>= (get cet1-ratio ratio-data) (+ min-cet1-ratio (var-get buffer-multiplier))),
                leverage-status: (>= (get leverage-ratio ratio-data) min-leverage-ratio),
                liquidity-status: (>= (get lcr-ratio ratio-data) min-lcr-ratio)
            })
        )
    )
)

;; Calculate compliance score
(define-private (calculate-compliance-score (ratios (tuple (cet1-ratio uint) (tier1-ratio uint) (total-capital-ratio uint) (leverage-ratio uint) (lcr-ratio uint) (nsfr-ratio uint) (capital-adequacy-status (string-ascii 16)) (regulatory-compliance bool) (buffer-requirements uint))) (buffers (tuple (capital-conservation-buffer uint) (countercyclical-buffer uint) (gsib-buffer uint) (dsib-buffer uint) (pillar2-requirements uint) (total-buffer-requirement uint) (current-buffer-level uint) (buffer-breach bool))))
    ;; Simplified scoring algorithm
    (let 
        (
            (cet1-score (min-uint u2000 (/ (get cet1-ratio ratios) u5)))
            (leverage-score (min-uint u2000 (/ (get leverage-ratio ratios) u2)))
            (liquidity-score (min-uint u2000 (/ (get lcr-ratio ratios) u10)))
            (buffer-score (if (get buffer-breach buffers) u0 u2000))
        )
        (+ cet1-score (+ leverage-score (+ liquidity-score buffer-score)))
    )
)

;; Optimize capital allocation
(define-public (optimize-capital-allocation (institution-id uint) (optimization-target (string-ascii 32)))
    (let 
        (
            (capital-data (unwrap! (map-get? capital-positions { institution-id: institution-id }) err-invalid-institution))
            (ratio-data (unwrap! (map-get? capital-ratios { institution-id: institution-id }) err-invalid-capital-data))
        )
        ;; Analyze current position and recommend actions
        (let 
            (
                (current-cet1 (get cet1-ratio ratio-data))
                (current-leverage (get leverage-ratio ratio-data))
                (target-cet1 (+ min-cet1-ratio (var-get buffer-multiplier)))
                (target-leverage (+ min-leverage-ratio u100)) ;; 1% above minimum
                
                ;; Calculate optimization impact
                (cet1-gap (if (< current-cet1 target-cet1) (- target-cet1 current-cet1) u0))
                (leverage-gap (if (< current-leverage target-leverage) (- target-leverage current-leverage) u0))
                (optimization-priority (if (> cet1-gap leverage-gap) "cet1-focused" "leverage-focused"))
            )
            
            ;; Create optimization strategy
            (create-optimization-strategy 
                institution-id
                optimization-priority
                target-cet1
                target-leverage
                u500000000 ;; Estimated $500M impact
                u10000000  ;; $10M implementation cost
                u6         ;; 6 months timeline
                true)      ;; Requires regulatory approval
        )
    )
)

;; Read-only Functions

;; Get capital position
(define-read-only (get-capital-position (institution-id uint))
    (map-get? capital-positions { institution-id: institution-id })
)

;; Get capital ratios
(define-read-only (get-capital-ratios (institution-id uint))
    (map-get? capital-ratios { institution-id: institution-id })
)

;; Get optimization strategy
(define-read-only (get-optimization-strategy (optimization-id uint))
    (map-get? optimization-strategies { optimization-id: optimization-id })
)

;; Get buffer requirements
(define-read-only (get-buffer-requirements (institution-id uint))
    (map-get? buffer-requirements { institution-id: institution-id })
)

;; Get stress test results
(define-read-only (get-stress-test-results (institution-id uint) (stress-test-id uint))
    (map-get? stress-test-results { institution-id: institution-id, stress-test-id: stress-test-id })
)

;; Get capital plan
(define-read-only (get-capital-plan (institution-id uint))
    (map-get? capital-planning { institution-id: institution-id })
)

;; Get minimum ratios
(define-read-only (get-minimum-ratios)
    {
        min-cet1-ratio: min-cet1-ratio,
        min-tier1-ratio: min-tier1-ratio,
        min-total-capital-ratio: min-total-capital-ratio,
        min-leverage-ratio: min-leverage-ratio,
        min-lcr-ratio: min-lcr-ratio,
        min-nsfr-ratio: min-nsfr-ratio
    }
)

;; Administrative Functions

;; Update buffer multiplier (admin only)
(define-public (update-buffer-multiplier (new-multiplier uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (asserts! (<= new-multiplier u500) (err u400)) ;; Max 5%
        (var-set buffer-multiplier new-multiplier)
        (ok true)
    )
)

;; Update stress buffer (admin only)
(define-public (update-stress-buffer (new-buffer uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (asserts! (<= new-buffer u1000) (err u400)) ;; Max 10%
        (var-set stress-buffer new-buffer)
        (ok true)
    )
)
