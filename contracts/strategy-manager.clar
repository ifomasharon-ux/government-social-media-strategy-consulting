;; Government Social Media Strategy Manager
;; Manages strategy documents, performance metrics, and stakeholder approvals

(define-constant ERR_NOT_FOUND 404)
(define-constant ERR_UNAUTHORIZED 401)
(define-constant ERR_INVALID_INPUT 400)
(define-constant ERR_ALREADY_EXISTS 409)
(define-constant ERR_INSUFFICIENT_METRICS 402)

(define-data-var strategy-counter uint u0)
(define-data-var performance-counter uint u0)
(define-data-var next-strategy-id uint u1)

(define-map strategies
  { strategy-id: uint }
  {
    title: (string-ascii 128),
    department: (string-ascii 64),
    created-at: uint,
    status: (string-ascii 16),
    target-audience: (string-ascii 256),
    budget-allocation: uint
  }
)

(define-map performance-metrics
  { metric-id: uint }
  {
    strategy-id: uint,
    engagement-rate: uint,
    reach-count: uint,
    recorded-at: uint,
    sentiment-score: uint
  }
)

(define-map stakeholder-approvals
  { approval-id: uint }
  {
    strategy-id: uint,
    approver: principal,
    approved-at: uint,
    feedback: (string-ascii 256)
  }
)

(define-public (create-strategy (title (string-ascii 128)) (department (string-ascii 64)) (target-audience (string-ascii 256)) (budget uint))
  (let ((strategy-id (var-get next-strategy-id)))
    (if (and (> (len title) u0) (> (len department) u0) (> budget u0))
      (begin
        (map-insert strategies
          { strategy-id: strategy-id }
          {
            title: title,
            department: department,
            created-at: burn-block-height,
            status: "draft",
            target-audience: target-audience,
            budget-allocation: budget
          }
        )
        (var-set strategy-counter (+ (var-get strategy-counter) u1))
        (var-set next-strategy-id (+ strategy-id u1))
        (ok strategy-id)
      )
      (err ERR_INVALID_INPUT)
    )
  )
)

(define-public (record-performance-metric (strategy-id uint) (engagement uint) (reach uint) (sentiment uint))
  (match (map-get? strategies { strategy-id: strategy-id })
    strategy
    (let ((metric-id (var-get performance-counter)))
      (if (and (>= engagement u0) (>= reach u0) (and (>= sentiment u0) (<= sentiment u100)))
        (begin
          (map-insert performance-metrics
            { metric-id: metric-id }
            {
              strategy-id: strategy-id,
              engagement-rate: engagement,
              reach-count: reach,
              recorded-at: burn-block-height,
              sentiment-score: sentiment
            }
          )
          (var-set performance-counter (+ metric-id u1))
          (ok metric-id)
        )
        (err ERR_INVALID_INPUT)
      )
    )
    (err ERR_NOT_FOUND)
  )
)

(define-public (submit-stakeholder-approval (strategy-id uint) (feedback (string-ascii 256)))
  (match (map-get? strategies { strategy-id: strategy-id })
    strategy
    (let ((approval-id (+ (var-get strategy-counter) (var-get performance-counter))))
      (begin
        (map-insert stakeholder-approvals
          { approval-id: approval-id }
          {
            strategy-id: strategy-id,
            approver: tx-sender,
            approved-at: burn-block-height,
            feedback: feedback
          }
        )
        (ok approval-id)
      )
    )
    (err ERR_NOT_FOUND)
  )
)

(define-public (update-strategy-status (strategy-id uint) (new-status (string-ascii 16)))
  (match (map-get? strategies { strategy-id: strategy-id })
    strategy
    (if (or (is-eq new-status "draft") (or (is-eq new-status "active") (or (is-eq new-status "completed") (is-eq new-status "archived"))))
      (begin
        (map-set strategies
          { strategy-id: strategy-id }
          (merge strategy { status: new-status })
        )
        (ok strategy-id)
      )
      (err ERR_INVALID_INPUT)
    )
    (err ERR_NOT_FOUND)
  )
)

(define-read-only (get-strategy (strategy-id uint))
  (map-get? strategies { strategy-id: strategy-id })
)

(define-read-only (get-performance-metric (metric-id uint))
  (map-get? performance-metrics { metric-id: metric-id })
)

(define-read-only (get-stakeholder-approval (approval-id uint))
  (map-get? stakeholder-approvals { approval-id: approval-id })
)

(define-read-only (get-total-strategies)
  (var-get strategy-counter)
)

(define-read-only (get-total-metrics)
  (var-get performance-counter)
)

(define-read-only (get-average-sentiment-for-strategy (strategy-id uint))
  (let (
    (total (var-get performance-counter))
  )
    (if (> total u0)
      (some u0)
      none
    )
  )
)

