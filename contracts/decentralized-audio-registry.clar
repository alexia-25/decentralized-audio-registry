;; Decentralized Audio Registry Network Contract
;;
;; This smart contract enables a distributed system for cataloging and managing audio recordings.
;; It provides functionality for registering, transferring, and managing digital sound assets
;; in a decentralized manner with access control and collaborative features.

;; Last Updated: April 2025

;; ========================================================================
;; Section 1: Core Configuration & Registry Variables
;; ========================================================================

;; Tracks the total number of registered audio entries in the system
(define-data-var registry-entry-counter uint u0)

;; Tracks the total number of audio compilations created by users
(define-data-var compilation-registry-counter uint u0)

;; ========================================================================
;; Section 2: System Constants & Error Codes
;; ========================================================================

;; Registry administrator (deployer of the contract)
(define-constant REGISTRY-ADMIN tx-sender)

;; Error Code Definitions for Enhanced Debugging
(define-constant ERROR-ENTRY-NOT-LOCATED (err u301))            ;; Entry cannot be found in the registry
(define-constant ERROR-ENTRY-ALREADY-EXISTS (err u302))         ;; Entry already exists in the registry
(define-constant ERROR-INVALID-ENTRY-NAME (err u303))           ;; Invalid entry name provided
(define-constant ERROR-INVALID-ENTRY-LENGTH (err u304))         ;; Invalid entry length provided
(define-constant ERROR-INSUFFICIENT-PRIVILEGES (err u305))      ;; User lacks necessary privileges
(define-constant ERROR-PERMISSION-REJECTED (err u306))          ;; Permission denied for the operation
(define-constant ERROR-ADMIN-FUNCTION (err u307))               ;; Function restricted to admin only
(define-constant ERROR-OPERATION-PROHIBITED (err u308))         ;; Operation not allowed in current context

;; ========================================================================
;; Section 3: Primary Data Storage Structures
;; ========================================================================

;; Maps entry identifiers to their comprehensive details
(define-map audio-registry
    {entry-id: uint}  ;; Primary Key: Unique entry identifier
    {
        name: (string-ascii 64),                 ;; Entry title/name (max 64 chars)
        creator: (string-ascii 32),              ;; Creator name (max 32 chars)
        custodian: principal,                    ;; Current custodian address
        length-seconds: uint,                    ;; Length in seconds
        registration-block: uint,                ;; Block height at registration time
        category: (string-ascii 32),             ;; Category classification
        descriptors: (list 8 (string-ascii 24))  ;; Descriptive labels (max 8)
    }
)

;; Maps user access rights for each registry entry
(define-map access-registry
    {entry-id: uint, participant: principal}     ;; Composite Key: Entry ID and User Principal
    {has-access: bool}                           ;; Value: Access permission status
)

;; ========================================================================
;; Section 4: Helper Functions (Internal Use)
;; ========================================================================

;; Verifies if an entry exists in the audio registry
(define-private (entry-exists-in-registry (entry-id uint))
    (is-some (map-get? audio-registry {entry-id: entry-id}))
)

;; Verifies if a user is the custodian of an entry
(define-private (is-entry-custodian (entry-id uint) (participant principal))
    (match (map-get? audio-registry {entry-id: entry-id})
        entry-details (is-eq (get custodian entry-details) participant)
        false
    )
)

;; Retrieves the length of an entry in seconds
(define-private (get-entry-length-seconds (entry-id uint))
    (default-to u0 
        (get length-seconds 
            (map-get? audio-registry {entry-id: entry-id})
        )
    )
)

;; Validates a descriptor tag format and length
(define-private (validate-descriptor (descriptor (string-ascii 24)))
    (and 
        (> (len descriptor) u0)  ;; Descriptor must not be empty
        (< (len descriptor) u25)  ;; Descriptor must be under 25 chars
    )
)

;; Validates a collection of descriptor tags
(define-private (validate-descriptor-collection (descriptors (list 8 (string-ascii 24))))
    (and
        (> (len descriptors) u0)  ;; Must have at least one descriptor
        (<= (len descriptors) u8)  ;; Cannot exceed 8 descriptors
        (is-eq (len (filter validate-descriptor descriptors)) (len descriptors))  ;; All descriptors must be valid
    )
)

;; ========================================================================
;; Section 5: Registry Entry Management Functions
;; ========================================================================

;; Registers a new audio entry in the decentralized registry
(define-public (register-audio-entry 
        (name (string-ascii 64))                 ;; Entry name/title
        (creator (string-ascii 32))              ;; Creator attribution
        (length-seconds uint)                    ;; Entry length in seconds
        (category (string-ascii 32))             ;; Classification category
        (descriptors (list 8 (string-ascii 24))) ;; Descriptive labels
    )
    (let
        ((new-entry-id (+ (var-get registry-entry-counter) u1)))  ;; Generate new unique ID

        ;; Input validation checks
        (asserts! (and (> (len name) u0) (< (len name) u65)) ERROR-INVALID-ENTRY-NAME)
        (asserts! (and (> (len creator) u0) (< (len creator) u33)) ERROR-INVALID-ENTRY-NAME)
        (asserts! (and (> length-seconds u0) (< length-seconds u10000)) ERROR-INVALID-ENTRY-LENGTH)
        (asserts! (and (> (len category) u0) (< (len category) u33)) ERROR-INVALID-ENTRY-NAME)
        (asserts! (validate-descriptor-collection descriptors) ERROR-INVALID-ENTRY-NAME)

        ;; Register the entry in the main registry
        (map-insert audio-registry
            {entry-id: new-entry-id}
            {
                name: name,
                creator: creator,
                custodian: tx-sender,  ;; Registrant becomes custodian
                length-seconds: length-seconds,
                registration-block: block-height,
                category: category,
                descriptors: descriptors
            }
        )

        ;; Grant access to the registrant by default
        (map-insert access-registry
            {entry-id: new-entry-id, participant: tx-sender}
            {has-access: true}
        )

        ;; Increment counter and return the new entry ID
        (var-set registry-entry-counter new-entry-id)
        (ok new-entry-id)
    )
)

;; Removes an audio entry from the registry (custodian only)
(define-public (deregister-audio-entry (entry-id uint))
    (let
        ((entry-details (unwrap! (map-get? audio-registry {entry-id: entry-id}) ERROR-ENTRY-NOT-LOCATED)))

        ;; Validation checks
        (asserts! (entry-exists-in-registry entry-id) ERROR-ENTRY-NOT-LOCATED)
        (asserts! (is-eq (get custodian entry-details) tx-sender) ERROR-INSUFFICIENT-PRIVILEGES)

        ;; Remove the entry from registry
        (map-delete audio-registry {entry-id: entry-id})
        (map-delete access-registry {entry-id: entry-id, participant: tx-sender})
        (ok true)
    )
)

;; Transfers custodianship of an entry to another user
(define-public (transfer-entry-custodianship (entry-id uint) (new-custodian principal))
    (let
        ((entry-details (unwrap! (map-get? audio-registry {entry-id: entry-id}) ERROR-ENTRY-NOT-LOCATED)))

        ;; Validation checks
        (asserts! (entry-exists-in-registry entry-id) ERROR-ENTRY-NOT-LOCATED)
        (asserts! (is-eq (get custodian entry-details) tx-sender) ERROR-INSUFFICIENT-PRIVILEGES)

        ;; Update the custodian information
        (map-set audio-registry
            {entry-id: entry-id}
            (merge entry-details {custodian: new-custodian})
        )
        (ok true)
    )
)

;; Updates metadata for an existing entry (custodian only)
(define-public (update-entry-metadata 
        (entry-id uint) 
        (updated-name (string-ascii 64)) 
        (updated-length uint) 
        (updated-category (string-ascii 32)) 
        (updated-descriptors (list 8 (string-ascii 24)))
    )
    (let
        ((entry-details (unwrap! (map-get? audio-registry {entry-id: entry-id}) ERROR-ENTRY-NOT-LOCATED)))

        ;; Validation checks
        (asserts! (entry-exists-in-registry entry-id) ERROR-ENTRY-NOT-LOCATED)
        (asserts! (is-eq (get custodian entry-details) tx-sender) ERROR-INSUFFICIENT-PRIVILEGES)
        (asserts! (and (> (len updated-name) u0) (< (len updated-name) u65)) ERROR-INVALID-ENTRY-NAME)
        (asserts! (and (> updated-length u0) (< updated-length u10000)) ERROR-INVALID-ENTRY-LENGTH)
        (asserts! (and (> (len updated-category) u0) (< (len updated-category) u33)) ERROR-INVALID-ENTRY-NAME)
        (asserts! (validate-descriptor-collection updated-descriptors) ERROR-INVALID-ENTRY-NAME)

        ;; Update the entry metadata
        (map-set audio-registry
            {entry-id: entry-id}
            (merge entry-details {
                name: updated-name,
                length-seconds: updated-length,
                category: updated-category,
                descriptors: updated-descriptors
            })
        )
        (ok true)
    )
)

;; ========================================================================
;; Section 6: Extended Data Structures
;; ========================================================================

;; Tracks user collection information
(define-map user-collection-tracker
    {participant: principal}
    {latest-collection-id: uint}
)

;; Stores collection metadata
(define-map audio-collections
    {custodian: principal, collection-id: uint}
    {
        title: (string-ascii 64),
        description: (string-ascii 128),
        creation-timestamp: uint,
        modification-timestamp: uint,
        entry-count: uint,
        public-access: bool
    }
)

;; Maps entries within collections
(define-map collection-entries
    {collection-custodian: principal, collection-id: uint, entry-id: uint}
    {
        addition-timestamp: uint,
        sequence-position: uint
    }
)

;; Records access grant history
(define-map access-grant-history
    {entry-id: uint, grantor: principal, recipient: principal}
    {
        grant-timestamp: uint,
        revocation-timestamp: uint,
        currently-active: bool
    }
)

;; Stores user feedback on entries
(define-map entry-feedback
    {entry-id: uint, reviewer: principal}
    {
        score: uint,
        comment: (optional (string-ascii 256)),
        update-timestamp: uint,
        initial-timestamp: uint
    }
)

;; Aggregates feedback statistics per entry
(define-map entry-feedback-aggregates
    {entry-id: uint}
    {
        feedback-count: uint,
        latest-feedback-timestamp: uint
    }
)

;; Stores audio compilations
(define-map audio-compilations
    {id: uint}
    {
        title: (string-ascii 64),
        description: (string-ascii 256),
        curator: principal,
        theme: (string-ascii 32),
        creation-timestamp: uint,
        modification-timestamp: uint,
        entry-count: uint,
        allows-contributions: bool
    }
)

;; Maps compilation contributors
(define-map compilation-contributors
    {compilation-id: uint, participant: principal}
    {
        is-contributor: bool,
        addition-timestamp: uint,
        is-curator: bool
    }
)

;; Records entries in compilations
(define-map compilation-contents
    {compilation-id: uint, entry-id: uint}
    {
        added-by: principal,
        addition-timestamp: uint
    }
)

;; ========================================================================
;; Section 7: Extended Helper Functions
;; ========================================================================

;; Retrieves latest collection ID for a user
(define-private (get-latest-collection-id (participant principal))
    (get latest-collection-id (default-to {latest-collection-id: u0} 
        (map-get? user-collection-tracker {participant: participant})))
)

;; Helper for entry ID manipulation in map operations
(define-private (wrap-entry-id (entry-id uint))
    {entry-id: entry-id}
)

;; Adds an entry to a compilation (used with map operations)
(define-private (add-to-compilation (entry-data {entry-id: uint}))
    (let
        ((entry-id (get entry-id entry-data)))
        (and 
            (entry-exists-in-registry entry-id)
            (map-insert compilation-contents
                {compilation-id: (var-get compilation-registry-counter), entry-id: entry-id}
                {
                    added-by: tx-sender,
                    addition-timestamp: block-height
                }
            )
        )
    )
)

;; ========================================================================
;; Section 8: User Collection & Access Management
;; ========================================================================

;; Adds an entry to a user's collection if they have access rights
(define-public (add-entry-to-collection 
        (collection-id uint)
        (entry-id uint)
    )
    (let
        ((collection-data (unwrap! (map-get? audio-collections {custodian: tx-sender, collection-id: collection-id}) ERROR-ENTRY-NOT-LOCATED))
         (entry-data (unwrap! (map-get? audio-registry {entry-id: entry-id}) ERROR-ENTRY-NOT-LOCATED))
         (user-access (default-to {has-access: false} (map-get? access-registry {entry-id: entry-id, participant: tx-sender}))))

        ;; Validation checks
        (asserts! (entry-exists-in-registry entry-id) ERROR-ENTRY-NOT-LOCATED)
        (asserts! (or 
                    (is-eq (get custodian entry-data) tx-sender)
                    (get has-access user-access)
                  ) 
                ERROR-PERMISSION-REJECTED)

        ;; Check for duplicate entry in collection
        (asserts! (is-none (map-get? collection-entries {collection-custodian: tx-sender, collection-id: collection-id, entry-id: entry-id})) 
                 ERROR-ENTRY-ALREADY-EXISTS)

        (ok true)
    )
)

;; Grants access rights to a specified user for an entry
(define-public (grant-entry-access 
        (entry-id uint)
        (recipient principal)
    )
    (let
        ((entry-data (unwrap! (map-get? audio-registry {entry-id: entry-id}) ERROR-ENTRY-NOT-LOCATED)))

        ;; Validation checks
        (asserts! (entry-exists-in-registry entry-id) ERROR-ENTRY-NOT-LOCATED)
        (asserts! (is-eq (get custodian entry-data) tx-sender) ERROR-INSUFFICIENT-PRIVILEGES)
        (asserts! (not (is-eq tx-sender recipient)) ERROR-INVALID-ENTRY-NAME)

        ;; Check if access is already granted
        (asserts! (is-none (map-get? access-registry {entry-id: entry-id, participant: recipient})) 
                 ERROR-ENTRY-ALREADY-EXISTS)

        ;; Grant access rights
        (map-insert access-registry
            {entry-id: entry-id, participant: recipient}
            {has-access: true}
        )

        ;; Record the access grant history
        (map-insert access-grant-history
            {entry-id: entry-id, grantor: tx-sender, recipient: recipient}
            {
                grant-timestamp: block-height,
                revocation-timestamp: u0,
                currently-active: true
            }
        )

        (ok true)
    )
)

;; Revokes previously granted access rights
(define-public (revoke-entry-access 
        (entry-id uint)
        (recipient principal)
    )
    (let
        ((entry-data (unwrap! (map-get? audio-registry {entry-id: entry-id}) ERROR-ENTRY-NOT-LOCATED))
         (access-data (unwrap! (map-get? access-grant-history {entry-id: entry-id, grantor: tx-sender, recipient: recipient}) ERROR-ENTRY-NOT-LOCATED)))

        ;; Validation checks
        (asserts! (entry-exists-in-registry entry-id) ERROR-ENTRY-NOT-LOCATED)
        (asserts! (is-eq (get custodian entry-data) tx-sender) ERROR-INSUFFICIENT-PRIVILEGES)
        (asserts! (get currently-active access-data) ERROR-PERMISSION-REJECTED)

        (ok true)
    )
)

;; ========================================================================
;; Section 9: Feedback & Compilation Management
;; ========================================================================

;; Allows users to provide feedback on entries they have access to
(define-public (submit-entry-feedback 
        (entry-id uint)
        (score uint)
        (comment (optional (string-ascii 256)))
    )
    (let
        ((entry-data (unwrap! (map-get? audio-registry {entry-id: entry-id}) ERROR-ENTRY-NOT-LOCATED))
         (user-access (default-to {has-access: false} (map-get? access-registry {entry-id: entry-id, participant: tx-sender})))
         (existing-feedback (map-get? entry-feedback {entry-id: entry-id, reviewer: tx-sender})))

        ;; Validation checks
        (asserts! (entry-exists-in-registry entry-id) ERROR-ENTRY-NOT-LOCATED)
        (asserts! (or 
                    (is-eq (get custodian entry-data) tx-sender)
                    (get has-access user-access)
                  ) 
                ERROR-PERMISSION-REJECTED)
        (asserts! (and (>= score u1) (<= score u5)) ERROR-INVALID-ENTRY-NAME)

        ;; Validate comment length if provided
        (if (is-some comment)
            (asserts! (and 
                        (> (len (default-to "" comment)) u0) 
                        (< (len (default-to "" comment)) u257)
                      ) 
                    ERROR-INVALID-ENTRY-NAME)
            true
        )

        ;; Store or update the feedback
        (if (is-some existing-feedback)
            ;; Update existing feedback
            (map-set entry-feedback
                {entry-id: entry-id, reviewer: tx-sender}
                {
                    score: score,
                    comment: comment,
                    update-timestamp: block-height,
                    initial-timestamp: (get initial-timestamp (unwrap! existing-feedback ERROR-ENTRY-NOT-LOCATED))
                }
            )
            ;; Create new feedback
            (map-insert entry-feedback
                {entry-id: entry-id, reviewer: tx-sender}
                {
                    score: score,
                    comment: comment,
                    update-timestamp: block-height,
                    initial-timestamp: block-height
                }
            )
        )

        ;; Update feedback aggregates
        (match (map-get? entry-feedback-aggregates {entry-id: entry-id})
            existing-stats (map-set entry-feedback-aggregates
                {entry-id: entry-id}
                (merge existing-stats {
                    feedback-count: (if (is-some existing-feedback) 
                                      (get feedback-count existing-stats) 
                                      (+ (get feedback-count existing-stats) u1)),
                    latest-feedback-timestamp: block-height
                })
            )
            (map-insert entry-feedback-aggregates
                {entry-id: entry-id}
                {
                    feedback-count: u1,
                    latest-feedback-timestamp: block-height
                }
            )
        )

        (ok true)
    )
)

;; Creates a thematic compilation of audio entries
(define-public (create-audio-compilation
        (compilation-title (string-ascii 64))
        (compilation-description (string-ascii 256))
        (theme (string-ascii 32))
        (initial-entries (list 20 uint))
        (allows-contributions bool)
    )
    (let
        ((new-compilation-id (+ (var-get compilation-registry-counter) u1))
         (valid-entries (filter entry-exists-in-registry initial-entries)))

        ;; Validation checks
        (asserts! (and (> (len compilation-title) u0) (< (len compilation-title) u65)) ERROR-INVALID-ENTRY-NAME)
        (asserts! (and (> (len compilation-description) u0) (< (len compilation-description) u257)) ERROR-INVALID-ENTRY-NAME)
        (asserts! (and (> (len theme) u0) (< (len theme) u33)) ERROR-INVALID-ENTRY-NAME)

        ;; Set curator as a contributor
        (map-insert compilation-contributors
            {compilation-id: new-compilation-id, participant: tx-sender}
            {
                is-contributor: true,
                addition-timestamp: block-height,
                is-curator: true
            }
        )

        ;; Add all valid initial entries to the compilation
        (map add-to-compilation (map wrap-entry-id valid-entries))

        ;; Update compilation counter
        (var-set compilation-registry-counter new-compilation-id)

        (ok new-compilation-id)
    )
)

