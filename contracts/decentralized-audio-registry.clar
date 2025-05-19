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
