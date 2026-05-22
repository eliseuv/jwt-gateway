# Revision history for jwt-gateway

## 0.1.0.0 -- 2026-05-21

* Refactor token domain to use a type-level state machine.
* Consolidate verification logic into Domain.Token.
* Implement token expiration check.
* Improve JWT parser to extract components directly into RawToken.
* Add comprehensive unit tests for token parsing and verification.
