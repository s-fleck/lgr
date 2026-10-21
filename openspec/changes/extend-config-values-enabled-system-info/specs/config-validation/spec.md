## ADDED Requirements

### Requirement: Validation of generalized function-value positions

Validation SHALL apply the existing function-value rules (used for filters and
`exception_handler`) to `{ ref: ... }`/`{ code: ... }` values appearing in any
component constructor argument. An argument using `{ code: ... }` SHALL be
rejected unless the loader is called with `allow_code = TRUE`, reported with
the same path-addressed error format as other config errors. An argument using
`{ ref: ... }` whose `ref` value is not a `character` scalar SHALL be rejected.

#### Scenario: Non-scalar ref rejected

- **WHEN** a layout argument is `{ ref: ["a", "b"] }`
- **THEN** validation SHALL fail reporting that `ref` must be a character
  scalar, at the argument's path

#### Scenario: Code form in an argument gated by allow_code

- **WHEN** an appender argument uses `{ code: "function() 1" }` and validation
  runs without `allow_code`
- **THEN** validation SHALL fail with an error at that argument's path
  explaining that inline code is disabled by default

### Requirement: Validation of the enabled key

Validation SHALL reject a non-boolean value for `enabled` on any component
definition (`layouts`/`filters`/`appenders`) or logger definition, reporting
the path of the offending key. `enabled` SHALL be recognized as a valid key
alongside the existing keys for each definition kind (it SHALL NOT trigger an
"unknown key" error).

#### Scenario: Non-boolean enabled rejected

- **WHEN** an appender definition sets `enabled: "sure"`
- **THEN** validation SHALL fail reporting that a boolean was expected at that
  appender's `enabled` path

#### Scenario: enabled recognized as a valid logger key

- **WHEN** a logger definition sets `enabled: false` alongside `threshold`
- **THEN** validation SHALL succeed without an "unknown key" error for `enabled`
