# config-validation

## Purpose

Defines validation of lgr configs prior to application: strict schema
validation, precise path-addressed error messages with did-you-mean
suggestions, reference-integrity and cycle detection among named components,
and the shipped JSON Schema for external editor support.

## Requirements

### Requirement: Strict pre-application validation

Every config SHALL be validated against the canonical schema before it is
applied. Validation SHALL reject unknown keys, wrong value types, unsupported
`version` values, malformed component definitions, and references to undefined
components. A config that fails validation SHALL NOT modify the logger
hierarchy.

#### Scenario: Unknown key rejected

- **WHEN** a logger definition contains the key `propogate`
- **THEN** validation SHALL fail and report the unknown key

#### Scenario: Wrong value type rejected

- **WHEN** `disable_existing_loggers` is set to the string `"maybe"`
- **THEN** validation SHALL fail reporting that a boolean was expected

#### Scenario: Valid config passes

- **WHEN** a fully well-formed config is validated
- **THEN** validation SHALL succeed and the config SHALL be applied

### Requirement: Precise, path-addressed error messages

Validation errors SHALL identify the exact location of the problem using a
dotted/indexed path (e.g. `loggers.app.appenders[2]`) and SHALL describe what
was expected. When an error is likely a typo of a known key or a known
component name, the message SHALL include a did-you-mean suggestion.

#### Scenario: Path is reported for a nested error

- **WHEN** the third appender reference of logger `app` is undefined
- **THEN** the error message SHALL include the path `loggers.app.appenders[2]`
  and the offending reference value

#### Scenario: Did-you-mean suggestion for a typo

- **WHEN** a logger references appender `consle` while `console` is defined
- **THEN** the error SHALL suggest `console`

#### Scenario: Multiple errors surfaced together

- **WHEN** a config has more than one validation error
- **THEN** validation SHOULD report all detected errors rather than only the
  first

### Requirement: Reference integrity and cycle detection

Validation SHALL verify that every component reference resolves to a defined
component and SHALL detect cyclic references among named components, failing
with an error that names the cycle.

#### Scenario: Cyclic component references detected

- **WHEN** named components reference each other in a cycle
- **THEN** validation SHALL fail with an error naming the components in the cycle

#### Scenario: Dangling reference detected

- **WHEN** an appender references a layout name that is not defined
- **THEN** validation SHALL fail naming the missing layout and the referring
  appender

### Requirement: Shipped JSON Schema for editors

The package SHALL ship a machine-readable JSON Schema describing the config
format, installed under `inst/` so it is available in the installed package.
The schema SHALL be usable by external editors for autocompletion and inline
validation, and its constraints SHALL be consistent with the runtime validator.

#### Scenario: JSON Schema is installed and locatable

- **WHEN** the package is installed and the schema path is requested via
  `system.file()`
- **THEN** a non-empty JSON Schema file SHALL be returned

#### Scenario: Example configs validate against the schema

- **WHEN** the bundled example configs under `inst/configs/` are checked against
  the shipped JSON Schema
- **THEN** they SHALL validate successfully
