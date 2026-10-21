## MODIFIED Requirements

### Requirement: Shipped JSON Schema for editors

The package SHALL ship a machine-readable JSON Schema describing the config
format, installed under `inst/` so it is available in the installed package.
The schema SHALL be usable by external editors for autocompletion and inline
validation, and its constraints SHALL be consistent with the runtime
validator, including the generalized function-value form (`{ ref: ... }` /
`{ code: ... }`) accepted in any component constructor argument, and the
optional `enabled` boolean key on component and logger definitions.

#### Scenario: JSON Schema is installed and locatable

- **WHEN** the package is installed and the schema path is requested via
  `system.file()`
- **THEN** a non-empty JSON Schema file SHALL be returned

#### Scenario: Example configs validate against the schema

- **WHEN** the bundled example configs under `inst/configs/` are checked against
  the shipped JSON Schema
- **THEN** they SHALL validate successfully

#### Scenario: enabled and ref/code forms accepted in arbitrary arguments

- **WHEN** a component definition in an example config sets `enabled: false`
  or an arbitrary argument to `{ ref: "pkg::fun" }`
- **THEN** the shipped JSON Schema SHALL accept it without a schema violation
