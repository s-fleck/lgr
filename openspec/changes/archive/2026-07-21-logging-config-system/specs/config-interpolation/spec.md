## ADDED Requirements

### Requirement: Environment variable substitution

Config values SHALL support environment-variable interpolation using the syntax
`${VAR}`. During loading, each `${VAR}` occurrence in a scalar string value
SHALL be replaced with the value of environment variable `VAR`. Interpolation
SHALL occur after parsing and before validation, so that interpolated values
are validated like literal ones.

#### Scenario: Simple substitution

- **WHEN** the environment variable `LOG_DIR` is `/var/log/app` and a config
  contains `file: ${LOG_DIR}/app.jsonl`
- **THEN** the resolved value SHALL be `/var/log/app/app.jsonl`

#### Scenario: Substitution in a threshold

- **WHEN** `LOG_LEVEL` is `debug` and a logger sets `threshold: ${LOG_LEVEL}`
- **THEN** the threshold SHALL resolve to `debug`

### Requirement: Type coercion of interpolated values

Interpolated values SHALL be leniently coerced so that a value produced by
interpolation behaves like the equivalent literal, because environment
variables are always strings but interpolation runs after parsing. When an
entire scalar value is produced by interpolation (`${VAR}` or `${VAR:-default}`
with no surrounding literal text), the resulting string SHALL be coerced as
follows: a number-like string SHALL become a number, a boolean-like string
(`true`/`false`, case-insensitive) SHALL become a logical, and any other string
SHALL remain a string. When the schema declares an expected type for the target
field, coercion SHALL target that type. When a `${VAR}` reference is only part
of a larger string (e.g. `${DIR}/app.log`), the result SHALL remain a string
with no numeric/boolean coercion. Coercion SHALL occur before validation so that
coerced values are validated and applied like literals.

#### Scenario: Number-like value coerced to a number

- **WHEN** `SIZE` is `1048576` and an appender sets `size: ${SIZE}`
- **THEN** the resolved value SHALL be the number `1048576`, identical to a
  literal `size: 1048576`

#### Scenario: Boolean-like value coerced to a logical

- **WHEN** `PROPAGATE` is `false` and a logger sets `propagate: ${PROPAGATE}`
- **THEN** the resolved value SHALL be the logical `FALSE`, identical to a
  literal `propagate: false`

#### Scenario: Non-numeric value remains a string

- **WHEN** `LOG_LEVEL` is `debug` and a value is exactly `${LOG_LEVEL}`
- **THEN** the resolved value SHALL be the string `debug`, treated identically
  to a literal `debug`

#### Scenario: Partial interpolation is not numerically coerced

- **WHEN** `PORT` is `8080` and a value is `host:${PORT}`
- **THEN** the resolved value SHALL be the string `host:8080` and SHALL NOT be
  coerced to a number

### Requirement: Inline code payloads are not interpolated

Interpolation SHALL NOT be applied to the string value of an inline `code:` key
(see `config-schema`, "Function-valued keys"). Interpolating environment values
into R source would be a code-injection vector and is disallowed; a `code:`
payload SHALL be passed through to evaluation (when `allow_code = TRUE`)
verbatim.

#### Scenario: Code payload left untouched by interpolation

- **WHEN** a `code:` value contains the text `${HOME}`
- **THEN** the `${HOME}` SHALL remain literal in the R source and SHALL NOT be
  replaced with an environment value

### Requirement: Default values for interpolation

The interpolation syntax SHALL support inline defaults using
`${VAR:-default}`. When `VAR` is unset or empty, the default SHALL be used.
When `VAR` is set to a non-empty value, that value SHALL be used.

#### Scenario: Default used when unset

- **WHEN** `LOG_DIR` is unset and a config contains `file: ${LOG_DIR:-/tmp}/a.log`
- **THEN** the resolved value SHALL be `/tmp/a.log`

#### Scenario: Value used when set

- **WHEN** `LOG_DIR` is `/data` and a config contains `${LOG_DIR:-/tmp}/a.log`
- **THEN** the resolved value SHALL be `/data/a.log`

### Requirement: Missing variable handling

When a `${VAR}` reference has no default and `VAR` is unset, loading SHALL fail
with a clear error naming the missing variable and the config path where it was
referenced, rather than silently substituting an empty string.

#### Scenario: Unset variable without default errors

- **WHEN** a config references `${LOG_DIR}` with no default and `LOG_DIR` is
  unset
- **THEN** loading SHALL fail with an error naming `LOG_DIR` and its location

### Requirement: Escaping literal dollar-brace sequences

The interpolation pass SHALL provide a way to include a literal `${...}`
sequence without substitution, via the escape `$${...}`. An escaped sequence
SHALL be emitted as a single literal `${...}` with no interpolation.

#### Scenario: Escaped sequence is left literal

- **WHEN** a config value contains `$${NOT_A_VAR}`
- **THEN** the resolved value SHALL be the literal `${NOT_A_VAR}` and no
  environment lookup SHALL occur
