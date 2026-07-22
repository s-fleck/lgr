# config-loading

## Purpose

Defines how lgr configurations are loaded and applied to the live logger
hierarchy: the public API surface, supported input formats and parser
selection, how configs update the hierarchy (including
`disable_existing_loggers`), the safe-by-default code-evaluation gate, and the
atomicity guarantee for applying a config.

## Requirements

### Requirement: Public config loading API

lgr SHALL provide a documented, exported API for loading a configuration and
applying it to the logger hierarchy. `Logger$config()` SHALL accept a
configuration supplied as a file path, an in-memory YAML/JSON string, or a
plain R `list` following the schema. Applying a config SHALL return the target
logger invisibly to support chaining.

#### Scenario: Apply config from a file path

- **WHEN** `lgr$config("path/to/lgr.yaml")` is called with a valid config file
- **THEN** the logger hierarchy SHALL be configured accordingly and the root
  logger SHALL be returned invisibly

#### Scenario: Apply config from a list

- **WHEN** `lgr$config(list(loggers = list(root = list(threshold = "warn"))))`
  is called
- **THEN** the root logger threshold SHALL be set to `warn`

#### Scenario: Reset with NULL

- **WHEN** `lgr$config(NULL)` is called
- **THEN** the target logger SHALL be reset to its default unconfigured state

### Requirement: YAML and JSON support with format detection

Config loading SHALL accept both YAML and JSON. When given a file path, the
format SHALL be detected by extension (`.json` → JSON; `.yaml`/`.yml` → YAML),
falling back to content sniffing when the extension is absent or ambiguous.
When given a string, the format SHALL be detected from its content. Parser
selection SHALL follow a defined precedence: JSON input SHALL be parsed with
`jsonlite` when it is installed, otherwise with `yaml` (JSON being valid YAML);
YAML input SHALL be parsed with `yaml`. The relevant parser package (`yaml`
and/or `jsonlite`) SHALL be an optional dependency, and a clear error SHALL be
raised if a required parser is not installed.

#### Scenario: Load a .json config

- **WHEN** a config is loaded from a file with a `.json` extension
- **THEN** it SHALL be parsed as JSON and applied

#### Scenario: Load a YAML string

- **WHEN** a multi-line YAML string is passed to `config()`
- **THEN** it SHALL be parsed as YAML and applied

#### Scenario: Missing parser package

- **WHEN** a YAML config is loaded but the `yaml` package is not installed
- **THEN** an informative error SHALL name the missing package

### Requirement: Hierarchy application and disable_existing_loggers

Applying a full config SHALL create or update every logger named in `loggers`
and SHALL leave loggers not mentioned in the config untouched. When
`disable_existing_loggers: true` is set, all pre-existing non-root loggers SHALL
be reset to their default state before the config is applied; the loggers named
in the config are then (re)configured, so the net effect is that only
non-root loggers absent from the config are left in the reset state. A full
multi-logger config SHALL only be applicable via the root logger.

#### Scenario: Existing loggers preserved by default

- **WHEN** a config is applied that does not mention an already-configured
  logger `other`
- **THEN** logger `other` SHALL retain its previous configuration

#### Scenario: disable_existing_loggers resets unlisted loggers

- **WHEN** a config with `disable_existing_loggers: true` is applied and a
  pre-existing logger `other` is not named in the config
- **THEN** logger `other` SHALL be reset to its default state

#### Scenario: disable_existing_loggers still configures listed loggers

- **WHEN** a config with `disable_existing_loggers: true` names logger `app`
- **THEN** logger `app` SHALL end in the state described by the config, not the
  reset state

#### Scenario: Full config rejected on a non-root logger

- **WHEN** a multi-logger config is applied via a non-root logger's `config()`
- **THEN** loading SHALL fail with an error directing the user to apply it via
  the root logger

### Requirement: Safe-by-default code evaluation

Config loading SHALL NOT evaluate config-supplied R source code by default. The
loading entry points (including `Logger$config()`) SHALL accept an `allow_code`
argument that defaults to `FALSE` (safe mode). The *only* form that evaluates
config-supplied source is the inline `code:` form (see `config-schema`,
"Function-valued keys"); in safe mode any value using the `code:` form SHALL be
rejected with a clear error naming the offending key, its path, and how to opt
in. All other function-valued forms — name references, `{ ref: ... }`,
`{ class: ... }` specs, and named-component references — do NOT evaluate
config-supplied source and SHALL remain available in safe mode. An actual R
`function` object supplied in a programmatic `list` config is a live object, not
source, and SHALL be accepted regardless of `allow_code`. When
`allow_code = TRUE`, `code:` values SHALL be evaluated.

#### Scenario: Default mode rejects an inline-code exception handler

- **WHEN** a config with an `exception_handler` using the `code:` form is loaded
  without specifying `allow_code`
- **THEN** loading SHALL fail with an error naming the `exception_handler` key,
  its path, and the `allow_code = TRUE` opt-in, and SHALL NOT modify any logger

#### Scenario: Default mode rejects an inline-code filter

- **WHEN** a config defines a filter using the `code:` form and is loaded
  without `allow_code`
- **THEN** loading SHALL fail with an error naming the offending filter and the
  opt-in

#### Scenario: Non-code function forms allowed in safe mode

- **WHEN** a safe-mode config uses only name references, `{ ref: ... }`,
  `{ class: ... }` specs (e.g. `class: FilterForceLevel`), and named-component
  references for its functions and components
- **THEN** it SHALL load successfully without requiring `allow_code`

#### Scenario: Live function in a list config allowed in safe mode

- **WHEN** a programmatic `list` config supplies an actual R `function` as an
  `exception_handler` and is applied without `allow_code`
- **THEN** it SHALL be accepted and installed, because no config-supplied source
  is evaluated

#### Scenario: Opt-in enables code evaluation

- **WHEN** the same config with a `code:`-form `exception_handler` is loaded
  with `allow_code = TRUE`
- **THEN** the handler SHALL be evaluated and installed

### Requirement: Atomic application

A configuration SHALL be fully parsed, interpolated, validated, and resolved
into objects BEFORE any change is made to the live logger hierarchy. If any
step fails, no logger SHALL be modified.

#### Scenario: Invalid config leaves hierarchy unchanged

- **WHEN** a config is applied that is valid except for one undefined appender
  reference
- **THEN** loading SHALL fail and every logger SHALL retain the exact state it
  had before the call

#### Scenario: Constructor error leaves hierarchy unchanged

- **WHEN** a component definition passes validation but its R6 constructor
  throws during instantiation
- **THEN** the error SHALL be surfaced and no logger SHALL have been modified
