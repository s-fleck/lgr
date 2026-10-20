## ADDED Requirements

### Requirement: FilterInjectSystemInfo built-in metadata bundle

lgr SHALL provide an exported `FilterInjectSystemInfo` R6 class, inheriting from `FilterInject`, that computes a fixed bundle of standard runtime metadata once at construction time and injects it into every `LogEvent` it processes, using the same per-event injection mechanism as `FilterInject`. The bundle SHALL include `hostName` and `rVersion` unconditionally; `containerId` only when running inside a Docker container; and `scriptFile` only when the current script's path can be determined. A field that cannot be determined (e.g. `scriptFile` outside of RStudio/`Rscript`/`source()`, or `containerId` outside Docker) SHALL be omitted from the injected values entirely rather than set to an empty or `NA` placeholder.

#### Scenario: Default fields injected

- **WHEN** `FilterInjectSystemInfo$new()` is constructed and attached to a Logger, and an event is logged
- **THEN** the resulting event SHALL contain `hostName` and `rVersion` fields with non-empty values

#### Scenario: containerId present only in Docker

- **WHEN** `FilterInjectSystemInfo$new()` is constructed in an environment where `/.dockerenv` exists
- **THEN** the resulting event SHALL contain a `containerId` field

#### Scenario: containerId absent outside Docker

- **WHEN** `FilterInjectSystemInfo$new()` is constructed in an environment where `/.dockerenv` does not exist
- **THEN** the resulting event SHALL NOT contain a `containerId` field

#### Scenario: scriptFile omitted when undeterminable

- **WHEN** `FilterInjectSystemInfo$new()` is constructed in a context where the running script's path cannot be determined
- **THEN** the resulting event SHALL NOT contain a `scriptFile` field, and construction SHALL NOT error

### Requirement: Custom values extend or override the bundle

`FilterInjectSystemInfo$new()` SHALL accept the same `...`/`.list` named-value arguments as `FilterInject$new()`. A named value supplied this way SHALL be injected in addition to the computed bundle; if its name matches one of the computed fields (`hostName`, `rVersion`, `containerId`, `scriptFile`), the supplied value SHALL override the computed one rather than causing a duplicate-name error.

#### Scenario: Extra custom value injected alongside the bundle

- **WHEN** `FilterInjectSystemInfo$new(environment = "prod")` is constructed and an event is logged
- **THEN** the resulting event SHALL contain both `environment = "prod"` and the computed `hostName`/`rVersion` fields

#### Scenario: Explicit value overrides a computed default

- **WHEN** `FilterInjectSystemInfo$new(hostName = "custom-host")` is constructed
- **THEN** the resulting event's `hostName` field SHALL be `"custom-host"`, not the value computed from `Sys.info()`

### Requirement: Usable from declarative config with no loader changes

`FilterInjectSystemInfo` SHALL be usable in a declarative `lgr` config purely through the existing `class:`-based component mechanism (`filters: { <name>: { class: FilterInjectSystemInfo, ... } }`), requiring no changes to the config loader, validator, or schema beyond what already supports any `EventFilter` subclass.

#### Scenario: Configured via a native lgr config

- **WHEN** a config defines `filters.systemInfo` with `class: FilterInjectSystemInfo` and lists `systemInfo` under a logger's `filters:`
- **THEN** applying the config SHALL attach a working `FilterInjectSystemInfo` instance to that logger, injecting the standard metadata bundle into events it processes

### Requirement: Graceful degradation without optional dependencies

`scriptFile` detection SHALL rely on the `rstudioapi` package only when available (`Suggests`, not `Imports`); its absence SHALL NOT cause an error, only a reduced ability to determine `scriptFile` from within RStudio.

#### Scenario: rstudioapi not installed

- **WHEN** `FilterInjectSystemInfo$new()` is constructed and the `rstudioapi` package is not installed
- **THEN** construction SHALL succeed, falling back to other script-path detection methods or omitting `scriptFile` if none apply
