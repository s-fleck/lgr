## Why

lgr is a business-grade logging framework, but it has no first-class,
documented way to configure a logger hierarchy from a file. What exists today
is an unexported, undocumented, and internally inconsistent set of helpers:
two competing config "dialects" (the simple `Logger:` shorthand that only
configures a single logger, and a more capable but hidden `logging_config`
that only works on the root logger), no schema, thin validation with unhelpful
errors, and no way for editors to assist authors. Professional logging
libraries (log4j2, NLog, Python `logging.config`, Serilog) all treat a
declarative config file as a primary, well-specified interface. lgr should
too: teams need to move logging behaviour out of code and into reviewable,
environment-specific config files without learning lgr's R6 internals.

## What Changes

- Introduce a **single canonical declarative config schema** (version `1`) that
  can configure the entire logger hierarchy — the root logger, named
  descendant loggers, and reusable named `appenders`, `layouts`, and
  `filters` with cross-references between them.
- **BREAKING**: Remove the legacy simple `Logger:` shorthand format and the
  `logger_config()` / `as_logger_config()` single-logger dialect. There is now
  exactly one config format. (Programmatic single-logger configuration via a
  plain `list` passed to `Logger$config()` remains, but the *file* format is
  unified.)
- Load config from **YAML or JSON**, supplied as a file path or an in-memory
  string, with reliable format detection, applied via a documented public API.
- **Environment-variable / value interpolation**: `${VAR}` and
  `${VAR:-default}` substitution in config values (paths, thresholds, etc.).
- **Strict schema validation** performed before anything is applied, with
  precise, path-pointing error messages ("`loggers.app.appenders[2]` — unknown
  appender reference 'consle'") and did-you-mean hints. Application is
  all-or-nothing: an invalid config never leaves the logger tree half-configured.
- **Safe by default**: loading a config SHALL NOT evaluate arbitrary R source
  code unless explicitly permitted. Code-valued keys (inline
  `exception_handler` bodies and inline function-based filters) are rejected in
  the default safe mode; evaluating them requires an explicit opt-in
  (`allow_code = TRUE`). Declarative class-based components remain fully
  available in safe mode.
- **Full feature coverage**: every lgr construct is configurable — all
  appender types and their parameters (console, file, JSON, rotating, buffer,
  memory), all layouts, both R6 `EventFilter`s and predicate-function filters,
  `exception_handler` code, `propagate`, per-logger and per-appender
  `threshold`s, and `LoggerGlue` loggers.
- Ship a **machine-readable JSON Schema** for lgr config files so editors
  (VS Code, etc.) provide autocompletion and inline validation.
- Make the config API **public, exported, and documented**: reference docs plus
  a dedicated configuration vignette with a complete format reference and
  worked examples.

## Capabilities

### New Capabilities

- `config-schema`: The canonical declarative configuration format — top-level
  structure (`version`, global options, `layouts`, `filters`, `appenders`,
  `loggers`), named reusable components and references between them, and the
  mapping from each config construct to the corresponding lgr object
  (appenders and all their parameters, layouts, R6 and function filters,
  exception handlers, thresholds, propagation, glue loggers).
- `config-loading`: Runtime loading and application behaviour — the public API
  entry points, reading YAML/JSON from a file path or string, format
  detection, building/updating the logger hierarchy, `disable_existing_loggers`
  semantics, safe-by-default code-evaluation policy (with `allow_code` opt-in),
  and atomic (all-or-nothing) application.
- `config-interpolation`: Value interpolation rules — `${VAR}` and
  `${VAR:-default}` environment-variable substitution, escaping, and where in
  the config interpolation is (and is not) applied.
- `config-validation`: Strict pre-application validation with precise,
  path-addressed error messages and suggestions, plus the shipped JSON Schema
  used both internally and by external editors.

### Modified Capabilities

<!-- No pre-existing OpenSpec specs; the legacy R config code is replaced, not an existing spec. -->

## Impact

- **New/rewritten code** (replacing `R/logger_config.R`): a config reader
  (format detection + YAML/JSON parsing), an interpolation pass, a schema
  validator, a component resolver (named components, references, R6
  constructor instantiation, cycle detection — the one solid part of the
  current implementation, retained and hardened), and hierarchy application.
- **Public API surface**: new exported, documented functions for loading and
  applying configs; `Logger$config()` continues to accept the new file format
  and plain lists. Removal of exported `as_logger_config()` and the
  `logger_config` constructor is **BREAKING** for the (small, pre-1.0) set of
  users relying on them.
- **Dependencies**: `yaml` and `jsonlite` remain Suggests (config is opt-in);
  no new hard dependencies.
- **Docs & assets**: new configuration vignette, expanded reference
  documentation, a JSON Schema file shipped under `inst/`, and refreshed
  example configs under `inst/configs/`.
- **Tests**: the existing `test_config_*` suite is superseded by tests aligned
  to the unified schema, validation errors, and interpolation.
- **User migration**: existing `Logger:`-shorthand and legacy `logging_config`
  files must be migrated to the unified schema; the vignette will include a
  migration section. This is acceptable pre-1.0 and on the `0.6.0` branch.
