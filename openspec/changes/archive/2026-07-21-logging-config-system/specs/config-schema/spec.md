## ADDED Requirements

### Requirement: Canonical top-level structure

The configuration format SHALL be a single canonical schema. A config document
SHALL be a mapping with an optional integer `version` field (defaulting to `1`
when omitted) and the following optional top-level sections: `layouts`,
`filters`, `appenders`, `loggers`, and global options (`disable_existing_loggers`).
Any other top-level key SHALL be rejected as invalid (see `config-validation`).

#### Scenario: Minimal config with only a root logger

- **WHEN** a config contains only `loggers: { root: { threshold: info } }`
- **THEN** it SHALL be accepted, `version` SHALL default to `1`, and the root
  logger's threshold SHALL be set to `info`

#### Scenario: Unknown top-level key is rejected

- **WHEN** a config contains a top-level key `appdenders`
- **THEN** loading SHALL fail with a validation error naming the offending key
  and SHALL NOT modify any logger

#### Scenario: Explicit unsupported version is rejected

- **WHEN** a config declares `version: 2`
- **THEN** loading SHALL fail with an error stating that only version `1` is
  supported

### Requirement: Named reusable components

The `layouts`, `filters`, and `appenders` sections SHALL each be a mapping from
a user-chosen component name to a component definition. A component definition
SHALL specify the R6 class to instantiate via a `class` key (e.g.
`class: AppenderConsole`) with the remaining keys supplied as constructor
arguments. Component names SHALL be usable as references elsewhere in the config.

#### Scenario: Named layout referenced by an appender

- **WHEN** `layouts` defines `text: { class: LayoutFormat, fmt: "%L %m" }` and
  an appender sets `layout: text`
- **THEN** the appender SHALL be constructed with the `LayoutFormat` instance
  defined under `text`

#### Scenario: Named appender referenced by multiple loggers

- **WHEN** two loggers both list the same named appender in their `appenders`
- **THEN** both loggers SHALL receive the same single constructed appender
  instance (definitions are resolved once and shared)

#### Scenario: Reference to an undefined component fails

- **WHEN** a logger references an appender name that is not defined in
  `appenders`
- **THEN** loading SHALL fail with an error naming the missing reference and its
  location

### Requirement: Logger hierarchy configuration

The `loggers` section SHALL be a mapping from logger name to logger
configuration. The key `root` SHALL configure the root logger. Slash-separated
names (e.g. `app/db`) SHALL configure descendant loggers. Each logger
configuration SHALL support `threshold`, `appenders`, `filters`, `propagate`,
and `exception_handler`. Appenders and filters on a logger SHALL be specifiable
either as references to named components or as inline definitions.

#### Scenario: Configuring a descendant logger

- **WHEN** `loggers` contains `app/db: { threshold: warn, propagate: false }`
- **THEN** the logger named `app/db` SHALL have threshold `warn` and propagate
  `false`

#### Scenario: Inline appender definition on a logger

- **WHEN** a logger's `appenders` contains an inline object with
  `class: AppenderConsole` rather than a name reference
- **THEN** that appender SHALL be constructed and attached to that logger

#### Scenario: Mixed references and inline definitions

- **WHEN** a logger's `appenders` list mixes a named reference and an inline
  definition
- **THEN** both SHALL be resolved and attached in the given order

### Requirement: Full feature coverage

The schema SHALL be able to express every configurable lgr construct: all
appender classes and their constructor parameters (including console, file,
JSON, rotating-file, and buffer/memory appenders), all layout classes, both
R6 `EventFilter` objects and predicate-function filters, per-logger and
per-appender `threshold`, `propagate`, `exception_handler` code, and
`LoggerGlue` loggers.

#### Scenario: Rotating file appender with parameters

- **WHEN** an appender is defined with `class: AppenderFileRotating` and
  rotation parameters (e.g. `size`, `max_backups`)
- **THEN** the appender SHALL be constructed with those parameters passed to its
  constructor

#### Scenario: Function-based filter

- **WHEN** a filter is defined with the inline `code:` form evaluating to a
  function of one argument `event`, with code evaluation enabled
  (`allow_code = TRUE`)
- **THEN** the resulting predicate function SHALL be attached as a filter

#### Scenario: Exception handler as code

- **WHEN** a logger defines `exception_handler` with the inline `code:` form,
  with code evaluation enabled (`allow_code = TRUE`)
- **THEN** the evaluated function SHALL be installed as that logger's exception
  handler

#### Scenario: Glue logger

- **WHEN** a logger definition selects a `LoggerGlue` logger class
- **THEN** the configured logger SHALL be a `LoggerGlue` instance

### Requirement: Function-valued keys

Function-valued positions SHALL accept a small set of explicit, tagged value
forms so intent is unambiguous and executable source is always clearly marked.
This applies wherever the config expects an R function — a logger's
`exception_handler` and any filter entry. The
accepted forms SHALL be: a **name reference** to an existing function (a bare
string in `exception_handler`, or an explicit `{ ref: <name> }` mapping),
resolved by looking up `<name>` or `<pkg>::<name>`; an **inline code** form
(`{ code: "<R source>" }`) whose value is R source parsed and evaluated to a
function; and, for filters only, a **class** form (`{ class: <FilterClass>, ... }`)
constructing an R6 `EventFilter`, plus a bare-string **named-component
reference** to an entry in the top-level `filters` section. Only the `code:`
form SHALL evaluate config-supplied R source; every other form SHALL be treated
as data and is therefore permitted in safe mode (gating is defined in
`config-loading`). When `class:` is present the value SHALL be treated as a
class spec, so a nested `code` key is an ordinary constructor argument, not
inline code.

#### Scenario: Exception handler as a name reference

- **WHEN** a logger sets `exception_handler: lgr::default_exception_handler`
- **THEN** the named function SHALL be looked up (without evaluating any
  config-supplied source) and installed as that logger's exception handler

#### Scenario: Name reference to a missing function fails

- **WHEN** `exception_handler` names a function that cannot be resolved
- **THEN** loading SHALL fail with an error naming the unresolved reference and
  its path

#### Scenario: Filter as a name reference via ref

- **WHEN** a logger's filter entry is `{ ref: mypkg::my_predicate }`
- **THEN** the named function SHALL be resolved by lookup and attached as a
  filter, distinct from a bare-string entry (which is a named-component
  reference)

#### Scenario: Inline code uses the code form

- **WHEN** a function-valued key uses `{ code: "<R source>" }`
- **THEN** the value SHALL be classified as inline code (evaluation gated per
  `config-loading`), and no other form SHALL be interpreted as inline code

#### Scenario: class takes precedence over a nested code argument

- **WHEN** a filter entry is `{ class: SomeFilter, code: "x" }`
- **THEN** it SHALL be treated as a `class:` spec with `code` passed as an
  ordinary constructor argument, not as inline code

### Requirement: Extension package components

A component's `class` SHALL support namespaced identifiers of the form
`pkg::ClassName` so that appenders, layouts, and filters provided by extension
packages (such as `lgrExtra`) can be configured.

#### Scenario: Namespaced appender class

- **WHEN** an appender is defined with `class: lgrExtra::AppenderSyslog` and the
  `lgrExtra` package is installed
- **THEN** the appender SHALL be constructed from that package's class

#### Scenario: Namespaced class from a missing package

- **WHEN** an appender references `pkg::AppenderX` and `pkg` is not installed
- **THEN** loading SHALL fail with an error naming the required package
