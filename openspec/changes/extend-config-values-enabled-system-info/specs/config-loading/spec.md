## ADDED Requirements

### Requirement: Function-valued arguments in any component position

Any constructor argument of a `layouts`/`filters`/`appenders` component definition SHALL accept the `{ ref: <name> }` and `{ code: "<R source>" }`
function-value forms described for filters/`exception_handler`, not only a
logger's `exception_handler` or a filter entry. Resolution SHALL follow the same
lookup rules (`<pkg>::<name>` supported for `ref:`) and the same safety gate
(`code:` requires `allow_code = TRUE`). A bare `character` scalar in a regular
argument position SHALL continue to be treated as a literal string value, not
a function reference — only the explicit `{ ref: ... }`/`{ code: ... }`
mapping form opts an argument into function resolution.

#### Scenario: Layout argument resolved via ref

- **WHEN** a `layouts` component definition sets `transform_event: { ref: "myPkg::my_transform" }`
- **THEN** the resolved layout's `transform_event` argument SHALL be the
  function `myPkg::my_transform`

#### Scenario: Appender argument resolved via inline code with allow_code

- **WHEN** an appender's `index` argument is set to `{ code: "function() Sys.Date()" }`
  and the config is loaded with `allow_code = TRUE`
- **THEN** the resolved appender SHALL receive the evaluated function as its
  `index` argument

#### Scenario: Inline code in a non-filter argument rejected by default

- **WHEN** an appender argument uses the `code:` form and the config is loaded
  without `allow_code`
- **THEN** loading SHALL fail with an error naming the offending key, its path,
  and the `allow_code = TRUE` opt-in, and no logger SHALL be modified

#### Scenario: Bare string argument stays a literal

- **WHEN** an appender argument is a plain string that happens to match the
  name of a loaded function
- **THEN** the resolved appender SHALL receive the literal string, not the
  function

### Requirement: Component and logger enable/disable toggle

Component definitions under `layouts`/`filters`/`appenders`, and logger definitions under `loggers`, SHALL accept an optional `enabled` key (a
boolean, default `TRUE`). When `enabled: false` on a named component
definition, the component SHALL NOT be instantiated, and any `appenders:`/
`filters:` list entry elsewhere in the config that references it by name
SHALL be silently dropped rather than treated as a validation error. When
`enabled: false` on an inline (unnamed) component definition appearing inside
an `appenders:`/`filters:` list, that entry SHALL be dropped from the
resulting list. When `enabled: false` on a logger definition, `apply_config()`
SHALL fully reset that logger to its default/unconfigured state — no custom
threshold, no appenders, no filters, `propagate = TRUE`, default
`exception_handler` — the same end state as if the logger had never been
configured, regardless of whatever state it was in before this config was
applied. This makes application idempotent: applying the same config with
`enabled: false` for a given logger always yields the same result,
independent of prior state.

#### Scenario: Disabled named appender is dropped from a referencing logger

- **WHEN** `appenders.metrics` has `enabled: false` and logger `app` lists
  `appenders: [console, metrics]`
- **THEN** the resolved `app` logger SHALL have only the `console` appender
  installed, and loading SHALL NOT fail due to the `metrics` reference

#### Scenario: Disabled inline filter dropped from its list

- **WHEN** a logger's `filters:` list contains one inline filter definition
  with `enabled: false` alongside one enabled inline filter
- **THEN** the resolved logger SHALL have only the enabled filter installed

#### Scenario: Disabled logger definition is fully reset

- **WHEN** a config names logger `app` with `enabled: false`, and `app` was
  previously configured with a threshold of `debug` and a `console` appender
- **THEN** after applying the config, logger `app` SHALL have no threshold of
  its own, no appenders, and no filters — the same state as an unconfigured
  logger — not its previous `debug` threshold or appender

#### Scenario: Re-applying a config with a disabled logger is idempotent

- **WHEN** a config naming logger `app` with `enabled: false` is applied twice
  in a row (with unrelated state changes to `app` attempted in between)
- **THEN** logger `app` SHALL be in its default/unconfigured state after each
  application, regardless of what happened in between

#### Scenario: Toggling via environment interpolation

- **WHEN** an appender definition sets `enabled: ${METRICS_ENABLED:-false}`
  and the environment variable is unset
- **THEN** the appender SHALL be treated as disabled
