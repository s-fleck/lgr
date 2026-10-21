# Declarative Logger Configuration

lgr can be configured from a single canonical, versioned configuration
format (`version: 1`) that can express the *entire* logger hierarchy:
the root logger, descendant loggers, and reusable named `layouts`,
`filters` and `appenders`. Use
[Logger\$config()](https://s-fleck.github.io/lgr/reference/Logger.md)
(usually via `lgr$config()`) to apply such a config, or
[`read_config()`](https://s-fleck.github.io/lgr/reference/read_config.md)/[`validate_config()`](https://s-fleck.github.io/lgr/reference/read_config.md)
to check a config without applying it.

## Top-level structure

A config document is a mapping with an optional `version` (defaults to
`1`; no other value is currently supported), an optional
`disable_existing_loggers` flag, and the optional sections `layouts`,
`filters`, `appenders` and `loggers`. `loggers` is a mapping from logger
name (`root` for the root logger, `/`-separated names such as `app/db`
for descendants) to a logger definition supporting `class` (`"Logger"`
or `"LoggerGlue"`), `threshold`, `appenders`, `filters`, `propagate` and
`exception_handler`. `layouts`/`filters`/`appenders` are mappings from a
user-chosen component name to a component definition: a mapping with a
`class` key (e.g. `class: AppenderConsole`, optionally namespaced as
`pkg::ClassName`) plus the remaining keys as constructor arguments.
Component names can be referenced by name wherever a
`layouts`/`filters`/ `appenders` value is expected (in another
component, or in a logger); such references are resolved once and the
resulting object is shared. Appenders and filters can also be defined
inline instead of referenced by name.

Function-valued positions – a logger's `exception_handler`, filter
entries, and (as of the generalized form) **any** constructor argument
of a `layouts`/`filters`/`appenders` component (e.g. a `LayoutJson`'s
`transform_event`, or an appender's dynamic `index`) – accept one of: a
**name reference** (a bare `character` scalar for `exception_handler`,
or an explicit `{ ref: <name> }`), resolved by lookup (`<pkg>::<name>`
supported); an **inline code** form (`{ code: "<R source>" }`), the only
form that evaluates config-supplied source and is therefore rejected
unless the loader is called with `allow_code = TRUE`; and, for filters
only, a `{ class: <FilterClass>, ... }` spec (constructing an R6
[EventFilter](https://s-fleck.github.io/lgr/reference/EventFilter.md))
or a bare-string reference to a named `filters` component. Outside of
these explicit `{ ref: ... }`/`{ code: ... }` forms, a plain `character`
scalar in an ordinary argument position is always a literal string,
never an implicit function reference.

## `enabled`

Component definitions under `layouts`/`filters`/`appenders`, and logger
definitions under `loggers`, accept an optional `enabled` key (a
boolean, defaulting to `TRUE`) – the natural target for `${VAR:-false}`
interpolation, so a sink can be toggled per environment without adding
or removing YAML blocks. When `enabled: false` on a *named* component,
it is not instantiated, and any `appenders:`/`filters:` list entry
elsewhere that references it by name is silently dropped rather than
treated as an "unknown reference" error; an inline (unnamed) component
definition with `enabled: false` is likewise dropped from whichever list
it appears in. When `enabled: false` on a logger definition, that logger
is fully reset to its default/unconfigured state (as if it had never
been configured), regardless of whatever state it was in before – this
keeps re-applying the config idempotent, rather than depending on the
logger's prior state.

## Applying a config is a full snapshot, not a patch

A logger definition under `loggers` fully determines that logger's
`threshold`, `appenders`, `filters`, `propagate` and
`exception_handler`: any of these keys that are *not* given for a listed
logger are reset to their default (no threshold/inherited, no appenders,
no filters, `propagate: true`, the default exception handler) rather
than left as they were before the config was applied. This makes
re-applying the same declarative config (or switching between config
files) reproducible and independent of whatever state a Logger happened
to be in beforehand. Note that this differs from the plain
(non-declarative) `list` form accepted by
[Logger\$config()](https://s-fleck.github.io/lgr/reference/Logger.md),
which only touches the elements it is given and leaves everything else
untouched.

## `disable_existing_loggers`

If `TRUE`, every pre-existing non-root logger *not* named under
`loggers` is reset to its default/unconfigured state before the rest of
the config is applied (loggers that *are* named under `loggers` are
unaffected by this flag; they always end up in the state the config
describes for them, per the previous section). Defaults to `FALSE`, so
orphaned loggers are left alone unless you opt in.

**Resetting is not the same as silencing.** A reset logger gets
`propagate = TRUE` and no appenders/threshold of its own, i.e. it now
inherits its ancestors' appenders and threshold. If a logger was
deliberately muted (e.g. `propagate: false` with no appenders) before
the reset, this can make it *more* verbose afterwards, not less – it
starts forwarding events to whichever ancestor appenders it now inherits
from.

## Environment variable interpolation

Scalar string values may contain `${VAR}` (substituted with the value of
environment variable `VAR`) or `${VAR:-default}` (using `default` when
`VAR` is unset or empty). A literal `${...}` can be produced with the
escape `$${...}`. A value produced *entirely* by interpolation is
leniently coerced (number-like to `numeric`, `true`/`false` to
`logical`), so `size: ${SIZE}` behaves like a literal `size: 1048576`.
Interpolation happens before validation and is never applied to `code:`
payloads.

## Safety

Loading a config never evaluates config-supplied R source by default.
Only the `code:` form does so, and it is rejected during validation
unless the caller passes `allow_code = TRUE`. All other forms
(name/`ref` references, `class` specs, named-component references) are
data, not source, and are always available.

## Environment-layered configs

lgr's config format itself is deliberately *not* environment-aware – it
describes one logger hierarchy, not a `default`/`qa`/`prod` bundle of
them. If you need different values per deployment stage, merge them
*before* calling
[`read_config()`](https://s-fleck.github.io/lgr/reference/read_config.md)/[Logger\$config()](https://s-fleck.github.io/lgr/reference/Logger.md),
rather than asking lgr to do the merging:

- for simple cases, `${VAR}`/`${VAR:-default}` interpolation (see above)
  is usually enough on its own – e.g.
  `isEnabled: ${ELASTIC_ENABLED:-false}`;

- for config files that look like:

      default:
        logger:
          appenders: {...}
      qa:
        logger:
          appenders: {...}

  (one `default` section plus per-environment overrides, a common
  pattern e.g. with the config package), load and merge the relevant
  sections yourself – `config::get(file = "logger.yaml", config = "qa")`
  or a plain `utils::modifyList(default_cfg, env_cfg)` – and pass the
  *already-merged* `list` to
  [`read_config()`](https://s-fleck.github.io/lgr/reference/read_config.md)/`Logger$config()`.

## See also

[Logger](https://s-fleck.github.io/lgr/reference/Logger.md),
`vignette("configuring-lgr", package = "lgr")`
