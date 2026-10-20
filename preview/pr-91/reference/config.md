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

Function-valued positions (a logger's `exception_handler`, and filter
entries) accept one of: a **name reference** (a bare `character` scalar
for `exception_handler`, or an explicit `{ ref: <name> }`), resolved by
lookup (`<pkg>::<name>` supported); an **inline code** form
(`{ code: "<R source>" }`), the only form that evaluates config-supplied
source and is therefore rejected unless the loader is called with
`allow_code = TRUE`; and, for filters only, a
`{ class: <FilterClass>, ... }` spec (constructing an R6
[EventFilter](https://s-fleck.github.io/lgr/reference/EventFilter.md))
or a bare-string reference to a named `filters` component.

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

## See also

[Logger](https://s-fleck.github.io/lgr/reference/Logger.md),
`vignette("configuring-lgr", package = "lgr")`
