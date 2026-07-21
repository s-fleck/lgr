## Context

lgr configures loggers, appenders, layouts, and filters as R6 objects wired
together at runtime. Today `R/logger_config.R` contains an unexported,
undocumented config layer with two overlapping dialects:

- a simple `Logger:` shorthand (`as_logger_config()` / `logger_config()`) that
  configures a single logger, and
- a hidden multi-logger `logging_config` (`as_logging_config()` /
  `apply_logging_config()`) with `version`, `disable_existing_loggers`, named
  `layouts`/`filters`/`appenders`, cross-references, and a `loggers` map.

The multi-logger engine already does the hard part well: it resolves named
components with a memoized `resolve_named()`, detects cyclic references,
instantiates R6 constructors from both implicit `ClassName:` keys and explicit
`class:`/`constructor:` specs, and supports namespaced `pkg::Class` for
extension packages (`lgrExtra`). What it lacks is a single clear schema, a real
validation layer with good errors, interpolation, editor support, a public
exported API, and documentation.

`Logger$config()` is the single application entry point and already dispatches
between the two dialects. `yaml` and `jsonlite` are Suggests (config is
opt-in). Work is happening on the `0.6.0` branch, pre-1.0, so a breaking format
change is acceptable.

## Goals / Non-Goals

**Goals:**

- One canonical, versioned, well-documented config schema covering every lgr
  construct (per `config-schema`).
- A public, exported, documented loading API (per `config-loading`).
- A read → interpolate → validate → resolve → apply pipeline that is
  **atomic**: the live hierarchy is never partially mutated.
- Strict validation with path-addressed errors and did-you-mean hints (per
  `config-validation`), plus a shipped JSON Schema for editors.
- Safe by default: no arbitrary R source is evaluated unless the caller opts in
  with `allow_code = TRUE` (per `config-loading`).
- `${VAR}` / `${VAR:-default}` interpolation (per `config-interpolation`).
- Retain and harden the existing resolver/cycle-detection logic rather than
  rewriting it from scratch.

**Non-Goals:**

- Config **serialization / export** (round-tripping a live logger tree back to
  YAML/JSON). Explicitly out of scope for this change (read-only decision).
- Backward compatibility with the legacy `Logger:` shorthand or the exported
  `as_logger_config()` / `logger_config()` surface — these are removed.
- Live config reloading / file watching.
- New hard dependencies; `yaml`/`jsonlite` stay optional.
- Allowlisting which R6 `class:` values may be instantiated. Safe mode (D8)
  gates arbitrary R *source* evaluation, not class construction from installed
  packages; restricting instantiable classes is possible future hardening.

## Decisions

### D1: A single pipeline with strict phase boundaries

Loading is implemented as an ordered pipeline of pure-ish stages, each with a
single responsibility:

```
read()        raw text/list  -> nested R list  (yaml/jsonlite; format detect)
interpolate() nested list    -> nested list    (${VAR} substitution on scalars)
validate()    nested list    -> nested list    (schema + reference + cycle checks)
resolve()     nested list    -> object graph   (R6 ctors, named-component memo)
apply()       object graph   -> mutate loggers (root + hierarchy)
```

Only `apply()` touches global state. `read`→`resolve` are computed entirely into
a fully-instantiated object graph first; `apply()` runs last and is the only
mutating step, giving atomicity (D3). **Alternative considered:** interleave
resolution and application per-logger (as the current `apply_logging_config()`
partly does). Rejected — it can leave the hierarchy half-configured on error and
makes validation ordering fragile.

### D2: Keep and harden the resolver, replace everything around it

`resolve_named()`, `resolve_component()`, `resolve_r6_ctors()`,
`get0_R6Class()`, and cycle detection are the sound core and are retained
(moved into the resolve stage). The reader, interpolation, validator, and
public API are new. The two legacy `as_*_config()` dialect entry points and the
`logger_config` S3 constructor are deleted. **Alternative considered:** full
green-field rewrite. Rejected — the resolver already handles subtle cases
(memoization, shared instances, namespaced ctors) that are easy to regress.

### D3: Atomicity via build-then-swap

`resolve()` produces, for the root config, a complete set of constructed
objects and a per-logger plan. `apply()` first computes the set of loggers to
reset (when `disable_existing_loggers`), then applies each logger's resolved
plan. **The atomicity guarantee rests on build-then-swap**: parsing,
interpolation, validation, and *all* R6 construction complete before `apply()`
mutates anything, so every failure mode named in the specs (invalid schema,
dangling/cyclic reference, constructor throw, missing env var) occurs strictly
before mutation and leaves the hierarchy untouched. `apply()` itself only wires
already-built objects into loggers, so it has no remaining validation/construction
failure surface. This fully satisfies `config-loading` → "Atomic application";
the requirement's absolute "no logger SHALL be modified" is achievable precisely
because nothing that can fail runs during the mutation step.

### D4: Schema definition is data, shared by validator and JSON Schema

The canonical schema is expressed once as an in-package R data structure (the
set of legal top-level keys, section shapes, per-key types, and the
class→constructor-arg expectations). The runtime validator walks the parsed
config against this structure. The **shipped JSON Schema** (`inst/`) is
generated from / kept consistent with the same source of truth, and a test
asserts the bundled example configs validate against it (per
`config-validation`). **Alternative considered:** hand-maintain the JSON Schema
separately. Rejected — drift between the runtime validator and the editor schema
is a maintenance hazard; a single source keeps them consistent.

### D5: Interpolation is a scalar-string pass before validation, with lenient coercion

Interpolation runs on scalar leaf strings only, after parsing and before
validation, so interpolated values are validated like literals (per
`config-interpolation`). Syntax: `${VAR}`, `${VAR:-default}`, escape `$${...}`.
Unset-without-default is a hard error naming the variable and its path.

Because env vars are always strings but interpolation runs post-parse, a value
produced *entirely* by interpolation is **leniently coerced** so it behaves like
the equivalent literal: number-like strings become numbers, boolean-like strings
become logicals, everything else stays a string; where the schema declares an
expected type for the field, coercion targets that type. Partial interpolation
(a `${VAR}` embedded in surrounding text, e.g. `${DIR}/app.log`) stays a string
with no numeric/boolean coercion. This keeps `size: ${SIZE}` /
`propagate: ${FLAG}` working without forcing users to think about quoting.
**Alternative considered:** interpolate lazily at apply time, or leave all
interpolated values as strings and require typed fields to accept strings.
Rejected — post-interpolation validation is simpler to reason about, and
string-only values would make numeric/boolean fields awkward and inconsistent
with their literal forms.

### D6: Error reporting carries a path

Validation accumulates errors as (path, message, suggestion?) records where
`path` is a dotted/indexed accessor (`loggers.app.appenders[2]`). Did-you-mean
uses a small string-distance check against known keys / component names. The
validator collects multiple errors before failing where practical (per
`config-validation`). Runtime and JSON Schema messages need not be byte-identical
but must agree on validity.

### D8: Safe by default, code evaluation is opt-in

Loading is **safe by default**: the pipeline never runs `eval(parse(text=...))`
on config-supplied R source unless the caller passes `allow_code = TRUE`. In
safe mode, the two code-valued keys — inline `exception_handler` bodies and
inline function-based filters — are rejected during validation with an error
that names the key, its path, and the opt-in. This makes a config file safe to
load from a coworker, a repo, or a deployment artifact without it being able to
execute arbitrary R.

The boundary is deliberately drawn at **arbitrary R source evaluation**, not at
object construction: `class:`-based instantiation of R6 appenders/layouts/filters
from installed packages (with data-only arguments) remains available in safe
mode, because it is the declarative mechanism the whole format is built on and
is analogous to log4j/NLog instantiating named classes from config. This keeps
the common cases (including the R6 filter classes like `FilterForceLevel`, which
cover most filtering needs declaratively) fully usable without `allow_code`.

`allow_code` is threaded through every loading entry point
(`read`/`validate`/`Logger$config()`); validation is the layer that enforces it,
so rejection happens before any construction or mutation (consistent with D3
atomicity). **Alternatives considered:** (a) a global option instead of an
argument — rejected as too easy to set-and-forget globally, defeating the safe
default; (b) allowlisting permitted R6 classes as well — deferred as future
hardening (noted in Non-Goals), since it would complicate the extension-package
story (`pkg::Class`) without addressing the primary `eval` risk.

### D9: Function-valued keys are an explicit tagged union

Positions expecting an R function (`exception_handler`, filter entries) accept a
small tagged union rather than an overloaded bare string:

- **name reference** — bare string (for `exception_handler`) or `{ ref: name }`;
  resolved by looking up `name` / `pkg::name` via a `get0_function()` sibling to
  the existing `get0_R6Class()` (same search order: lgr → loaded namespaces →
  global). Lookup does not execute the function, so it is safe.
- **inline code** — `{ code: "<R source>" }`; the only form that
  `eval(parse())`s config-supplied source.
- **class** (filters only) — `{ class: FilterClass, ... }`, an R6 `EventFilter`.
- **named-component reference** (filters only) — bare string naming a top-level
  `filters` entry (the existing mechanism; this is why a filter's bare string is
  *not* a function-name reference — use `{ ref: }` for that).

Precedence when classifying a mapping: `class:` present → class spec (a nested
`code` key is just a constructor arg); else `code:` present → inline code; else
`ref:`/bare string → reference.

This is what makes safe mode (D8) precise: **the safe/unsafe decision keys off
the presence of `code:`**, never a regex that tries to tell "a name" from "a
code body" apart. A malicious config file cannot carry an executable payload
except under a `code:` key, which the default path refuses. It also composes
with interpolation (D5): `code:` payloads are exempt from `${VAR}` substitution
so env values can't be injected into R source.

A live R `function` supplied through a programmatic `list` config is a resolved
object, not source, and is accepted regardless of `allow_code`. **Alternative
considered:** sniff a bare string (name-like regex → reference, else → code).
Rejected — it makes the security boundary depend on a regex (a bug there is a
vulnerability) and is a foot-gun for authors; explicit `code:` keeps the
dangerous path obvious in the file and trivial to validate. YAML/JSON mapping
keys (`code`/`ref`/`class`) were chosen over YAML custom tags for JSON
portability.

### D7: Public API surface

- `Logger$config(x)` remains the primary entry point, now accepting only the
  unified file format (path or string) or a schema-shaped `list`, plus `NULL`
  to reset.
- Add exported, documented helpers for reading/validating a config without
  applying it (useful for testing and CI of user config files), e.g. a
  `read_config()` / `validate_config()` pair, and a `config` object class.
- Export the JSON Schema location accessor.

Exact function names are finalized during implementation; the requirement is a
documented, exported surface.

## Risks / Trade-offs

- **[Breaking change for existing config files]** → Ship a migration section in
  the config vignette with before/after examples; keep the change on the 0.6.0
  branch; call it out in NEWS. Programmatic `list`-based single-logger config
  still works.
- **[JSON Schema drifting from the runtime validator]** → Generate/derive both
  from one in-package schema definition (D4) and add a test that the shipped
  examples validate against the JSON Schema.
- **[Atomicity not fully achievable if an R6 constructor mutates global state on
  build]** → Construct all objects before `apply()`; document that appender
  constructors should be side-effect-free at build time; reset-set ordering
  chosen to minimize windows.
- **[Interpolation surprises (e.g. `${}` in a legitimate log format string)]** →
  Provide the `$${...}` escape and scope interpolation to scalar values only;
  document clearly which values are interpolated.
- **[Config files execute arbitrary R code]** → `exception_handler` and
  function-based filters would otherwise be evaluated as R (`eval(parse(...))`),
  making a config file executable input (same class of risk as YAML
  deserialization RCE). Mitigation: **safe by default** (D8) — code-valued keys
  are rejected unless the caller opts in with `allow_code = TRUE`, so an
  untrusted config cannot run arbitrary R under the default path. The residual
  risk when `allow_code = TRUE`, and the fact that `class:` instantiation still
  runs installed-package constructor code, SHALL be documented in the vignette
  and reference docs. Class allowlisting is possible future hardening
  (Non-Goals).
- **[Optional-dependency friction]** → Detect the needed parser per format and
  raise a clear, actionable "install `yaml`/`jsonlite`" error.

## Migration Plan

1. Implement the pipeline and public API behind the unified schema; keep the
   retained resolver working.
2. Rewrite `inst/configs/*` to the unified schema; add the JSON Schema under
   `inst/`.
3. Replace the `test_config_*` suite with tests aligned to the unified schema,
   validation errors, interpolation, and JSON-Schema conformance.
4. Remove legacy dialect code and exports; update `NAMESPACE` via roxygen.
5. Write the configuration vignette (format reference + worked examples +
   migration section) and reference docs.
6. Update `NEWS.md` with the breaking change and migration pointer.

Rollback: the change is isolated to the config layer plus docs/tests; reverting
the commit restores the previous behaviour without touching core logging.

## Open Questions

- Final exported function names (`read_config`/`validate_config`/schema
  accessor) — resolve during implementation to fit lgr naming conventions.
- Whether `LoggerGlue` selection is expressed via a `class:` on the logger entry
  or a dedicated key — decide when wiring logger construction.
- Whether to depend on an existing R JSON-Schema validator for the
  example-conformance test or ship a minimal internal check — decide based on
  available lightweight packages vs. added Suggests.
