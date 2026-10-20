## Why

`lgr`'s declarative `version: 1` config format is meant to express an entire logger hierarchy without any hand-written R code. To validate that claim, we reproduced a real, existing production logging setup - a hand-rolled `config_logging()` helper that reads a YAML config (optionally environment-layered) and wires up console/file/ElasticSearch/Dynatrace logging by hand, including `lgrExtra` appenders, custom JSON layouts with `transform_event`/`transform_event_names`, a dynamic per-day ElasticSearch index name, and injection of system metadata (host name, container id, script path, R version) via `FilterInject` - as a native `lgr` config, to prove the native format can actually reproduce this kind of setup end-to-end, and fix it where it can't.

Reproducing that production `logger:` config block as a native `lgr` config surfaced three concrete gaps:

1. **No generic function-valued constructor arguments.** The config loader only allows function values (`ref:`/`code:`) for a logger's `exception_handler` and for filter entries. It cannot express a custom `transform_event` function on `LayoutJson` (required to reproduce a Dynatrace-compatible JSON layout).
2. **No declarative way to disable a component.** The reference config uses an `isEnabled: true/false` flag per appender so ops can flip a sink on/off per environment without deleting/re-adding YAML blocks (e.g. via environment-layered config). The native format has no equivalent - an appender is either fully specified or entirely absent from `appenders:`/logger `appenders:` list.
3. **No convenient way to inject standard runtime metadata.** `config_logging()` automatically injects `hostName`, `containerId` (in Docker), `rVersion` and `scriptFile` into every log event via `FilterInject`, computed once at config-apply time by calling R functions (`Sys.info()`, `R.Version()`, Docker-file detection, RStudio/`Rscript` script-path introspection) - not by reading environment variables, so it cannot be expressed with plain `FilterInject` + `${VAR}` interpolation alone (unlike a deployment-tool release id, which is just `Sys.getenv("RELEASE_ID")` and already works today via `"${RELEASE_ID:-}"`).

Environment-layered configs (`default:`/`qa:`/`prod:` merged externally, e.g. via the `config` package) are deliberately treated as out of scope for `lgr` itself - that merging can and should happen before the merged result is handed to `read_config()`/`Logger$config()`.

**ElasticSearch is explicitly out of scope for this change.** `lgrExtra::AppenderElasticSearch$new()` requires a pre-built ElasticSearch connection *object* (from `elastic::connect(host)`) as its `conn` argument - not a host string, not a function to be called later by the appender (unlike `index`, which does accept a function). Building that object from primitive config values (a host string) doesn't fit either of this change's two supported patterns for dynamic values (a function `lgr` calls later, vs. a fully-computed literal via a dedicated class like `FilterInjectSystemInfo`) without either blending those two meanings together or bolting on a one-off mechanism for a single field. The clean fix belongs in `lgrExtra` itself: extend `AppenderElasticSearch` to accept the primitive connection arguments (e.g. `host`) directly, so `conn` becomes an ordinary constructor argument. That's follow-up work in the `lgrExtra` repo, not `lgr`, and is out of scope here.

## What Changes

- Add support for function-valued constructor arguments (`ref:`/`code:`) on **any** component field, not just `exception_handler`/filter entries - so a `layouts` component can declare `transform_event: {ref: "pkg::some_fun"}` (or `code:`, gated by `allow_code`), matching the same safety rules already used for filters.
- Add an `enabled` key (default `TRUE`) recognized on appender/filter/layout component definitions and on logger definitions; when `enabled: false`, the component is parsed/validated but omitted from the resolved config (an appender entry referencing a disabled component is dropped; a disabled logger definition is fully reset to its default state).
- Add a new exported `FilterInjectSystemInfo` R6 class (inherits `FilterInject`) that automatically computes and injects standard runtime metadata (`hostName`, `rVersion`, `containerId` when running in Docker, `scriptFile` when determinable) once at construction time, while still accepting arbitrary extra/override named values exactly like `FilterInject`. This is ordinary content added through the *existing* `class:`-based extension mechanism - it requires no changes to the config loader itself.
- Write a native `lgr` config reproducing the console + file + `lgrExtra::AppenderDynatrace` portion of the reference production setup (with the same JSON layout/transform and system-metadata injection as `config_logging()`, using `FilterInjectSystemInfo`) as a fixture/example and cover it with an integration test, to prove parity with a real production use case. ElasticSearch is deliberately excluded (see "Why").
- Document, in `?config`, the recommended pattern for environment-layered configs (pre-merge with the `config` package or plain list-merging, then call `read_config()`), including a worked example.

## Capabilities

### New Capabilities
- `system-info-injection`: a `FilterInjectSystemInfo` filter class providing built-in, config-friendly injection of standard runtime metadata into log events.

### Modified Capabilities
- `config-loading`: generalize function-valued (`ref:`/`code:`) argument resolution to arbitrary component constructor arguments, and add `enabled`-based component/logger toggling.
- `config-validation`: validate the new `enabled` key (must be boolean) and validate `ref:`/`code:` forms in arbitrary argument positions using the existing safety rules (`allow_code`).
- `config-schema`: extend the shipped JSON Schema to reflect `enabled` and the generalized function-value form.

## Impact

- Affected code: `R/config.R` (`validate_component_def`, `validate_function_value`, `instantiate_component`, `resolve_*` helpers, `apply_config`), `inst/lgr-config-schema.json`, `R/Filter.R` (new `FilterInjectSystemInfo` class), `R/system_info.R` (new runtime-metadata detection helpers, adapted from a hand-rolled production logging config helper).
- Affected docs: `?config` roxygen docs, `vignette("configuring-lgr")`, new `?FilterInjectSystemInfo` roxygen page.
- New optional dependency: `rstudioapi` added to `Suggests` (for script-path detection when running interactively in RStudio), with graceful degradation when absent - consistent with lgr's existing pattern for `yaml`/`jsonlite`.
- New test fixtures under `tests/testthat/` reproducing the console/file/Dynatrace portion of a real production config (requires `lgrExtra` as a `Suggests` test dependency, already used elsewhere in the test suite for Dynatrace/ElasticSearch appenders).
- No breaking changes to existing configs: `enabled` defaults to `TRUE`, existing `ref:`/`code:` usage in filters/`exception_handler` is unaffected, and `FilterInjectSystemInfo` is a purely additive new class alongside the existing `FilterInject`.
