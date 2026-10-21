---
name: lgr-glue-package-usage
description: "Use when adding, refactoring, or reviewing package logging in R with lgr and LoggerGlue: package-level lg setup, structured fields, level selection, and error logging."
---

# lgr package logger usage

## Purpose

Use this skill when you need to add, refactor, or review logging in R packages.

## Agent trigger phrases

Load this skill when the request mentions one or more of:

- lgr logging in a package
- LoggerGlue or glue-style logging
- package logger setup or `.onLoad`
- improve log levels or structured logging
- log errors with context
- capture or assert log events in package tests

## Core rules (apply always)

- Use one package logger named `lg`.
- Create loggers with `lgr::get_logger()` or `lgr::get_logger_glue()`, never `Logger$new()`.
- Prefer `lgr::get_logger_glue("<package_name>")` for interpolation and named fields.
- Keep dynamic values in named fields, not long pasted strings.
- Do not configure appenders/layouts/thresholds inside package code, and do not ship a package-specific configuration system. Configuration belongs to the host application (entry point, launcher, or user startup code) applying lgr's declarative config — see section 5.

### 1) Package-level logger initialization

Initialize shared logger `lg` in `.onLoad` in `R/<package_name>-package.R`:

```r
.onLoad <- function(...) {
  assign(
    "lg",
    lgr::get_logger_glue("<package_name>"),
    envir = parent.env(environment())
  )
}
```

### 2) Structured logging with named fields

Always use structured logging with named fields for dynamic values.

```r
lg$info("Create target index {indexName}", indexName = index_name)
lg$debug("Retrying bulk insert because of error: {errorMessage}", errorMessage = flatten_error_parents(error$message))
```

- Keep field names stable and descriptive (for example: `indexName`, `documentCount`, `batchCount`).
- Pick one field-name convention per project (lgr's examples use camelCase, which matches common sinks such as Elasticsearch) and keep it stable across the codebase.

### 3) Level selection

- `info`: lifecycle milestones (start, created index, completed insert, saved outputs)
- `debug`: progress and diagnostics (batching, retries, remote table connection)
- `warn`: recoverable anomalies (unexpected input shape, index already exists)
- `error`: failures and abort paths
- `trace`: very low-level diagnostics; usually avoid in production

Use the lowest level that still keeps production logs actionable.

### 4) Error handling pattern

Zero-dependency default (base R): log context, then rethrow the original condition:

```r
tryCatch({
  # work
}, error = function(e){
  lg$error("Operation failed: {msg}", msg = e$message)
  stop(e)
})
```

- Include enough context fields to debug without reproduction.
- If the project uses rlang (tidyverse-style packages), prefer preserving the original condition's class, message, and call: log, then re-signal with `rlang::cnd_signal(e)`, or wrap with `rlang::abort(..., parent = e)` when adding context of your own.
- Use `lgr::get_caller()` when explicit caller metadata is needed.

### 5) Configuring logging at startup (host side, not package side)

A package exposes **no logging-configuration API** of its own: no exported setters, no vendored config format. The host application applies lgr's declarative configuration once at startup, after the package is loaded:

- file-based: `lgr::lgr$config("<path>/logging.yaml")` with a `version: 1` YAML/JSON document (see `?lgr::config`), or the `LGR_DEFAULT_CONFIG` environment variable;
- `${VAR}` interpolation keeps host-specific paths (log file locations) out of the config text.

```yaml
# <data-dir>/logging.yaml — applied by the host launcher via lgr::lgr$config()
version: 1
appenders:
  jsonl:
    class: AppenderJson
    file: "${LOG_FILE:-<package_name>.jsonl}"
loggers:
  <package_name>:
    class: LoggerGlue
    threshold: info
    appenders: [jsonl]
```

- Declare `class: LoggerGlue` for glue loggers: a logger definition without `class:` instantiates a plain `Logger`, which silently drops interpolation.
- Name only the loggers you intend to change (`disable_existing_loggers: false`, the default) so the host never reconfigures unrelated loggers.
- A missing config file is a valid default (lgr defaults hold); a file lgr rejects must fail loud. lgr's validation errors name the offending key but not the source file — hosts should wrap the call and include the path in the message.

### 6) Capturing log events in tests

Assert on behavior, not on console side effects: attach an `AppenderBuffer`, raise the threshold, run the code, and restore state deterministically:

```r
capture_lg_records <- function(code) {
  app <- lgr::AppenderBuffer$new(buffer_size = 100L)
  lg$add_appender(app, name = "test_buffer")
  old_threshold <- lg$threshold
  lg$set_threshold("trace")
  on.exit(
    { lg$remove_appender("test_buffer"); lg$set_threshold(old_threshold) },
    add = TRUE
  )
  force(code)
  events <- app$buffer_events
  app$clear()
  events
}

# assert (levels are integers: 200 error, 300 warn, 400 info, 500 debug)
events <- capture_lg_records(lg$warn("import skipped", n = 3L))
expect_true(any(vapply(
  events,
  function(e) e$level == 300L && grepl("import skipped", e$msg, fixed = TRUE),
  logical(1)
)))
```

- Always restore the threshold and remove the buffer appender via `on.exit()`/`withr::defer()`, even on failure.
- Mute inherited console appenders the same way if the suite must stay silent.

## Agent workflow

1. Verify package-level logger exists and is named `lg`.
2. If missing, add `.onLoad` logger initialization with `get_logger_glue("<package_name>")`.
3. Convert interpolated free-text values into named fields.
4. Adjust levels to `info`/`debug`/`warn`/`error` using the conventions above.
5. Ensure error paths log context and preserve the original condition (rethrow, or `cnd_signal()` when rlang is available — section 4).
6. Leave logger configuration to the host application applying lgr's declarative config (section 5), not package internals.
7. In tests, assert on captured buffer events, never on console side effects (section 6).

## Review checklist

- Single package logger (`lg`) is used consistently.
- Logger created via `get_logger()` or `get_logger_glue()`, never `Logger$new()`.
- Logger name follows `<package_name>/...` hierarchy where relevant.
- Dynamic values are passed as named fields in one stable per-project convention.
- Levels match event intent and are not overly noisy.
- Error logs include actionable context and preserve original condition details.
- No package file attaches appenders or sets thresholds; no package-owned config API or config format.
- Tests capture events via a buffer appender with guaranteed state restore.
- Messages are concise, consistent, and machine-friendly.
