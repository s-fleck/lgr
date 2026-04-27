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

## Core rules (apply always)

- Use one package logger named `lg`.
- Create loggers with `lgr::get_logger()` or `lgr::get_logger_glue()`, never `Logger$new()`.
- Prefer `lgr::get_logger_glue("<package_name>")` for interpolation and named fields.
- Keep dynamic values in named fields, not long pasted strings.
- Do not configure appenders/layouts/thresholds inside package code; configure centrally at startup by the package user.

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
lg$info("Create target index {indexName} on smartFeed cluster", indexName = index_name)
lg$debug("Retrying bulk insert because of error: {errorMessage}", errorMessage = flatten_error_parents(error$message))
```

- Keep field names stable and descriptive (for example: `indexName`, `documentCount`, `batchCount`).

### 3) Level selection

- `info`: lifecycle milestones (start, created index, completed insert, saved outputs)
- `debug`: progress and diagnostics (batching, retries, remote table connection)
- `warn`: recoverable anomalies (unexpected input shape, index already exists)
- `error`: failures and abort paths
- `trace`: very low-level diagnostics; usually avoid in production

Use the lowest level that still keeps production logs actionable.

### 4) Error handling pattern

Common pattern for logging errors:

```r
tryCatch({
  # work
}, error = function(e){
  lg$error("Operation failed: {msg}", msg = e$message)
  stop(e)
})
```

- Include enough context fields to debug without reproduction.
- Preserve original conditions with parent/response payloads when rethrowing.
- Use `lgr::get_caller()` when explicit caller metadata is needed.

## Agent workflow

1. Verify package-level logger exists and is named `lg`.
2. If missing, add `.onLoad` logger initialization with `get_logger_glue("<package_name>")`.
3. Convert interpolated free-text values into named fields.
4. Adjust levels to `info`/`debug`/`warn`/`error` using the conventions above.
5. Ensure error paths log context and rethrow original conditions.
6. Leave logger configuration to application startup, not package internals.

## Review checklist

- Single package logger (`lg`) is used consistently.
- Logger created via `get_logger()` or `get_logger_glue()`, never `Logger$new()`.
- Logger name follows `<package_name>/...` hierarchy where relevant.
- Dynamic values are passed as named fields.
- Levels match event intent and are not overly noisy.
- Error logs include actionable context and preserve original condition details.
- Messages are concise, consistent, and machine-friendly.
