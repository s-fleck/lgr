---
name: r-package-style-guide
description: >
  Code style conventions for R package code. Use when writing or refactoring
  functions in an R package to apply company style rules (pipes, anonymous
  functions, formatting, data.table, prohibited packages).
metadata:
  author: Stefan Fleck
  version: "3.0.0"
---

# R Package Style Guide

Apply these conventions consistently when generating or modifying code in an R
package. For workflow commands, testing, documentation and NAMESPACE rules see
the `r-package-development` skill.

## When To Use

- Writing a new function in `R/`.
- Refactoring existing package code to company style.
- Reviewing package code for style conformance.

## AI Coding Directives

- **Favor vectorized operations** over loops; prefer functional composition and stateless pipelines
- **Minimize output**: Use `{cli}` package for necessary console output only; avoid excessive logging
- **Show base R solutions first**; suggest helper functions only for genuinely repetitive operations
- **Separate concerns**: Keep data preparation separate from plotting and layout
- **Avoid excessive comments**: Code should be self-explanatory
- **Validate the final code**: Ensure style conformance before finishing.

## Code Conventions

- **Pipe**: Always use `|>`, never `%>%`
- **Anonymous functions**: Use `\(x) x + 1`, not `function(x)`
- **URL building**: Use `sfmisc::url_tidy(...)`
- **Indentation**: 2 spaces
- **Formatter**: Run `air format .` after generating any code. If the project has a clean `air format . --check` baseline (Air 0.12.0, typically pinned in CI), formatting whole files is safe; otherwise format only the files you touched
- **Prohibited packages** Never use the following packages: magrittr, elastic
- Do not use `dplyr::case_match()`, use the modern replacement `dplyr::recode_values()` instead
- **Never `<<-`** — not in package code, not in tests. Callbacks/promises
  evaluate in the caller's environment, so plain `<-` already writes where you
  want; `<<-` mutates a scope you do not own.
- **Scalar numeric guards use `is.finite()`**, not `is.na()` — `is.na()`
  happily passes `Inf` (a percentage formatter once rendered "Inf%"). Use
  `length(x) == 1L && !is.finite(x)` (covers NA/NaN/±Inf).

## Errors (fail loud)

- **Error primitive is `stop()` with a classed condition** — raise a custom
  condition so callers can catch a class: `stop(ConfigValidationError(msg))`,
  or `error(message = msg, class = "MyError")`. Never `stop()` with a bare
  string for a user-facing failure: name the offending argument/file/key/value.
- **One message, one string** — combine message parts with
  `sprintf()`/`paste0()` into a single string before raising. A condition
  constructor takes one message; splitting it across calls loses the class.
- **Validation**: if the project vendors assertion helpers — `assert(condition,
  message)` plus NA-safe predicates such as `is_scalar_numeric()`, `is_n()`,
  `is_integerish()` — prefer them over hand-rolled `if (bad) stop(...)`. An
  assertion helper needs a strictly `TRUE`/`FALSE` condition: an `NA` or
  length-0 condition fires a generic message instead of yours, so compose
  NA-safe predicates (`!anyNA(x)`, `is_scalar_numeric(x) && !is.na(x)`), never
  a bare comparison that can yield `NA`.
- **Rethrow preserves the original condition**: inside `tryCatch(..., error = ...)`
  log with context, then re-raise the caught condition with `stop(e)` — never
  rebuild it from `conditionMessage(e)`, which drops the class.
- `sprintf()` templates and args must match exactly — a stray arg only *warns*
  at runtime, it does not fail. Assert the rendered message with
  `expect_error(regexp = )`.
- No `stopifnot()` for user-facing errors: `stop(<Condition>(sprintf(...)))` must
  name the offending argument/file/key/value.

**Package depends on rlang?** Load the `r-package-style-guide-rlang` skill — it
overrides this section only.

## File & console I/O

- `cat()` has no `useBytes` — stray args land in `...` and get *printed* (this
  once corrupted a CSV with a literal `TRUE`). Use
  `writeLines(x, useBytes = TRUE)`; check `formals()` before passing extras.
- Writes to user files: write `path.tmp`, rename, `unlink()` the tmp on the
  failure path.

## Database (DBI/SQLite)

- Parameterized `?` placeholders only — never string-paste values (injection +
  quoting bugs).
- Multi-statement writes in one transaction: `dbBegin()`/`dbCommit()` with
  `dbRollback()` on error.
- Give `sprintf()`-built SQL/PRAGMA the same template/arg care as error
  messages.

## data.table

**Do not pipe `set*` functions** - they modify by reference:

```r
# Good:
data.table::setnames(iris, sfmisc::pascal_case(names(iris)))

# Bad:
iris |> data.table::setnames(sfmisc::pascal_case(names(iris)))
```

**data.table semantics** — scoping (`i`/`j`, column-name shadowing), joins,
by-reference updates and key types — are covered by the `data-table` skill. This
section only covers style.
