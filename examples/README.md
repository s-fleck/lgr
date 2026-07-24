# Manual test examples for the config system

Each numbered folder is a small, self-contained scenario for a human to run
and eyeball the output of, covering one aspect of the declarative logging
config system (`config-schema`, `config-loading`, `config-interpolation`,
`config-validation`).

## Running

From the package root, with the dev version of lgr loaded:

```r
devtools::load_all(".")
source("examples/01-basic-yaml-config/demo.R")
```

Each `demo.R` is meant to be run interactively (`source()`'d or pasted line
by line), not with `Rscript`. Every script prints what it's doing and what
you should see, so you can compare expected vs. actual output as you go.
Restart your R session (or run `lgr$config(NULL)`) between examples if you
run more than one in the same session, since config state persists on the
package-level `lgr` root logger.

## Scenarios

| Folder | What it exercises |
| --- | --- |
| `01-basic-yaml-config` | Loading a config from a file path vs. a YAML string vs. a `list`; chaining; `config(NULL)` reset |
| `02-multi-logger-hierarchy` | Named `layouts`/`filters`/`appenders`, shared instances, descendant loggers, `propagate`, `disable_existing_loggers` |
| `03-env-var-interpolation` | `${VAR}`, `${VAR:-default}`, type coercion, missing-variable error, `$${...}` escaping |
| `04-safe-code-eval-gate` | `code:` rejected by default, name/`ref` references always allowed, opt-in via `allow_code = TRUE` |
| `05-validation-errors` | Unknown keys, wrong types, dangling references, cycles, did-you-mean suggestions - and that the hierarchy is left untouched |
| `06-json-and-rotation` | JSON config format, `AppenderFileRotating`, `LoggerGlue` |
