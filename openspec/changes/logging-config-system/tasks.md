## 1. Schema definition (source of truth)

- [ ] 1.1 Define the canonical schema as an in-package R data structure: legal top-level keys (`version`, `disable_existing_loggers`, `layouts`, `filters`, `appenders`, `loggers`), section shapes, per-key types, and class→constructor-arg expectations (supports `config-schema`, `config-validation` D4).
- [ ] 1.2 Enumerate configurable classes and their parameters (all appenders incl. rotating/buffer/memory, all layouts, R6 + function filters, `LoggerGlue`), including namespaced `pkg::Class` support.
- [ ] 1.3 Decide and document how `LoggerGlue` selection is expressed in a logger entry (resolves a design Open Question).
- [ ] 1.4 Define the function-valued-key tagged union in the schema (`exception_handler`, filter entries): name reference (bare string / `{ ref: }`), inline `{ code: }`, `{ class: }` (filters), and named-component reference (filters); with the classification precedence `class:` → `code:` → `ref:`/bare (per design D9).

## 2. Reader (parse + format detection)

- [ ] 2.1 Implement `read()` stage: accept file path, YAML/JSON string, or list; detect format by extension then content sniff; parse via `yaml`/`jsonlite`.
- [ ] 2.2 Raise clear, actionable errors when a required optional parser package is not installed.
- [ ] 2.3 Unit-test file paths (`.yaml`/`.yml`/`.json`), string inputs, and missing-parser errors (`config-loading`).

## 3. Interpolation

- [ ] 3.1 Implement scalar-string interpolation pass for `${VAR}` and `${VAR:-default}`, run after parse and before validation.
- [ ] 3.2 Implement `$${...}` escape for literal sequences; error on unset-without-default naming the variable and its config path.
- [ ] 3.3 Implement lenient type coercion of fully-interpolated values: number-like → number, boolean-like → logical, else string (targeting the schema's expected type where declared); leave partial interpolations (`${VAR}` within surrounding text) as strings.
- [ ] 3.4 Exempt inline `code:` payloads from interpolation (no `${VAR}` substitution into R source).
- [ ] 3.5 Unit-test substitution, defaults, escaping, missing-variable errors, coercion (numeric, boolean, string, partial-not-coerced), and the `code:` exemption (`config-interpolation`).

## 4. Validation + error reporting

- [ ] 4.1 Implement `validate()` walking the parsed config against the schema (1.1): unknown keys, wrong types, unsupported `version`, malformed component defs.
- [ ] 4.2 Implement path-addressed error records (`loggers.app.appenders[2]`) with did-you-mean suggestions via string distance; collect multiple errors before failing where practical.
- [ ] 4.3 Implement reference-integrity and cycle-detection checks at the validation layer (naming the cycle / dangling reference).
- [ ] 4.4 Enforce safe-by-default policy in validation by keying off the `code:` form: when `allow_code` is `FALSE`, reject any function-valued key using `{ code: }` with an error naming the key, its path, and the `allow_code = TRUE` opt-in; allow name references, `{ ref: }`, `{ class: }`, and named-component references unconditionally.
- [ ] 4.5 Unit-test each rejection path and the precise error messages, including safe-mode rejections and the `allow_code = TRUE` opt-in path (`config-validation`, `config-loading`).

## 5. Resolver (retain + harden)

- [ ] 5.1 Move `resolve_named()`, `resolve_component()`, `resolve_r6_ctors()`, `get0_R6Class()`, and cycle detection into the resolve stage; keep memoization + shared-instance behavior.
- [ ] 5.2 Ensure named components resolve once and are shared across referencing loggers; support inline definitions mixed with references.
- [ ] 5.3 Add `get0_function()` (sibling of `get0_R6Class()`, same search order) to resolve name references / `{ ref: }` for function-valued keys by lookup (no source eval); accept live `function` objects from list configs as-is.
- [ ] 5.4 Resolve `{ code: }` function values via `eval(parse())` only when `allow_code = TRUE` (validation already blocks them in safe mode); construct all objects before any application.
- [ ] 5.5 Unit-test shared instances, mixed inline/reference, namespaced classes, name-reference resolution (incl. missing-reference error), and constructor-error propagation.

## 6. Application (atomic)

- [ ] 6.1 Implement `apply()` as the only mutating stage: compute reset set for `disable_existing_loggers`, then apply per-logger resolved plans (root + hierarchy).
- [ ] 6.2 Enforce that full multi-logger configs are only applicable via the root logger; error otherwise.
- [ ] 6.3 Guarantee build-then-swap atomicity: verify hierarchy is unchanged when any earlier stage fails.
- [ ] 6.4 Unit-test atomicity (undefined reference, constructor throw), `disable_existing_loggers`, preservation of unmentioned loggers, and `NULL` reset (`config-loading`).

## 7. Public API + wiring

- [ ] 7.1 Wire the pipeline into `Logger$config()` for path/string/list/`NULL`; add the `allow_code = FALSE` argument; return target logger invisibly.
- [ ] 7.2 Add exported helpers to read and validate a config without applying (e.g. `read_config()` / `validate_config()`), each carrying the `allow_code = FALSE` argument, and a `config` object class (finalize names to lgr conventions).
- [ ] 7.3 Add exported accessor for the shipped JSON Schema location.

## 8. JSON Schema asset

- [ ] 8.1 Generate/derive a JSON Schema from the schema source of truth (1.1) and install it under `inst/`.
- [ ] 8.2 Add a test asserting bundled example configs validate against the JSON Schema; decide validator dependency vs. minimal internal check (resolves a design Open Question).

## 9. Remove legacy dialects

- [ ] 9.1 Delete the `Logger:` shorthand and `logging_config` dialect code paths and the `logger_config()` / `as_logger_config()` surface from `R/logger_config.R`.
- [ ] 9.2 Remove associated exports and regenerate `NAMESPACE`/`man/` via roxygen2.
- [ ] 9.3 Rewrite `inst/configs/*` (minimal, recommended, production) to the unified schema.

## 10. Docs

- [ ] 10.1 Write a configuration vignette: full format reference, worked YAML/JSON examples, interpolation (incl. coercion rules), validation, and a migration section from the legacy formats.
- [ ] 10.4 Document the security model in the vignette and reference docs: loading is safe by default (code-valued keys rejected); `allow_code = TRUE` opts into evaluating `exception_handler`/function filters; note residual risk of `allow_code` and of `class:` constructor code, and advise care with untrusted configs.
- [ ] 10.2 Write reference documentation for all new exported functions and the config object (roxygen2, per repo skill).
- [ ] 10.3 Update `NEWS.md` with the **BREAKING** format change and migration pointer.

## 11. Tests + verification

- [ ] 11.1 Replace the `test_config_*` suite with tests aligned to the unified schema across reader, interpolation, validation, resolver, application, and JSON-Schema conformance.
- [ ] 11.2 Run `devtools::document()` clean, then `devtools::check()`; resolve any new warnings/notes introduced by this change.
