# R roxygen2 Documentation Skill Guide

Use this guide to write consistent, idiomatic roxygen2 documentation for R packages.
Targets **roxygen2 >= 8.0.0**.

## 0. DESCRIPTION Configuration (roxygen2 8.0)

roxygen2 8.0 moved its configuration from the old `Roxygen:`/`RoxygenNote:`
fields to `Config/roxygen2/*` fields. Use the new fields; roxygen2 migrates
old ones automatically the first time you `document()`, but write new
`DESCRIPTION`s with the new form directly:

```
Config/roxygen2/markdown: TRUE
Config/roxygen2/version: 8.0.0
```

replaces the old:

```
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.3
```

Other `Roxygen: list(...)` options (e.g. `r6 = FALSE`, `roclets = ...`) move
the same way, e.g. `Config/roxygen2/r6: FALSE`.

**Gotcha (verified empirically against an installed roxygen2 8.0.0):** the
value must be the literal R boolean `TRUE`/`FALSE` (all-caps) — a lowercase
`true`/`false` is read as a plain string, silently fails to parse as logical,
and markdown rendering is skipped with **no error or warning**. The generated
`.Rd` files still build, but every `**bold**`, `` `code` ``, and `[link]`
comes through as raw unconverted markdown text instead of proper Rd markup.
If a `document()` run produces clean `.Rd` files that nonetheless look wrong
when previewed (literal asterisks/backticks/brackets in the rendered help),
check this field's casing first before suspecting anything else.

## Quick Agent Checklist

- Every exported function has `@param`, `@return`, and `@export`
- Every exported function has at least one `@examples` block (use `\dontrun{}` for side effects)
- Internal helpers usually don't have documentation. If they do (because they are complex or hard tounderstand) use `@noRd` (no Rd file) — never `@export`
- Related functions share a page via `@rdname`; secondary functions use `@inheritParams` or `@rdname` only
- Run `devtools::document()` after changes to regenerate `NAMESPACE` and `man/`
- Check `devtools::check()` for documentation warnings
- **R6 classes**: use roxygen2's native R6 support (see [Section 12](#12-r6-class-documentation)) — document fields with `@field` and methods with roxygen blocks placed directly above each method, not with a hand-rolled `@section Methods:` block

---

## 1. Basic Function Template

```r
#' Short one-line title (sentence case, no period)
#'
#' One or more paragraphs of description. The first paragraph is the
#' description. Additional paragraphs become the details section.
#'
#' @param x Description of `x`. Type annotation goes here, e.g.
#'   `character` scalar.
#' @param conn An Elasticsearch connection (see [elastic::connect()]).
#' @param ... Additional arguments passed to [other_function()].
#'
#' @return Description of the return value and its type/class.
#'
#' @seealso [related_function()], \url{https://example.com}
#'
#' @export
#'
#' @examples
#' my_function("hello")
my_function <- function(x, conn, ...) { }
```

---

## 2. Tags Reference

### Mandatory for exported functions

| Tag                | Purpose                   |
| ------------------ | ------------------------- |
| `@param name desc` | Document each parameter   |
| `@return`          | Describe the return value |
| `@export`          | Add to `NAMESPACE`        |

### Common optional tags

| Tag                        | Purpose                                               |
| -------------------------- | ----------------------------------------------------- |
| `@examples`                | Runnable examples (or `\dontrun{}`)                   |
| `@seealso`                 | Links to related functions/URLs                       |
| `@inheritParams source_fn` | Copy `@param` entries from another function           |
| `@rdname name`             | Group functions onto one help page                    |
| `@noRd`                    | Internal helper — no `.Rd` file generated             |
| `@keywords internal`       | Marks a topic as internal (used for package doc file) |

---

## 3. Parameter Descriptions

Use consistent type annotations at the start of each `@param`:

```r
#' @param index `character` scalar. Name of the Elasticsearch index.
#' @param batch_size `integer` scalar. Number of documents per batch.
#' @param conn An Elasticsearch connection (see [elastic::connect()]).
#' @param x A `data.frame` or `list`. The documents to be inserted.
#' @param log_success `logical` scalar. Write a log message on success?
#' @param ... Additional arguments passed to [elastic_docs_bulk_index()].
```

For parameters with multiple allowed types describe each:

```r
#' @param query A `list` or `character` scalar (JSON). The query to filter documents.
#' @param source
#'   - `TRUE` to return the entire document source.
#'   - `FALSE` to not return the document source.
#'   - `character` vector of source fields (supports wildcard `*` patterns).
```

---

## 4. Return Value Descriptions

```r
#' @return An `Elasticsearch` connection object (see [elastic::connect()]).
#' @return The body of the Elasticsearch response as a `list`.
#' @return A `POSIXct` vector of the same length as `x`.
#' @return `TRUE` invisibly on success.
```

Use `invisible()` in the function and document it:

```r
#' @return `response` (invisibly)
return(invisible(response))
```

---

## 5. Examples

### Side-effect free — run as-is

```r
#' @examples
#' connect_url("https://192.168.0.1:1234")
#' connect_url("https://192.168.0.1:1234/context/path")
#' parse_unix_timestamp_smart(1761820417)     # seconds
#' parse_unix_timestamp_smart(1761820417123)  # milliseconds
```

### Requires external resources — wrap in `\dontrun{}`

```r
#' @examples
#' \dontrun{
#' conn <- connect_url("https://my-cluster:9200", api_key = "abc123")
#' dump_index(conn, "my-index")
#' }
```

---

## 6. Grouping Functions with `@rdname`

When multiple functions share the same help page, put the full documentation on the primary function and use `@rdname` on the others. Add `@export` on every variant that should be exported.

```r
#' Primary function title
#'
#' Description of the family of functions.
#'
#' `elastic_docs_bulk_index()` is a simple bulk insert.
#' `elastic_docs_bulk_index_batched()` splits into batches.
#'
#' @param conn ...
#' @param index ...
#' @return ...
#' @export
#' @examples \dontrun{ elastic_docs_bulk_index(conn, df, "my-index") }
elastic_docs_bulk_index <- function(conn, x, index, ...) { }


#' @param batch_size `integer` scalar. Number of rows per batch.
#' @rdname elastic_docs_bulk_index
#' @export
elastic_docs_bulk_index_batched <- function(conn, x, index, batch_size = 10000L, ...) { }


#' @param group_vars `character` vector. Grouping columns in `x`.
#' @rdname elastic_docs_bulk_index
#' @export
elastic_docs_bulk_index_grouped <- function(conn, x, index, group_vars, ...) { }
```

---

## 7. `@inheritParams`

Avoid copy-pasting identical `@param` descriptions. Use `@inheritParams` to reuse them:

```r
#' @inheritParams elastic_docs_bulk_index
#' @inheritParams elastic::connect
```

Only the parameters that appear in the current function's signature are pulled in — unused params are silently ignored.

**roxygen2 8.0**: `@inheritParams` supports argument filtering — pull in only
specific params, or all-but-some:

```r
#' @inheritParams elastic_docs_bulk_index x y   # only x and y
#' @inheritParams elastic_docs_bulk_index -conn # everything except conn
```

---

## 8. Internal Helpers

Use `@noRd` for private helper functions. Do **not** add `@export`.

```r
#' Parse numeric epoch timestamps stored as character strings
#' @noRd
parse_numeric_string_timestamps <- function(x, result) { }
```

For the package-level documentation file (`R/<pkg>-package.R`), use `@keywords internal`:

```r
#' @keywords internal
#' @importFrom glue glue
"_PACKAGE"
```

---

## 9. Links and Cross-References

| Syntax              | Use for                                          |
| ------------------- | ------------------------------------------------ |
| `[other_fn()]`      | Function in this package                         |
| `[pkg::other_fn()]` | Function in another package                      |
| `\url{https://...}` | External URL                                     |
| `\code{value}`      | Inline code (prefer backticks in modern roxygen) |

```r
#' @seealso
#' - \url{https://www.elastic.co/docs/api/doc/elasticsearch/operation/operation-bulk}
#' - [elastic::connect()]
#' - [connect_url()]
```

---

## 10. NAMESPACE Imports

- Do not add or remove namespace imports. Leave modifying imports up to the user. 
- If you do add imports because the user asks you to, add them at the package 
  level in `R/<pkg>-package.R`.

- Exceptions: Commonly used packages you may add yourself without being asked:
  - `glue`

Examples:

```r
#' @import glue 
```

---

## 11. What Not to Do

- Do **not** add `@export` to internal helpers — use `@noRd`
- Do **not** duplicate `@param` descriptions across `@rdname` siblings — use `@inheritParams` or put them on the primary function only
- Do **not** leave `@return` empty — always describe the return value
- Do **not** write examples that require network access without `\dontrun{}`
- Do **not** use `@importFrom <package> <function>`. always prefer `<package>::<function>` syntax.
- Do **not** document parameters that don't exist in the function signature

---

## 12. R6 Class Documentation

roxygen2 has **native support for `R6::R6Class` objects**: fields, active
bindings, and methods are documented in-line, right next to their definition,
instead of in one big hand-written block above the class. This package targets
roxygen2 8.0 and no `Config/roxygen2/r6: false` opt-out is set in
`DESCRIPTION`, so native R6 handling is active — use it for all R6 classes,
including when updating existing ones.

roxygen2 validates that every public method, field, active binding, and method
argument is documented, and warns on `devtools::document()`/`devtools::check()`
if something is missing.

### 12.1 Class-level documentation

Put the roxygen block directly above the `R6::R6Class()` call, same as for a
function. roxygen2 automatically adds a superclass section (with links) and
collects every method's `@examples` into one combined, checkable Examples
section — you do **not** need to (and should not) write your own `@section
Methods:` block or manually list methods.

```r
#' Loggers
#'
#' A Logger produces a [LogEvent] that contains a log message along with
#' metadata and dispatches it to one or more [Appenders].
#'
#' @export
Logger <- R6::R6Class(
  "Logger",
  public = list(
    # ... fields and methods, each documented in-line (see below) ...
  )
)
```

`@include`, `@seealso`, `@examples` (class-level usage examples), etc. all
still work as on any other roxygen block.

### 12.2 Fields and active bindings — `@field`

Document each public field or active binding with `@field name description`
directly above its definition, inside `public = list(...)` / `active =
list(...)`:

```r
public = list(
  #' @field name First or full name of the person.
  name = NULL,

  #' @field threshold `character` or `integer` scalar. The minimum log level
  #'   this Logger will process.
  threshold = NULL
),
active = list(
  #' @field last_event The last [LogEvent] processed by this Logger
  #'   (read-only).
  last_event = function() { ... }
)
```

- A field inherited from a superclass automatically inherits its parent's
  `@field` documentation — don't redocument it unless the description differs.
- To deliberately hide an inherited field from the subclass's docs, use
  `@field name NULL`.
- `private` fields are not part of the public API and are not documented.

### 12.3 Methods

Document each method (including `initialize`) with a roxygen block directly
above it, using structured tags only — a bare leading paragraph (like a
function's title/description) is **not** supported for methods; use explicit
`@description`:

```r
public = list(
  #' @description
  #' Create a new Logger.
  #' @param name `character` scalar. The Logger's name.
  #' @param threshold `character` or `integer` scalar. Minimum log level.
  #' @returns A new `Logger` object.
  initialize = function(name, threshold = NA) { ... },

  #' @description
  #' Change the Logger's threshold.
  #' @param level `character` or `integer` scalar. The new threshold.
  #' @returns The Logger object, invisibly (for chaining).
  set_threshold = function(level) { ... }
)
```

Supported method tags: `@description`, `@details`, `@param`, `@returns`
(preferred over `@return` for R6 methods, matches roxygen2's own convention),
and `@examples`. All rendered method docs are folded into the class's single
combined "Methods" section — you never write that section header yourself.

**Parameter resolution order** — if a method parameter has no `@param` in its
own block, roxygen2 looks for one, in order:

1. A class-level `@param` tag (on the block above `R6Class()`)
2. A matching `@field` tag, for `initialize()`'s parameters only (so a
   constructor arg that just sets a same-named field doesn't need its own
   `@param`) — `initialize()` picks up `@field` docs automatically
3. The same-named parameter documented on the same method in a parent class
   (works automatically for overridden/extended methods as of roxygen2 8.0 —
   a subclass method that adds one new parameter only needs to document
   *that* parameter, not repeat the inherited ones)

If none of these resolve it, `document()`/`check()` warns. Use `@noRd` on a
method to suppress its docs (and the warnings) entirely — e.g. for
internal-only public methods that shouldn't appear in the man page.

**roxygen2 8.0 R6 improvements worth relying on:**

- Fields, active bindings, *and* method parameter docs are all inherited from
  superclasses — don't redocument a parameter a parent method already
  documents unless the meaning changed for the subclass.
- `\dontrun{}`/`\donttest{}`/`\dontshow{}` wrappers are stripped automatically
  when method `@examples` are folded into the class's combined Examples
  section, so wrap example code in these as needed at the method level
  without worrying about leftover wrapper markup in the rendered page.
- The constructor is rendered as `ClassName$new(args)` in the Usage section;
  every other method renders as `obj$method(args)` — no manual `@usage`
  needed.
- The auto-generated "Super classes" section drops the `pkg::` prefix for
  parent classes defined in the same package (only cross-package parents show
  the prefix).
- `@description`/`@details` on a method support markdown headings (e.g. `##
  Subheading`) if you need structure within a single method's docs.

### 12.4 Methods added via `Class$set()`

If a method is added to the class after the initial `R6Class()` call (via
`$set()`), put the roxygen block directly above the `$set()` call — roxygen2
still discovers it by introspection:

```r
#' @description
#' Print a human-readable representation of the Logger.
Logger$set("public", "print", function() { ... })
```

### 12.5 `@R6method` — methods roxygen2 can't discover

For the rare case where a method is constructed in a way roxygen2's static
introspection can't follow (e.g. assembled programmatically, or `$set()`
called inside a loop/helper), document it explicitly with `@R6method
Class$method` on a standalone block:

```r
#' @R6method Logger$set_threshold
#' @description
#' Change the Logger's threshold.
#' @param level `character` or `integer` scalar. The new threshold.
NULL
```

Prefer letting roxygen2 auto-discover methods (12.2–12.4); reach for
`@R6method` only when introspection genuinely fails.

### 12.6 What NOT to do (superseded pre-7.3 patterns)

Older lgr code (and pre-7.3 R6 documentation generally) used hand-written
patterns to work around the lack of native support. When touching an existing
R6 class, migrate these away rather than adding to them:

- Do **not** write a manual `#' @section Methods:` block with a bullet list of
  method signatures — roxygen2 now generates this from the in-line `@field`/
  method docs.
- Do **not** put all documentation in one big comment block above
  `R6Class()` with prose describing every field/method — move each piece of
  documentation next to the field/method it documents.
- Do **not** use `@format` / `@docType class` to mark up an R6 class — that
  pattern predates R6-aware roxygen2 and is for base R6 documented as a plain
  object; the native support in 12.1 replaces it.
- Do **not** duplicate a parameter's description on every method that shares
  it — rely on the parameter-resolution order in 12.3 (class-level `@param`,
  or `@field`, or the parent class's method) instead of repeating text.
- Do **not** add `Config/roxygen2/r6: false` to `DESCRIPTION` — that reverts
  the whole package to legacy (roxygen2 6.x-style) R6 handling and is not
  set here; don't introduce it without an explicit reason.
