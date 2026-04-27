# R roxygen2 Documentation Skill Guide

Use this guide to write consistent, idiomatic roxygen2 documentation for R packages.

## Quick Agent Checklist

- Every exported function has `@param`, `@return`, and `@export`
- Every exported function has at least one `@examples` block (use `\dontrun{}` for side effects)
- Internal helpers usually don't have documentation. If they do (because they are complex or hard tounderstand) use `@noRd` (no Rd file) — never `@export`
- Related functions share a page via `@rdname`; secondary functions use `@inheritParams` or `@rdname` only
- Run `devtools::document()` after changes to regenerate `NAMESPACE` and `man/`
- Check `devtools::check()` for documentation warnings

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
