#' Declarative Logger Configuration
#'
#' @description
#' lgr can be configured from a single canonical, versioned configuration
#' format (`version: 1`) that can express the *entire* logger hierarchy: the
#' root logger, descendant loggers, and reusable named `layouts`, `filters`
#' and `appenders`. Use [Logger$config()][Logger] (usually via `lgr$config()`)
#' to apply such a config, or [read_config()]/[validate_config()] to check a
#' config without applying it.
#'
#' @section Top-level structure:
#' A config document is a mapping with an optional `version` (defaults to
#' `1`; no other value is currently supported), an optional
#' `disable_existing_loggers` flag, and the optional sections `layouts`,
#' `filters`, `appenders` and `loggers`. `loggers` is a mapping from logger
#' name (`root` for the root logger, `/`-separated names such as `app/db` for
#' descendants) to a logger definition supporting `class` (`"Logger"` or
#' `"LoggerGlue"`), `threshold`, `appenders`, `filters`, `propagate` and
#' `exception_handler`. `layouts`/`filters`/`appenders` are mappings from a
#' user-chosen component name to a component definition: a mapping with a
#' `class` key (e.g. `class: AppenderConsole`, optionally namespaced as
#' `pkg::ClassName`) plus the remaining keys as constructor arguments.
#' Component names can be referenced by name wherever a `layouts`/`filters`/
#' `appenders` value is expected (in another component, or in a logger); such
#' references are resolved once and the resulting object is shared. Appenders
#' and filters can also be defined inline instead of referenced by name.
#'
#' Function-valued positions (a logger's `exception_handler`, and filter
#' entries) accept one of: a **name reference** (a bare `character` scalar
#' for `exception_handler`, or an explicit `{ ref: <name> }`), resolved by
#' lookup (`<pkg>::<name>` supported); an **inline code** form
#' (`{ code: "<R source>" }`), the only form that evaluates config-supplied
#' source and is therefore rejected unless the loader is called with
#' `allow_code = TRUE`; and, for filters only, a `{ class: <FilterClass>,
#' ... }` spec (constructing an R6 [EventFilter]) or a bare-string reference
#' to a named `filters` component.
#'
#' @section Applying a config is a full snapshot, not a patch:
#' A logger definition under `loggers` fully determines that logger's
#' `threshold`, `appenders`, `filters`, `propagate` and `exception_handler`:
#' any of these keys that are *not* given for a listed logger are reset to
#' their default (no threshold/inherited, no appenders, no filters,
#' `propagate: true`, the default exception handler) rather than left as they
#' were before the config was applied. This makes re-applying the same
#' declarative config (or switching between config files) reproducible and
#' independent of whatever state a Logger happened to be in beforehand. Note
#' that this differs from the plain (non-declarative) `list` form accepted by
#' [Logger$config()][Logger], which only touches the elements it is given and
#' leaves everything else untouched.
#'
#' @section `disable_existing_loggers`:
#' If `TRUE`, every pre-existing non-root logger *not* named under `loggers`
#' is reset to its default/unconfigured state before the rest of the config
#' is applied (loggers that *are* named under `loggers` are unaffected by
#' this flag; they always end up in the state the config describes for them,
#' per the previous section). Defaults to `FALSE`, so orphaned loggers are
#' left alone unless you opt in.
#'
#' **Resetting is not the same as silencing.** A reset logger gets
#' `propagate = TRUE` and no appenders/threshold of its own, i.e. it now
#' inherits its ancestors' appenders and threshold. If a logger was
#' deliberately muted (e.g. `propagate: false` with no appenders) before the
#' reset, this can make it *more* verbose afterwards, not less -- it starts
#' forwarding events to whichever ancestor appenders it now inherits from.
#'
#' @section Environment variable interpolation:
#' Scalar string values may contain `${VAR}` (substituted with the value of
#' environment variable `VAR`) or `${VAR:-default}` (using `default` when
#' `VAR` is unset or empty). A literal `${...}` can be produced with the
#' escape `$${...}`. A value produced *entirely* by interpolation is
#' leniently coerced (number-like to `numeric`, `true`/`false` to `logical`),
#' so `size: ${SIZE}` behaves like a literal `size: 1048576`. Interpolation
#' happens before validation and is never applied to `code:` payloads.
#'
#' @section Safety:
#' Loading a config never evaluates config-supplied R source by default.
#' Only the `code:` form does so, and it is rejected during validation
#' unless the caller passes `allow_code = TRUE`. All other forms (name/`ref`
#' references, `class` specs, named-component references) are data, not
#' source, and are always available.
#'
#' @name config
#' @seealso [Logger], `vignette("configuring-lgr", package = "lgr")`
#' @include Logger.R get_logger.R default_functions.R
NULL




# public API ----------------------------------------------------------------

#' Read and validate an lgr configuration
#'
#' Parses, interpolates and validates a declarative lgr configuration without
#' applying it to any logger. Useful for checking a config file (e.g. in CI)
#' or for programmatically inspecting a config before applying it.  See
#' [config] for the format.
#'
#' @param x a `character` scalar (a file path, or a YAML/JSON string), or a
#'   `list` following the config format described in [config].
#' @param allow_code `TRUE` or `FALSE`. Whether to permit the inline `code:`
#'   form for `exception_handler` and filter entries, which evaluates
#'   config-supplied R source. Defaults to `FALSE` (safe mode).
#'
#' @return an object of class `lgr_config` (a validated, interpolated
#'   nested `list`), invisibly usable with [Logger$config()][Logger].
#' @export
#'
#' @examples
#' cfg <- read_config(list(loggers = list(root = list(threshold = "warn"))))
#' validate_config(list(loggers = list(root = list(threshold = "warn"))))
read_config <- function(x, allow_code = FALSE) {
  assert(is_scalar_bool(allow_code))
  raw <- read_config_source(x)
  assert(
    is_full_config_list(raw),
    "A declarative lgr config must be a mapping containing at least one of ",
    "`loggers`, `layouts`, `version`, or `disable_existing_loggers` at the ",
    "top level. If you meant to pass an already-constructed `list` of ",
    "appenders/filters for a single Logger, use `Logger$config()` directly."
  )
  parsed <- interpolate_config(raw)
  validate_config_list(parsed, allow_code = allow_code)
  structure(parsed, class = c("lgr_config", "list"))
}




#' @rdname read_config
#' @export
#' @return `validate_config()` returns `TRUE` (invisibly) if `x` is valid,
#'   and throws an informative error otherwise.
validate_config <- function(x, allow_code = FALSE) {
  read_config(x, allow_code = allow_code)
  invisible(TRUE)
}




#' Location of the shipped lgr configuration JSON Schema
#'
#' lgr ships a [JSON Schema](https://json-schema.org/) describing its
#' declarative configuration format (see [config]) for use by editors (e.g.
#' the VS Code YAML/JSON Schema extensions) to provide autocompletion and
#' inline validation.
#'
#' @return a `character` scalar: the path to the installed JSON Schema file.
#' @export
#' @examples
#' config_schema_file()
config_schema_file <- function() {
  system.file("lgr-config-schema.json", package = "lgr")
}




# read ------------------------------------------------------------------

read_config_source <- function(x) {
  if (is.list(x)) {
    return(x)
  }

  assert(
    is_scalar_character(x),
    "`x` must be a file path, a YAML/JSON string, or a `list`, not ",
    class_fmt(x)
  )

  if (!grepl("\n", x) && file.exists(x)) {
    return(parse_config_file(x))
  }

  parse_config_text(x)
}




parse_config_file <- function(file) {
  ext <- tolower(tools::file_ext(file))

  if (identical(ext, "json")) {
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      return(jsonlite::fromJSON(file, simplifyVector = FALSE))
    }
    assert_namespace("yaml")
    return(yaml::read_yaml(file))
  }

  if (ext %in% c("yaml", "yml")) {
    assert_namespace("yaml")
    return(yaml::read_yaml(file))
  }

  parse_config_text(paste(readLines(file, warn = FALSE), collapse = "\n"))
}




parse_config_text <- function(text) {
  if (looks_like_json(text)) {
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      return(jsonlite::fromJSON(text, simplifyVector = FALSE))
    }
    assert_namespace("yaml")
    return(yaml::read_yaml(text = text))
  }

  assert_namespace("yaml")
  yaml::read_yaml(text = text)
}




looks_like_json <- function(x) {
  grepl("^\\s*[\\{\\[]", x)
}




is_full_config_list <- function(x) {
  is.list(x) &&
    any(c("version", "loggers", "layouts", "disable_existing_loggers") %in% names(x))
}




# interpolate -------------------------------------------------------------

INTERP_VAR_RE <- "\\$\\{([A-Za-z_][A-Za-z0-9_]*)(:-([^}]*))?\\}"
INTERP_FULL_RE <- paste0("^", INTERP_VAR_RE, "$")
INTERP_ESCAPE_RE <- "\\$(\\$\\{[^}]*\\})"


interpolate_config <- function(x, in_code = FALSE) {
  if (in_code) {
    return(x)
  }

  if (is.list(x)) {
    nms <- names(x) %||% rep("", length(x))
    for (i in seq_along(x)) {
      x[[i]] <- interpolate_config(x[[i]], in_code = identical(nms[[i]], "code"))
    }
    return(x)
  }

  if (is.character(x)) {
    if (identical(length(x), 1L)) {
      # a scalar value may be fully coerced to a non-character type (see
      # `coerce_interpolated()`)
      return(interpolate_string(x))
    }

    # elements of a multi-element character vector (e.g. a bare list of
    # appender/filter name references) stay character even if a single
    # element would otherwise be eligible for numeric/boolean coercion
    return(vapply(x, function(s) {
      r <- interpolate_string(s)
      if (!is.character(r)) as.character(r) else r
    }, character(1), USE.NAMES = FALSE))
  }

  x
}




interpolate_string <- function(s) {
  # protect escaped `$${...}` sequences from substitution
  escaped <- character(0)
  protect <- function(m) {
    literal <- substring(m, 2) # drop the leading escaping "$"
    escaped[[length(escaped) + 1]] <<- literal
    sprintf("ESCAPEDDOLLAR%dESCAPEDDOLLAR", length(escaped))
  }

  s2 <- s
  repeat {
    m <- regmatches(s2, regexpr(INTERP_ESCAPE_RE, s2, perl = TRUE))
    if (!length(m) || !nzchar(m)) break
    s2 <- sub(INTERP_ESCAPE_RE, protect(m), s2, perl = TRUE)
  }

  full_match <- grepl(INTERP_FULL_RE, s2, perl = TRUE)

  s3 <- s2
  repeat {
    m <- regmatches(s3, regexpr(INTERP_VAR_RE, s3, perl = TRUE))
    if (!length(m) || !nzchar(m)) break

    parts <- regmatches(m, regexec(INTERP_VAR_RE, m, perl = TRUE))[[1]]
    var <- parts[[2]]
    has_default <- nzchar(parts[[3]])
    default <- parts[[4]]

    val <- Sys.getenv(var, unset = NA_character_)
    if (is.na(val) || !nzchar(val)) {
      if (has_default) {
        val <- default
      } else {
        stop(ConfigInterpolationError(sprintf(
          "Environment variable '%s' is not set and no default was given.",
          var
        )))
      }
    }

    s3 <- sub(INTERP_VAR_RE, gsub("\\\\", "\\\\\\\\", val, perl = TRUE), s3, perl = TRUE)
  }

  # restore escaped literals
  for (i in seq_along(escaped)) {
    s3 <- sub(sprintf("ESCAPEDDOLLAR%dESCAPEDDOLLAR", i), escaped[[i]], s3, fixed = TRUE)
  }

  if (full_match && !length(escaped)) {
    return(coerce_interpolated(s3))
  }

  s3
}




coerce_interpolated <- function(x) {
  if (grepl("^-?[0-9]+(\\.[0-9]+)?$", x)) {
    return(as.numeric(x))
  }
  if (tolower(x) %in% c("true", "false")) {
    return(toupper(x) == "TRUE")
  }
  x
}




ConfigInterpolationError <- function(message) {
  error(message, "ConfigInterpolationError")
}




# validate ------------------------------------------------------------------

CONFIG_TOP_LEVEL_KEYS <- c(
  "version", "disable_existing_loggers", "layouts", "filters", "appenders", "loggers"
)
CONFIG_LOGGER_KEYS <- c(
  "class", "threshold", "appenders", "filters", "propagate", "exception_handler"
)


validate_config_list <- function(x, allow_code = FALSE) {
  errs <- new_config_errors()

  if (!is.list(x)) {
    errs$add("<root>", paste0("Config must be a mapping (named list), not ", class_fmt(x)))
    errs$throw_if_any()
    return(invisible(TRUE))
  }

  for (k in setdiff(names(x), CONFIG_TOP_LEVEL_KEYS)) {
    errs$add(k, sprintf("Unknown top-level key '%s'.", k), did_you_mean(k, CONFIG_TOP_LEVEL_KEYS))
  }

  if (!is.null(x[["version"]])) {
    v <- x[["version"]]
    if (!(is_scalar(v) && isTRUE(suppressWarnings(as.numeric(v)) == 1))) {
      errs$add("version", sprintf("Only 'version: 1' is supported, not '%s'.", preview_object(v)))
    }
  }

  if (!is.null(x[["disable_existing_loggers"]])) {
    if (!is_scalar_bool(x[["disable_existing_loggers"]])) {
      errs$add("disable_existing_loggers", "Expected a boolean (true/false).")
    }
  }

  defs <- list(
    layouts = x[["layouts"]] %||% list(),
    filters = x[["filters"]] %||% list(),
    appenders = x[["appenders"]] %||% list()
  )

  for (type in c("layouts", "filters", "appenders")) {
    section <- defs[[type]]
    if (length(section) && is.null(names(section))) {
      errs$add(type, sprintf("'%s' must be a named mapping of component definitions.", type))
      next
    }
    for (nm in names(section)) {
      validate_component_def(
        section[[nm]], paste0(type, ".", nm), defs, allow_code, errs,
        is_filter_type = identical(type, "filters")
      )
    }
  }

  loggers <- x[["loggers"]] %||% list()
  if (length(loggers) && is.null(names(loggers))) {
    errs$add("loggers", "'loggers' must be a named mapping from logger name to logger definition.")
  }
  for (nm in names(loggers)) {
    validate_logger_def(loggers[[nm]], paste0("loggers.", nm), defs, allow_code, errs, is_root = identical(nm, "root"))
  }

  validate_component_graph(defs, errs)

  errs$throw_if_any()
  invisible(TRUE)
}




validate_component_def <- function(def, path, defs, allow_code, errs, is_filter_type = FALSE) {
  if (!is.list(def) || is.null(def[["class"]])) {
    errs$add(path, "A component definition must be a mapping containing a `class` key.")
    return(invisible())
  }

  if (!is_scalar_character(def[["class"]])) {
    errs$add(paste0(path, ".class"), "`class` must be a character scalar.")
  }

  if (!is.null(def[["layout"]])) {
    validate_layout_field(def[["layout"]], paste0(path, ".layout"), defs, errs)
  }
  if (!is.null(def[["filters"]])) {
    validate_filters_field(def[["filters"]], paste0(path, ".filters"), defs, allow_code, errs)
  }
  if (!is.null(def[["appenders"]])) {
    validate_appenders_field(def[["appenders"]], paste0(path, ".appenders"), defs, allow_code, errs)
  }

  invisible()
}




validate_layout_field <- function(x, path, defs, errs) {
  if (is_scalar_character(x)) {
    if (!x %in% names(defs$layouts)) {
      errs$add(path, sprintf("Unknown layout reference '%s'.", x), did_you_mean(x, names(defs$layouts)))
    }
  } else if (is.list(x) && !is.null(x[["class"]])) {
    if (!is_scalar_character(x[["class"]])) {
      errs$add(paste0(path, ".class"), "`class` must be a character scalar.")
    }
  } else {
    errs$add(path, "`layout` must be a name reference or an inline definition with `class`.")
  }
  invisible()
}




validate_appenders_field <- function(x, path, defs, allow_code, errs) {
  entries <- normalize_config_entries(x)
  multi <- length(entries) > 1 || is.list(x)

  for (i in seq_along(entries)) {
    e <- entries[[i]]
    p <- if (multi) sprintf("%s[%d]", path, i - 1L) else path

    if (is_scalar_character(e)) {
      if (!e %in% names(defs$appenders)) {
        errs$add(p, sprintf("Unknown appender reference '%s'.", e), did_you_mean(e, names(defs$appenders)))
      }
    } else if (is.list(e)) {
      validate_component_def(e, p, defs, allow_code, errs, is_filter_type = FALSE)
    } else {
      errs$add(p, "Appender entries must be a name reference or an inline component definition.")
    }
  }
  invisible()
}




validate_filters_field <- function(x, path, defs, allow_code, errs) {
  entries <- normalize_config_entries(x)
  multi <- length(entries) > 1 || is.list(x)

  for (i in seq_along(entries)) {
    e <- entries[[i]]
    p <- if (multi) sprintf("%s[%d]", path, i - 1L) else path
    validate_function_value(e, p, defs, allow_code, errs, is_filter_context = TRUE)
  }
  invisible()
}




validate_function_value <- function(x, path, defs, allow_code, errs, is_filter_context) {
  if (is.list(x)) {
    if (!is.null(x[["class"]])) {
      if (!is_filter_context) {
        errs$add(path, "`class` specs are only valid for filters.")
        return(invisible())
      }
      validate_component_def(x, path, defs, allow_code, errs, is_filter_type = TRUE)
      return(invisible())
    }

    if (!is.null(x[["code"]])) {
      if (!is_scalar_character(x[["code"]])) {
        errs$add(paste0(path, ".code"), "`code` must be a character scalar of R source.")
      }
      if (!allow_code) {
        errs$add(path, sprintf(paste(
          "'%s' uses inline R code (`code:`), which is disabled by default",
          "for safety. Pass `allow_code = TRUE` to allow it."
        ), path))
      }
      return(invisible())
    }

    if (!is.null(x[["ref"]])) {
      if (!is_scalar_character(x[["ref"]])) {
        errs$add(paste0(path, ".ref"), "`ref` must be a character scalar naming a function.")
      }
      return(invisible())
    }

    errs$add(path, "Expected a mapping with one of `class`, `code`, or `ref`.")
    return(invisible())
  }

  if (is_scalar_character(x)) {
    if (is_filter_context && !x %in% names(defs$filters)) {
      errs$add(path, sprintf("Unknown filter reference '%s'.", x), did_you_mean(x, names(defs$filters)))
    }
    return(invisible())
  }

  errs$add(path, "Invalid value.")
}




validate_logger_def <- function(def, path, defs, allow_code, errs, is_root = FALSE) {
  if (!is.list(def)) {
    errs$add(path, "A logger definition must be a mapping.")
    return(invisible())
  }

  for (k in setdiff(names(def), CONFIG_LOGGER_KEYS)) {
    errs$add(paste0(path, ".", k), sprintf("Unknown key '%s' in logger definition.", k), did_you_mean(k, CONFIG_LOGGER_KEYS))
  }

  if (!is.null(def[["class"]])) {
    if (!is_scalar_character(def[["class"]]) || !def[["class"]] %in% c("Logger", "LoggerGlue")) {
      errs$add(paste0(path, ".class"), sprintf("Logger `class` must be 'Logger' or 'LoggerGlue', not '%s'.", preview_object(def[["class"]])))
    } else if (is_root && identical(def[["class"]], "LoggerGlue")) {
      errs$add(paste0(path, ".class"), "The root logger cannot be a 'LoggerGlue'.")
    }
  }

  if (!is.null(def[["threshold"]]) && !is_threshold(def[["threshold"]])) {
    errs$add(paste0(path, ".threshold"), sprintf("Invalid threshold '%s'.", preview_object(def[["threshold"]])))
  }

  if (!is.null(def[["propagate"]]) && !is_scalar_bool(def[["propagate"]])) {
    errs$add(paste0(path, ".propagate"), "Expected a boolean (true/false).")
  }

  if (!is.null(def[["appenders"]])) {
    validate_appenders_field(def[["appenders"]], paste0(path, ".appenders"), defs, allow_code, errs)
  }
  if (!is.null(def[["filters"]])) {
    validate_filters_field(def[["filters"]], paste0(path, ".filters"), defs, allow_code, errs)
  }
  if (!is.null(def[["exception_handler"]])) {
    validate_function_value(def[["exception_handler"]], paste0(path, ".exception_handler"), defs, allow_code, errs, is_filter_context = FALSE)
  }

  invisible()
}




validate_component_graph <- function(defs, errs) {
  validate_reference_graph(defs$appenders, "appenders", errs)
  validate_reference_graph(defs$filters, "filters", errs)
}




validate_reference_graph <- function(components, field, errs) {
  adj <- lapply(components, function(def) referenced_component_names(def[[field]], field))
  names(adj) <- names(components)

  visited <- character(0)

  dfs <- function(node, path_stack) {
    if (node %in% path_stack) {
      cyc <- c(path_stack[seq(which(path_stack == node), length(path_stack))], node)
      errs$add(
        paste0(field, ".", node),
        sprintf("Cyclic %s reference detected: %s", field, paste(cyc, collapse = " -> "))
      )
      return(invisible())
    }
    if (node %in% visited) {
      return(invisible())
    }
    visited[[length(visited) + 1]] <<- node

    for (nb in adj[[node]] %||% character(0)) {
      if (nb %in% names(adj)) {
        dfs(nb, c(path_stack, node))
      }
    }
  }

  for (nm in names(adj)) {
    dfs(nm, character(0))
  }
  invisible()
}




# `field` is "appenders" or "filters": the field within a component definition
# that may hold further references of the same kind, so that self-references
# among named `appenders`/`filters` components can be detected before resolving.
referenced_component_names <- function(x, field) {
  if (is.null(x)) {
    return(character(0))
  }
  if (is.character(x)) {
    return(x)
  }
  if (is.list(x)) {
    if (!is.null(x[["class"]])) {
      return(referenced_component_names(x[[field]], field))
    }
    if (!is.null(x[["code"]]) || !is.null(x[["ref"]])) {
      return(character(0))
    }
    out <- character(0)
    for (e in x) {
      if (is_scalar_character(e)) {
        out <- c(out, e)
      } else if (is.list(e)) {
        out <- c(out, referenced_component_names(e, field))
      }
    }
    return(out)
  }
  character(0)
}




normalize_config_entries <- function(x) {
  if (is.character(x)) {
    return(as.list(x))
  }
  if (is.list(x) && any(c("class", "code", "ref") %in% names(x))) {
    # a single component/function-value spec, not a list of entries to iterate
    return(list(x))
  }
  if (is.list(x)) {
    return(x)
  }
  list(x)
}




# error collection ----------------------------------------------------------

new_config_errors <- function() {
  self <- new.env()
  self$items <- list()

  self$add <- function(path, message, suggestion = NULL) {
    if (!is.null(suggestion)) {
      message <- paste0(message, " Did you mean '", suggestion, "'?")
    }
    self$items[[length(self$items) + 1L]] <- list(path = path, message = message)
    invisible()
  }

  self$throw_if_any <- function() {
    if (!length(self$items)) {
      return(invisible())
    }
    lines <- vapply(
      self$items,
      function(it) sprintf("  - [%s] %s", it$path, it$message),
      character(1)
    )
    stop(ConfigValidationError(paste(
      c(sprintf("Invalid lgr config (%d issue(s)):", length(self$items)), lines),
      collapse = "\n"
    )))
  }

  self
}




ConfigValidationError <- function(message) {
  error(message, "ConfigValidationError")
}




did_you_mean <- function(x, choices) {
  choices <- setdiff(choices, x)
  if (!length(choices)) {
    return(NULL)
  }
  d <- utils::adist(x, choices)[1, ]
  best <- choices[[which.min(d)]]
  if (min(d) <= max(2, floor(nchar(x) / 3))) {
    best
  } else {
    NULL
  }
}




# resolve ---------------------------------------------------------------

resolve_config <- function(parsed, allow_code = FALSE) {
  defs <- list(
    layouts = parsed[["layouts"]] %||% list(),
    filters = parsed[["filters"]] %||% list(),
    appenders = parsed[["appenders"]] %||% list()
  )

  resolvers <- new_config_resolvers(defs, allow_code)

  # eagerly resolve every named component so that definition errors surface
  # even for components that no logger currently references
  for (type in c("layouts", "filters", "appenders")) {
    for (nm in names(defs[[type]])) {
      resolvers$resolve_named(type, nm)
    }
  }

  logger_plans <- list()
  loggers_cfg <- parsed[["loggers"]] %||% list()

  for (nm in names(loggers_cfg)) {
    ldef <- loggers_cfg[[nm]] %||% list()

    logger_plans[[nm]] <- list(
      class = ldef[["class"]] %||% "Logger",
      threshold = if (!is.null(ldef[["threshold"]])) standardize_threshold(ldef[["threshold"]]) else NULL,
      appenders = if (!is.null(ldef[["appenders"]])) {
        resolve_appenders_field(ldef[["appenders"]], defs, allow_code, resolvers)
      } else {
        list()
      },
      filters = if (!is.null(ldef[["filters"]])) {
        resolve_filters_field(ldef[["filters"]], defs, allow_code, resolvers)
      } else {
        list()
      },
      propagate = if (!is.null(ldef[["propagate"]])) isTRUE(ldef[["propagate"]]) else TRUE,
      exception_handler = if (!is.null(ldef[["exception_handler"]])) {
        resolve_function_value(ldef[["exception_handler"]], defs, allow_code, resolvers, is_filter_context = FALSE)
      } else {
        NULL
      }
    )
  }

  list(
    disable_existing_loggers = isTRUE(parsed[["disable_existing_loggers"]]),
    loggers = logger_plans
  )
}




new_config_resolvers <- function(defs, allow_code) {
  cache <- list(appenders = list(), layouts = list(), filters = list())
  stack <- list(appenders = character(0), layouts = character(0), filters = character(0))

  self <- new.env()

  self$resolve_named <- function(type, name) {
    if (!is.null(cache[[type]][[name]])) {
      return(cache[[type]][[name]])
    }

    assert(name %in% names(defs[[type]]), "Unknown `", type, "` reference: '", name, "'")
    assert(!(name %in% stack[[type]]), "Cyclic reference detected in `", type, "`: ", name)

    stack[[type]] <<- c(stack[[type]], name)
    res <- instantiate_component(defs[[type]][[name]], defs, allow_code, self)
    stack[[type]] <<- setdiff(stack[[type]], name)

    cache[[type]][[name]] <<- res
    res
  }

  self
}




instantiate_component <- function(def, defs, allow_code, resolvers) {
  ctor <- get0_R6Class(def[["class"]])
  assert(!is.null(ctor), "Could not resolve R6 constructor '", def[["class"]], "'.")

  args <- def[setdiff(names(def), "class")]

  if (!is.null(args[["layout"]])) {
    args[["layout"]] <- resolve_layout_field(args[["layout"]], defs, allow_code, resolvers)
  }
  if (!is.null(args[["filters"]])) {
    args[["filters"]] <- resolve_filters_field(args[["filters"]], defs, allow_code, resolvers)
  }
  if (!is.null(args[["appenders"]])) {
    args[["appenders"]] <- resolve_appenders_field(args[["appenders"]], defs, allow_code, resolvers)
  }

  do.call(ctor$new, args)
}




resolve_layout_field <- function(x, defs, allow_code, resolvers) {
  if (is_scalar_character(x)) {
    return(resolvers$resolve_named("layouts", x))
  }
  instantiate_component(x, defs, allow_code, resolvers)
}




resolve_appenders_field <- function(x, defs, allow_code, resolvers) {
  entries <- normalize_config_entries(x)
  res <- lapply(entries, function(e) {
    if (is_scalar_character(e)) {
      resolvers$resolve_named("appenders", e)
    } else {
      instantiate_component(e, defs, allow_code, resolvers)
    }
  })
  names(res) <- vapply(entries, function(e) if (is_scalar_character(e)) e else "", character(1))
  res
}




resolve_filters_field <- function(x, defs, allow_code, resolvers) {
  entries <- normalize_config_entries(x)
  lapply(entries, function(e) {
    resolve_function_value(e, defs, allow_code, resolvers, is_filter_context = TRUE)
  })
}




resolve_function_value <- function(x, defs, allow_code, resolvers, is_filter_context) {
  if (is.function(x)) {
    return(x)
  }

  if (is.list(x)) {
    if (!is.null(x[["class"]])) {
      return(instantiate_component(x, defs, allow_code, resolvers))
    }

    if (!is.null(x[["code"]])) {
      assert(allow_code, "Inline `code:` requires `allow_code = TRUE`.")
      fn <- eval(parse(text = x[["code"]]))
      assert(is.function(fn), "`code` must evaluate to a `function`.")
      return(fn)
    }

    if (!is.null(x[["ref"]])) {
      fn <- get0_function(x[["ref"]])
      assert(!is.null(fn), "Could not resolve function reference '", x[["ref"]], "'.")
      return(fn)
    }

    stop("Invalid function-valued config entry.")
  }

  if (is_scalar_character(x)) {
    if (is_filter_context) {
      return(resolvers$resolve_named("filters", x))
    }
    fn <- get0_function(x)
    assert(!is.null(fn), "Could not resolve function reference '", x, "'.")
    return(fn)
  }

  stop("Invalid function-valued config entry.")
}




get0_R6Class <- function(x) {
  assert(is_scalar_character(x))

  if (grepl("::", x, fixed = TRUE)) {
    parts <- strsplit(x, "::", fixed = TRUE)[[1]]
    assert(length(parts) == 2L, "Invalid namespaced constructor: '", x, "'")
    pkg <- parts[[1]]
    cls <- parts[[2]]
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required for constructor '", x, "'")
    }
    res <- get0(cls, envir = asNamespace(pkg), inherits = FALSE)

    if (R6::is.R6Class(res)) {
      return(res)
    }
    return(NULL)
  }

  res <- get0(x, envir = parent.env(environment()))
  if (R6::is.R6Class(res)) {
    return(res)
  }

  for (ns in loadedNamespaces()) {
    res <- get0(x, envir = asNamespace(ns), inherits = FALSE)
    if (R6::is.R6Class(res)) {
      return(res)
    }
  }

  NULL
}




get0_function <- function(x) {
  assert(is_scalar_character(x))

  if (grepl("::", x, fixed = TRUE)) {
    parts <- strsplit(x, "::", fixed = TRUE)[[1]]
    assert(length(parts) == 2L, "Invalid namespaced function reference: '", x, "'")
    pkg <- parts[[1]]
    nm <- parts[[2]]
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required for function reference '", x, "'")
    }
    res <- get0(nm, envir = asNamespace(pkg), inherits = FALSE)
    if (is.function(res)) {
      return(res)
    }
    return(NULL)
  }

  res <- get0(x, envir = parent.env(environment()))
  if (is.function(res)) {
    return(res)
  }

  for (ns in loadedNamespaces()) {
    res <- get0(x, envir = asNamespace(ns), inherits = FALSE)
    if (is.function(res)) {
      return(res)
    }
  }

  NULL
}




# apply -----------------------------------------------------------------

apply_config <- function(resolved) {
  if (isTRUE(resolved$disable_existing_loggers)) {
    for (nm in setdiff(ls(envir = loggers), c("root", names(resolved$loggers)))) {
      get_logger(nm)$config(NULL)
    }
  }

  for (nm in names(resolved$loggers)) {
    plan <- resolved$loggers[[nm]]
    want_glue <- identical(plan$class, "LoggerGlue")

    if (identical(nm, "root")) {
      lg <- get_logger("root")
    } else {
      cur <- get_logger(nm)
      if (want_glue != inherits(cur, "LoggerGlue")) {
        lg <- get_logger(nm, class = if (want_glue) LoggerGlue else Logger, reset = TRUE)
      } else {
        lg <- cur
      }
    }

    if (!is.null(plan$threshold)) {
      lg$set_threshold(plan$threshold)
    } else if (identical(nm, "root")) {
      # LoggerRoot$set_threshold() rejects `NULL`; fall back to its own default
      lg$set_threshold(getOption("lgr.default_threshold", 400L))
    } else {
      lg$set_threshold(NULL)
    }
    lg$set_appenders(plan$appenders)
    lg$set_propagate(plan$propagate)
    lg$set_filters(plan$filters)
    lg$set_exception_handler(plan$exception_handler %||% default_exception_handler)
  }

  invisible(get_logger("root"))
}
