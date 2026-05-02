#' Logger Configuration Objects
#'
#' `logger_config()` is an S3 constructor for `logger_config` objects
#' that can be passed to the `$config` method of a [Logger]. You
#' can just pass a normal `list` instead, but using this constructor is
#' a more formal way that includes additional argument checking.
#'
#'
#' @param appenders see [Logger]
#' @param threshold see [Logger]
#' @param filters see [Logger]
#' @param exception_handler see [Logger]
#' @param propagate see [Logger]
#'
#' @return a `list` with the subclass `"logger_config"`
#'
logger_config <- function(
  appenders = NULL,
  threshold = NULL,
  filters = NULL,
  exception_handler = NULL,
  propagate = TRUE
) {
  # init/preconditions
  if (is.function(exception_handler)) {
    exception_handler <- deparse(exception_handler)
  } else if (!is.null(exception_handler)) {
    assert(
      is.character(exception_handler),
      "'exception_handler' must be a function, a character scalar giving the",
      "name of a function, or a character vector containing arbitrary R code."
    )
  }

  assert(is.null(threshold) || is_threshold(threshold))
  assert(is.null(appenders) || all(vapply(appenders, is.list, logical(1))))
  assert(is.null(filters) || all(vapply(filters, is.list, logical(1))))

  if (!is.null(propagate)) {
    assert(is_scalar(propagate))
    if (is.character(propagate)) {
      propagate <- toupper(propagate)
    }
    propagate <- as.logical(propagate)
    assert(is_scalar_bool(propagate))
  }

  cfg <- compact(list(
    appenders = appenders,
    threshold = threshold,
    filters = filters,
    exception_handler = exception_handler,
    propagate = propagate
  ))

  class(cfg) <- c("logger_config", "list")
  cfg
}


is_logger_config <- function(x) {
  inherits(x, "logger_config")
}


parsed_logger_config <- function(
  appenders = list(),
  threshold = NULL,
  filters = list(),
  exception_handler = default_exception_handler,
  propagate = TRUE
) {
  structure(
    list(
      appenders = standardize_appenders_list(appenders),
      threshold = threshold,
      filters = standardize_filters_list(filters),
      exception_handler = exception_handler,
      propagate = propagate
    ),
    class = c("parsed_logger_config", "list")
  )
}


is_parsed_logger_config <- function(x) {
  inherits(x, "parsed_logger_config")
}


parse_logger_config <- function(
  x,
  defaults = parsed_logger_config()
) {
  if (is_parsed_logger_config(x)) {
    return(x)
  } else {
    assert(all(names(x) %in% names(defaults)))
  }

  if (is_logger_config(x)) {
    objects <- resolve_r6_ctors(x)

    res <- defaults

    if ("appenders" %in% names(x)) {
      res$appenders <- standardize_appenders_list(objects$appenders)
    }

    if ("exception_handler" %in% names(x)) {
      res$exception_handler <- eval(parse(text = x[["exception_handler"]]))
    }

    if ("propagate" %in% names(x)) {
      res$propagate <- as.logical(toupper(x[["propagate"]]))
    }

    if ("threshold" %in% names(x)) {
      res$threshold <- standardize_threshold(x$threshold)
    }

    if ("filters" %in% names(x)) {
      res$filters <- standardize_filters_list(objects$filters)
    }

    class(res) <- c("parsed_logger_config", "list")
  } else {
    res <- do.call(parsed_logger_config, x)
  }

  res
}


#' `as_logger_config()` coerces any supported \R object to a `logger_config`
#' object. You usually do not have to call this function directly, as it is
#' automatically invoked by the `config()` method of Loggers (see examples)
#'
#' @param x any \R object. Especially:
#'   * A `character` scalar. This can either be the path to a
#'     YAML file or a character scalar containing valid YAML
#'   * a list containing the elements `appenders`, `threshold`, `exception_handler`,
#'     `propagate` and `filters`. See the section *Fields* in [Logger] for
#'     details.
#'   * a Logger object, to clone its configuration.
#'
#' @rdname logger_config
#' @seealso \url{https://yaml.org/}
#' @return a logger_config object
#' @export
#'
as_logger_config <- function(x) {
  UseMethod("as_logger_config")
}


#' @export
as_logger_config.NULL <- function(x) {
  as_logger_config(list())
}


#' @rdname logger_config
#' @export
as_logger_config.list <- function(x) {
  if (identical(names(x), "Logger")) {
    x <- x[["Logger"]]
  }

  assert(all(
    names(x) %in%
      c("exception_handler", "propagate", "threshold", "appenders", "filters")
  ))
  class(x) <- c("logger_config", class(x))
  x
}


#' @rdname logger_config
#' @export
as_logger_config.character <- function(
  x
) {
  dd <- read_serialized_logger_config(x)

  assert(
    identical(names(dd), "Logger"),
    "If 'x' is a YAML file, it must contain a single logger object"
  )

  as_logger_config(dd)
}


read_serialized_logger_config <- function(x) {
  assert(is.character(x))

  if (identical(length(x), 1L) && !grepl("\n", x)) {
    if (
      identical(tolower(tools::file_ext(x)), "json") &&
        requireNamespace("jsonlite", quietly = TRUE)
    ) {
      jsonlite::read_json(x, simplifyVector = TRUE)
    } else {
      assert(file.exists(x), "The file '", x, "' does not exist.")
      assert_namespace("yaml")
      yaml::read_yaml(file = x)
    }
  } else {
    assert_namespace("yaml")
    yaml::read_yaml(text = x)
  }
}


is_logging_config <- function(x) {
  inherits(x, "logging_config")
}


as_logging_config <- function(x) {
  if (!is.list(x)) {
    return(NULL)
  }

  if (identical(names(x), "lgr")) {
    x <- x[["lgr"]]
  }

  if (identical(names(x), "Logger")) {
    return(NULL)
  }

  has_logging_fields <- any(
    names(x) %in%
      c(
        "version",
        "root",
        "loggers",
        "layouts",
        "disable_existing_loggers"
      )
  )

  if (!has_logging_fields) {
    return(NULL)
  }

  if ("root" %in% names(x)) {
    x$loggers <- x$loggers %||% list()
    x$loggers$root <- x$root
    x$root <- NULL
  }

  x$version <- x$version %||% 1L
  x$appenders <- x$appenders %||% list()
  x$layouts <- x$layouts %||% list()
  x$filters <- x$filters %||% list()
  x$loggers <- x$loggers %||% list()
  x$disable_existing_loggers <- isTRUE(x$disable_existing_loggers)

  assert(is.list(x$appenders), "`appenders` in a logging config must be a list")
  assert(is.list(x$layouts), "`layouts` in a logging config must be a list")
  assert(is.list(x$filters), "`filters` in a logging config must be a list")
  assert(is.list(x$loggers), "`loggers` in a logging config must be a list")

  if (length(x$loggers)) {
    assert(
      !is.null(names(x$loggers)) && all(!is_blank(names(x$loggers))),
      "All entries in `loggers` must be named with logger names."
    )
  }

  structure(x, class = c("logging_config", "list"))
}


as_any_logger_config <- function(x) {
  if (is.character(x)) {
    dd <- read_serialized_logger_config(x)
    lc <- as_logging_config(dd)
    if (is_logging_config(lc)) {
      return(lc)
    }

    return(as_logger_config(dd))
  }

  if (is.list(x)) {
    lc <- as_logging_config(x)
    if (is_logging_config(lc)) {
      return(lc)
    }
  }

  as_logger_config(x)
}


apply_logging_config <- function(cfg, logger_name = "root") {
  assert(is_logging_config(cfg))
  assert(is_scalar_character(logger_name))

  if (isTRUE(cfg$disable_existing_loggers)) {
    for (nm in setdiff(ls(envir = loggers), "root")) {
      get_logger(nm)$config(NULL)
    }
  }

  defs <- list(
    appenders = cfg$appenders,
    layouts = cfg$layouts,
    filters = cfg$filters
  )

  cache <- list(
    appenders = list(),
    layouts = list(),
    filters = list()
  )

  stack <- list(
    appenders = character(),
    layouts = character(),
    filters = character()
  )

  resolve_named <- function(type, name) {
    assert(is_scalar_character(type))
    assert(is_scalar_character(name))
    assert(type %in% c("appenders", "layouts", "filters"))

    if (!is.null(cache[[type]][[name]])) {
      return(cache[[type]][[name]])
    }

    if (name %in% stack[[type]]) {
      stop("Cyclic reference detected in `", type, "`: ", name)
    }

    assert(
      name %in% names(defs[[type]]),
      "Unknown `",
      type,
      "` reference: '",
      name,
      "'"
    )

    stack[[type]] <<- c(stack[[type]], name)
    on.exit(
      {
        stack[[type]] <<- stack[[type]][stack[[type]] != name]
      },
      add = TRUE
    )

    spec <- defs[[type]][[name]]
    res <- resolve_component(type, spec)
    cache[[type]][[name]] <<- res
    res
  }

  resolve_component <- function(type, spec) {
    if (type == "layouts") {
      obj <- resolve_r6_ctors(spec)
      if (is.list(obj) && length(obj) == 1L && inherits(obj[[1]], "Layout")) {
        obj <- obj[[1]]
      }

      assert(
        inherits(obj, "Layout"),
        "Layout config does not resolve to a `Layout` object"
      )
      return(obj)
    }

    if (type == "filters") {
      if (
        is.character(spec) && is_scalar(spec) && spec %in% names(defs$filters)
      ) {
        return(resolve_named("filters", spec))
      }

      obj <- resolve_r6_ctors(spec)
      if (is.list(obj) && length(obj) == 1L && is_filter(obj[[1]])) {
        obj <- obj[[1]]
      }

      assert_filter(obj)
      return(obj)
    }

    if (type == "appenders") {
      if (
        is.character(spec) && is_scalar(spec) && spec %in% names(defs$appenders)
      ) {
        return(resolve_named("appenders", spec))
      }

      if (is.list(spec)) {
        if (
          "layout" %in%
            names(spec) &&
            is.character(spec$layout) &&
            is_scalar(spec$layout) &&
            spec$layout %in% names(defs$layouts)
        ) {
          spec$layout <- resolve_named("layouts", spec$layout)
        }

        if ("filters" %in% names(spec)) {
          spec$filters <- resolve_logger_filters(
            spec$filters,
            resolve_named,
            defs
          )
        }

        if ("appenders" %in% names(spec)) {
          spec$appenders <- resolve_logger_appenders(
            spec$appenders,
            resolve_named,
            defs
          )
        }
      }

      obj <- resolve_r6_ctors(spec)
      if (is.list(obj) && length(obj) == 1L && inherits(obj[[1]], "Appender")) {
        obj <- obj[[1]]
      }

      assert(
        inherits(obj, "Appender"),
        "Appender config does not resolve to an `Appender` object"
      )
      return(obj)
    }

    stop("Unknown config component type: ", type)
  }

  for (nm in names(cfg$layouts)) {
    resolve_named("layouts", nm)
  }
  for (nm in names(cfg$filters)) {
    resolve_named("filters", nm)
  }
  for (nm in names(cfg$appenders)) {
    resolve_named("appenders", nm)
  }

  for (nm in names(cfg$loggers)) {
    lcfg <- cfg$loggers[[nm]] %||% list()
    assert(is.list(lcfg), "Each logger config must be a list")

    if ("appenders" %in% names(lcfg)) {
      lcfg$appenders <- resolve_logger_appenders(
        lcfg$appenders,
        resolve_named,
        defs
      )
    }

    if ("filters" %in% names(lcfg)) {
      lcfg$filters <- resolve_logger_filters(lcfg$filters, resolve_named, defs)
    }

    if (
      "exception_handler" %in%
        names(lcfg) &&
        is.function(lcfg$exception_handler)
    ) {
      lcfg$exception_handler <- deparse(lcfg$exception_handler)
    }

    lcfg <- parse_logger_config(as_logger_config(lcfg))
    get_logger(nm)$config(lcfg)
  }

  if (!logger_name %in% names(cfg$loggers)) {
    get_logger(logger_name)
  }

  invisible(get_logger(logger_name))
}


resolve_logger_appenders <- function(x, resolve_named, defs) {
  if (is.null(x)) {
    return(list())
  }

  if (inherits(x, "Appender")) {
    return(list(x))
  }

  if (is.character(x)) {
    res <- lapply(x, function(nm) {
      if (nm %in% names(defs$appenders)) {
        resolve_named("appenders", nm)
      } else {
        stop("Unknown appender reference: '", nm, "'")
      }
    })
    names(res) <- x
    return(res)
  }

  assert(
    is.list(x),
    "`appenders` must be a list, a character reference, or NULL"
  )

  for (i in seq_along(x)) {
    cur <- x[[i]]
    if (is.character(cur) && is_scalar(cur) && cur %in% names(defs$appenders)) {
      x[[i]] <- resolve_named("appenders", cur)
    } else {
      x[[i]] <- resolve_r6_ctors(cur)
      if (
        is.list(x[[i]]) &&
          length(x[[i]]) == 1L &&
          inherits(x[[i]][[1]], "Appender")
      ) {
        x[[i]] <- x[[i]][[1]]
      }
    }
  }

  standardize_appenders_list(x)
}


resolve_logger_filters <- function(x, resolve_named, defs) {
  if (is.null(x)) {
    return(list())
  }

  if (is_filter(x)) {
    return(list(x))
  }

  if (is.character(x)) {
    res <- lapply(x, function(nm) {
      if (nm %in% names(defs$filters)) {
        resolve_named("filters", nm)
      } else {
        stop("Unknown filter reference: '", nm, "'")
      }
    })
    names(res) <- x
    return(res)
  }

  assert(is.list(x), "`filters` must be a list, a character reference, or NULL")

  for (i in seq_along(x)) {
    cur <- x[[i]]
    if (is.character(cur) && is_scalar(cur) && cur %in% names(defs$filters)) {
      x[[i]] <- resolve_named("filters", cur)
    } else {
      x[[i]] <- resolve_r6_ctors(cur)
      if (is.list(x[[i]]) && length(x[[i]]) == 1L && is_filter(x[[i]][[1]])) {
        x[[i]] <- x[[i]][[1]]
      }
    }
  }

  standardize_filters_list(x)
}


resolve_r6_ctors <- function(x) {
  if (
    !is.recursive(x) ||
      inherits(x, c("Appender", "Layout", "Logger", "EventFilter"))
  ) {
    return(x)
  }

  if (is_r6_ctor_spec(x)) {
    return(instantiate_r6_ctor_spec(x))
  }

  ctors <- lapply(names(x), get0_R6Class)

  for (i in seq_along(x)) {
    if (length(ctors) && !is.null(ctors[[i]])) {
      args <- resolve_r6_ctors(x[[i]])
      if (is.null(args)) {
        args <- list()
      }

      # prevent logger not named warning
      suppressWarnings(x[[i]] <- do.call(ctors[[i]]$new, args))
    } else {
      if (is.recursive(x[[i]])) {
        x[[i]] <- resolve_r6_ctors(x[[i]])
      } else {
        x[[i]] <- x[[i]]
      }
    }
  }

  x
}


get0_R6Class <- function(x) {
  assert(is_scalar_character(x))

  # support explicit namespaced constructors, e.g. "lgrExtra::AppenderSyslog"
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

  # Also search loaded namespaces to support extension packages such as lgrExtra
  for (ns in loadedNamespaces()) {
    res <- get0(x, envir = asNamespace(ns), inherits = FALSE)
    if (R6::is.R6Class(res)) {
      return(res)
    }
  }

  NULL
}


is_r6_ctor_spec <- function(x) {
  is.list(x) && any(c("class", "constructor") %in% names(x))
}


instantiate_r6_ctor_spec <- function(x) {
  cls <- x$class %||% x$constructor
  assert(
    is_scalar_character(cls),
    "`class` in constructor specs must be a character scalar"
  )

  args <- compact(c(
    x$args %||% x$params,
    x[setdiff(names(x), c("class", "constructor", "args", "params"))]
  ))

  if (length(args)) {
    args <- resolve_r6_ctors(args)
  }

  ctor <- get0_R6Class(cls)
  assert(!is.null(ctor), "Could not resolve R6 constructor: '", cls, "'")

  suppressWarnings(do.call(ctor$new, args))
}


standardize_appenders_list <- function(x) {
  if (is.null(x)) {
    return(list())
  }

  if (inherits(x, "Appender")) {
    x <- list(x)
  }

  assert(
    is.list(x) && all(vapply(x, inherits, TRUE, "Appender")),
    "'appenders' must either be a single Appender, a list thereof, or ",
    "NULL for no appenders."
  )

  x
}
