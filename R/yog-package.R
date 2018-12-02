#'
#'
#' @section Options:
#'
#' You can also set these options in your `.Rprofile` to make them permanent
#'
#' \describe{
#'   \item{`yog.colors`}{a `list` of `functions` used for coloring the log
#'     levels in console output. Usually these will be functions from the
#'     package **crayon**}
#'   \item{`yog.log_levels`}{A named `integer` vector of log levels that are
#'     known to yog for labeling, setting thresholds, etc... . Instead of
#'     modifying this option manually use [add_log_levels()] and
#'     [remove_log_levels()]}
#'  \item{`yog.suspend_logging`}{`TRUE` or `FALSE`. Suspend all logging for
#'    all loggers.  Instead of modifying this option manually use
#'    [suspend_logging()] and [unsuspend_log_levels()]}
#'  \item{`yog.user`}{a `character` scalar. The default username for new
#'    Loggers. Users can also be set per Logger, though this rarely makes
#'    sense.
#'   }
#' }
#'
#' @keywords internal
#' @importFrom stats setNames
"_PACKAGE"




#' @export yog
.onLoad <- function(...){

  # options -----------------------------------------------------------------
  op <- options()
  op.this <- list()



  # +- colors ------------------------------------------------------------------
  if (requireNamespace("crayon", quietly = TRUE)){

    style_error   <- crayon::make_style("#BB3333", colors = 256)
    style_fatal   <- function(x) style_error(crayon::bold(x))
    style_warning <- crayon::make_style("#EEBB50", colors = 256)
    style_subtle  <- crayon::make_style(grDevices::grey(0.5), grey = TRUE)
    style_accent  <- crayon::silver
    col_nchar     <- crayon::col_nchar

    op.this[["yog.colors"]] = list(
      "fatal" = style_fatal,
      "error" = style_error,
      "warn"  = style_warning,
      "debug" = style_subtle,
      "trace" = style_subtle
    )

  } else {
    style_fatal   <- function(...) paste(...)
    style_error   <- style_fatal
    style_warning <- style_fatal
    style_subtle  <- style_fatal
    style_accent  <- style_fatal
    col_nchar     <- function(...) nchar(...)

    op.this[["yog.colors"]] = list()
  }

  assign("style_fatal", style_fatal, envir = parent.env(environment()))
  assign("style_error", style_fatal, envir = parent.env(environment()))
  assign("style_warning", style_fatal, envir = parent.env(environment()))
  assign("style_subtle", style_fatal, envir = parent.env(environment()))
  assign("style_accent", style_fatal, envir = parent.env(environment()))
  assign("col_nchar", style_fatal, envir = parent.env(environment()))


  # +- log_levels --------------------------------------------------------------
  op.this[["yog.log_levels"]] <- c(
    "fatal" = 100L,
    "error" = 200L,
    "warn"  = 300L,
    "info"  = 400L,
    "debug" = 500L,
    "trace" = 600L
  )


  # +- set options -------------------------------------------------------------
    toset <- !(names(op.this) %in% names(op))
    if(any(toset)) options(op.this[toset])



  # root looger -------------------------------------------------------------
  appenders <- list(console = AppenderConsole$new())
  if (requireNamespace("data.table", quietly = TRUE))
    appenders[["memory"]] <- AppenderMemoryDt$new()

  assign(
    "yog",
    Logger$new(
      name = "root",    # usally a logger should have the same name as the package it belongs to
      appenders = appenders,
      parent = NULL  # _never_ set the parent to null yoursel, root should be the only root logger
    ),
    envir = parent.env(environment())
  )
}
