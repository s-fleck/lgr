#' A Fully Featured Logging Framework for R
#'
#' For details please refer to `vignette("lgr", package = "lgr")`.
#'
#' @section Options:
#'
#' You can also set these options in your `.Rprofile` to make them permanent
#'
#' \describe{
#'   \item{`lgr.colors`}{a `list` of `functions` used for coloring the log
#'     levels in console output. Usually these will be functions from the
#'     package **crayon**}
#'   \item{`lgr.log_levels`}{A named `integer` vector of log levels that are
#'     known to lgr for labeling, setting thresholds, etc... . Instead of
#'     modifying this option manually use [add_log_levels()] and
#'     [remove_log_levels()]}
#'  \item{`lgr.suspend_logging`}{`TRUE` or `FALSE`. Suspend all logging for
#'    all loggers.  Instead of modifying this option manually use
#'    [suspend_logging()] and [unsuspend_logging()]}
#'  \item{`lgr.user`}{a `character` scalar. The default username for
#'     `lgr::get_user()`.
#'   }
#' }
#'
#' @keywords internal
#' @importFrom stats setNames
"_PACKAGE"





#' @export lgr
NULL

.onLoad <- function(...){

  # options -----------------------------------------------------------------
  op <- options()
  op.this <- list()

  # dyn s3 methods ----------------------------------------------------------
  if (requireNamespace("data.table", quietly = TRUE)){
    dyn_register_s3_method("data.table", "as.data.table", "LogEvent")
    dyn_register_s3_method("tibble", "as_tibble", "LogEvent")
  }



  # +- colors ------------------------------------------------------------------
  if (requireNamespace("crayon", quietly = TRUE) && crayon::has_color()){

    style_error   <- crayon::make_style("#BB3333", colors = 256)
    style_fatal   <- function(...) style_error(crayon::bold(...))
    style_warning <- crayon::make_style("#EEBB50", colors = 256)
    style_subtle  <- crayon::make_style(grDevices::grey(0.5), grey = TRUE)
    style_accent  <- crayon::make_style("#ca2c92", colors = 256)

    col_nchar     <- crayon::col_nchar

    op.this[["lgr.colors"]] <- list(
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

    op.this[["lgr.colors"]] = list()
  }

  assign("style_fatal", style_fatal, envir = parent.env(environment()))
  assign("style_error", style_error, envir = parent.env(environment()))
  assign("style_warning", style_warning, envir = parent.env(environment()))
  assign("style_subtle", style_subtle, envir = parent.env(environment()))
  assign("style_accent", style_accent, envir = parent.env(environment()))
  assign("col_nchar", col_nchar, envir = parent.env(environment()))


  # +- log_levels --------------------------------------------------------------
  op.this[["lgr.log_levels"]] <- c(
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
  appenders <- list(console = AppenderConsole$new(threshold = 400L))
  if (requireNamespace("data.table", quietly = TRUE)){
    appenders[["memory"]] <- AppenderDt$new(layout = appenders$console$layout)
  }

  assign(
    "root",
    LoggerRoot$new("root", appenders = appenders, threshold = NA),
    envir = loggers
  )

  assign("lgr", get_logger("root"), envir = parent.env(environment()))
}
