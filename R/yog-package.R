#'
#'
#' @section Options:
#'
#' \describe{
#'   \item{`yog.colors`}{a `list` of `functions` used for coloring the log
#'     levels in console output. Usually these will be functions from the
#'     packages **crayon** or **colt**}
#'   \item{`yog.log_levels`}{A named integer vector of log levels that are
#'     known to yog for labeling, setting thresholds, etc... . Instead of
#'     modifying this option manually use [add_log_levels()] and
#'     [remove_log_levels()]}
#'  \item{`yog.suspend_logging`}{`TRUE` or `FALSE`. Suspend all logging for
#'    all loggers.  Instead of modifying this option manually use
#'    [suspend_logging()] and [unsuspend_log_levels()]}
#' }
#'
#' @keywords internal
#' @importFrom stats setNames
"_PACKAGE"




#' @export yog
.onLoad <- function(...){
  if (requireNamespace("crayon", quietly = TRUE)){

    options(
      yog.colors = list(
        "fatal" = style_fatal,
        "error" = style_error,
        "warn"  = style_warning,
        "debug" = style_subtle,
        "trace" = style_subtle
      )
    )
  } else {
    options(yog.colors = list())
  }

  options(
    yog.log_levels = c(
      "fatal" = 100L,
      "error" = 200L,
      "warn"  = 300L,
      "info"  = 400L,
      "debug" = 500L,
      "trace" = 600L
    )
  )

  appenders <- list(
    console = AppenderConsole$new()
  )

  if (requireNamespace("data.table", quietly = TRUE)){
    appenders[["memory"]] <- AppenderMemoryDt$new()
  }

  assign(
    "yog",
    Logger$new(
      name = "root",    # usally a logger should have the same name as the package it belongs to
      appenders = appenders,
      parent = NULL  # _never_ set the parent to null yoursel, root should be the only root logger
    ),
    envir = asNamespace("yog")
  )
}
