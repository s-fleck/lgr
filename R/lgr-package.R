#' A Fully Featured Logging Framework for R
#'
#' For details please refer to `vignette("lgr", package = "lgr")`.
#'
#' @section Options:
#'
#' You can also set these options in your `.Rprofile` to make them permanent.
#' Some options can also be set via environment variables (The environment
#' variables are only used if the option is not set manually from R).
#'
#' \describe{
#'   \item{`lgr.colors`}{a `list` of `functions` used for coloring the log
#'     levels in console output. Usually these will be functions from the
#'     package **crayon**}
#'   \item{`lgr.log_levels`}{A named `integer` vector of log levels that are
#'     known to lgr for labeling, setting thresholds, etc... . Instead of
#'     modifying this option manually use [add_log_levels()] and
#'     [remove_log_levels()]}
#'  \item{`lgr.default_threshold`}{
#'    `character` or `integer` scalar. The minimum [log level][log_levels] that
#'    should be processed by the root logger. Defaults to `400` (`"info"`),
#'    or to the value of the environment variable `LGR_DEFAULT_THRESHOLD`
#'    if it is set. This option overrides the threshold specified in
#'    `lgr.default_config` if both are set.
#'  }
#'  \item{`lgr.default_config`}{
#'    Default configuration for the root logger. Can either be a special list
#'    object, a path to a YAML file, or a character scalar containing YAML
#'    code. See [logger_config] for details. Defaults to the value of the
#'    environment variable `LGR_DEFAULT_CONFIG` if it is set.
#'  }
#'  \item{`lgr.suspend_logging`}{`TRUE` or `FALSE`. Suspend all logging for
#'    all loggers. Defaults to the `TRUE` if the environment variable
#'    `LGR_SUSPEND_LOGGING` is set to `"TRUE"`. Instead of modifying this
#'    option manually use [suspend_logging()] and [unsuspend_logging()]}
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
    dyn_register_s3_method("data.table", "as.data.table", "LogEvent")
    dyn_register_s3_method("tibble", "as_tibble", "LogEvent")


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

  # +- defaults from env vars --------------------------------------------------
    op.this[["lgr.suspend_logging"]] <- identical(Sys.getenv("LGR_SUSPEND_LOGGING"), "TRUE")

    default_config <- Sys.getenv("LGR_DEFAULT_CONFIG")
    if (is_blank(default_config)) {
      default_config <- NULL

    } else {
      if (!is_scalar_character(default_config)){
        warning("Environment variable 'LGR_DEFAULT_CONFIG' is set to an illegal value")
      } else {
        default_config <- tryCatch(
          as_logger_config(default_config),
          error = function(e){
            warning(
              "Environment variable 'LGR_DEFAULT_CONFIG' is set but '", default_config,
              "' does not seem to be a valid logger_config"
            )
            NULL
          }
        )
      }
    }

    op.this[["lgr.default_config"]]    <- default_config

    default_threshold <- Sys.getenv("LGR_DEFAULT_THRESHOLD")

    if (!is_blank(default_threshold)){
      default_threshold <- tryCatch(
        standardize_threshold(tolower(default_threshold)),
        error = function(e){
          warning(
            "Environment variable 'LGR_DEFAULT_THRESHOLD' is set but '", config,
            "' does not seem to be a valid threshold"
          )
          NULL
        }
      )
    } else {
      default_threshold <- NULL
    }


    op.this[["lgr.default_threshold"]] <- default_threshold


  # +- set options -------------------------------------------------------------
    toset <- !(names(op.this) %in% names(op))
    if(any(toset)) options(op.this[toset])



  # root looger -------------------------------------------------------------
  appenders <- list(console = AppenderConsole$new(threshold = NA))

  assign(
    "root",
    LoggerRoot$new("root", appenders = appenders, threshold = NA),  # threshold cannot be null
    envir = loggers
  )
  assign("lgr", get_logger("root"), envir = parent.env(environment()))

  if (!is.null(getOption("lgr.default_config"))){
    tryCatch(
      lgr$config(getOption("lgr.default_config")),
      error = function(e)
        warning("Option 'lgr.default_config' is set but does not seem to point ",
                "to a valid logger_config:", e$message)
    )
  }

  lgr$set_threshold(getOption("lgr.default_threshold", 400L))
}
