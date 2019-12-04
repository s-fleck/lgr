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

  # dyn s3 methods ------------------------------------------------------
  dyn_register_s3_method("data.table", "as.data.table", "LogEvent")
  dyn_register_s3_method("data.table", "as.data.table", "event_list")
  dyn_register_s3_method("tibble", "as_tibble", "LogEvent")


  # options -------------------------------------------------------------
  op <- options()
  op.this <- list()



  # +- colors --------------------------------------------------------------
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

    # make color functions available inside the package
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


  # +- options from envars -------------------------------------------------
    op.this[["lgr.suspend_logging"]]   <- get_envar_suspend_logging()
    op.this[["lgr.default_config"]]    <- get_envar_default_config()
    op.this[["lgr.default_threshold"]] <- get_envar_default_threshold(fallback = 400)


  # +- set options (if they were not set manually before) ------------------
    toset <- !(names(op.this) %in% names(op))
    if(any(toset)) options(op.this[toset])



  # root looger -------------------------------------------------------------
    assign(
      "root",
      LoggerRoot$new("root", threshold = NA),  # threshold cannot be null
      envir = loggers
    )
    assign(
      "lgr",
      get_logger("root"),
      envir = parent.env(environment())
    )

    # config
    default_config <- getOption("lgr.default_config")

    if (is.null(default_config)){
      basic_config()

    } else {
      tryCatch(
        lgr$config(default_config),
        error = function(e) {
          warning(
            "The option 'lgr.default_config' is set to an invalid logger_config: ",
            e$message, call. = FALSE
          )
        }
      )
    }

  lgr$set_threshold(getOption("lgr.default_threshold", 400L))
}




# get environment variables -----------------------------------------------

get_envar_suspend_logging <- function(){
  envar_suspend_logging <- toupper(Sys.getenv("LGR_SUSPEND_LOGGING"))

  if (identical(envar_suspend_logging, "TRUE")){
    TRUE
  } else if (
    identical(envar_suspend_logging, "FALSE") ||
    is_blank(envar_suspend_logging)
  ){
    FALSE
  } else {
    warning(
      "Environment variable 'LGR_SUSPEND_LOGGING' is set to '", envar_suspend_logging,
      "' but must be either 'TRUE' or 'FALSE'",  call. = FALSE)
    FALSE
  }
}




get_envar_default_config <- function(){
  envar_default_config <- Sys.getenv("LGR_DEFAULT_CONFIG")

  if (is_blank(envar_default_config)){
    NULL
  } else {
    if (!is_scalar_character(envar_default_config)){
      warning("Environment variable 'LGR_DEFAULT_CONFIG' must be a path to a YAML or JSON config file", call. = FALSE)
      NULL
    } else {
      tryCatch(
        as_logger_config(envar_default_config),
        error = function(e){
          warning(
            "Environment variable 'LGR_DEFAULT_CONFIG' is set but '", envar_default_config,
            "' is not a path to a valid YAML file", call. = FALSE
          )
          NULL
        }
      )
    }
  }
}




get_envar_default_threshold <- function(fallback = 400){
  envvar_default_threshold <- tolower(Sys.getenv("LGR_DEFAULT_THRESHOLD"))

  if (!is_scalar(envvar_default_threshold)){
    warning(
      "Environment variable 'LGR_DEFAULT_THRESHOLD' bust be a single",
      "numeric or character value.", call. = FALSE
    )
    return(fallback)
  }

  int_threshold <- suppressWarnings(as.integer(envvar_default_threshold))

  if (!is.na(int_threshold))
      envvar_default_threshold <- int_threshold

  if (is_blank(envvar_default_threshold)){
    fallback
  } else {
    tryCatch(
      standardize_threshold(envvar_default_threshold),
      error = function(e){
        warning(
          "Environment variable 'LGR_DEFAULT_THRESHOLD' is set but '", envvar_default_threshold,
          "' is not a valid threshold", call. = FALSE
        )
        fallback
      }
    )
  }
}
