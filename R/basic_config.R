#' Basic Setup for the Logging System
#'
#' A quick and easy way to configure the root logger. This is less powerful
#' then using [`lgr$config()` or `lgr$set_*()`][Logger], but reduces the
#' most common configrations to a single line of code.
#'
#' @param file `character` scalar: If not `NULL` a [AppenderFile] will be
#'   created that logs to this file. If the filename ends in `.jsonl`, the
#'   Appender will be set up to use the [JSON
#'   Lines](http://jsonlines.org/) format instead of plain text (see
#'   [AppenderFile] and [AppenderJson]).
#' @param fmt `character` scalar: Format to use if `file` is supplied and not a
#'   `.jsonl` file. If `NULL` it defaults to `"%L [%t] %m"` (see
#'   [format.LogEvent])
#' @param console_fmt `character` scalar: like `fmt` but used for console output
#' @param console_timestamp_fmt `character` scalar: like `timestamp_fmt` but
#'   used for console output
#' @inheritParams print.LogEvent
#' @inheritParams Logger
#' @param appenders a single [Appender] or a list thereof.
#' @param threshold `character` or `integer` scalar. The minimum [log
#'   level][log_levels] that should be processed by the root logger.
#' @param memory `logical` scalar. or a `threshold` (see above). Add an Appender
#'   that logs to a memory buffer, see also [show_log()] and [AppenderBuffer]
#' @param console `logical` scalar or a `threshold` (see above). Add an appender
#'   logs to the console (i.e. displays messages in an interactive R session)
#'
#' @return the `root` Logger (lgr)
#' @export
#'
#' @examples
#' # log to a file
#' basic_config(file = tempfile())
#' unlink(lgr$appenders$file$file)  # cleanup
#'
#  # log to a JSON file
#' basic_config(file = tempfile(fileext = "jsonl"))
#' unlink(lgr$appenders$file$file)  # cleanup
#'
#' # log debug messages to a memory buffer
#' basic_config(threshold = "all", memory = "all", console = "info")
#' lgr$info("an info message")
#' lgr$debug("a hidden message")
#' show_log()
#'
#' # reset to default config
#' basic_config()
basic_config <- function(
  file = NULL,
  fmt = "%L [%t] %m",
  timestamp_fmt = "%Y-%m-%d %H:%M:%OS3",
  threshold = "info",
  appenders = NULL,
  console = if (is.null(appenders)) "all" else FALSE,
  console_fmt = "%L [%t] %m %f",
  console_timestamp_fmt = "%H:%M:%OS3",
  memory  = FALSE
){
  stopifnot(
    is.null(file) || is_scalar_character(file),
    is_scalar_character(fmt),
    is_scalar_character(console_fmt),
    is_scalar_character(timestamp_fmt),
    is_threshold(threshold),
    is_scalar_bool(console) || is_threshold(console),
    is_scalar_bool(memory) || is_threshold(console),
    is.null(appenders) || is.list(appenders) || inherits(appenders, "Appender")
  )

  l <-
    get_logger()$
    config(NULL)$
    set_threshold(threshold)


  if (length(appenders)){
    assert(
      is.null(file)    || !"file" %in% names(appenders),
      "If `appenders` contains an appender named `file`, the `file` argument to basic_config() must be `NULL`"
    )
    assert(
      isFALSE(console) || !"console" %in% names(appenders),
      "If `appenders` contains an appender named `console`, the `console` argument to basic_config() must be `FALSE`"
    )
    assert(
      isFALSE(memory)  || !"memory" %in% names(appenders),
      "If `appenders` contains an appender named `memory`, the `memory` argument to basic_config() must be `FALSE`"
    )

    l$set_appenders(appenders)
  }


  if (!is.null(file)){
    ext <- tools::file_ext(file)

    if (identical(tolower(ext), "json")){
      stop(
        "Please use `.jsonl` and not `.json` as file extension for JSON log",
        "files. The reason is that that JSON files created",
        "by lgr are not true JSON files but JSONlines files.",
        "See http://jsonlines.org/ for more infos."
      )

    } else if (identical(tolower(ext), "jsonl")){
      assert (is.null(fmt), "`fmt` must be null if `file` is a '.jsonl' file")
      l$add_appender(
        name = "file",
        AppenderJson$new(threshold = NA)
      )

    } else {
      l$add_appender(
        name = "file",
        AppenderFile$new(
          file = file,
          threshold = NA,
          layout = LayoutFormat$new(
            fmt = fmt,
            timestamp_fmt = timestamp_fmt
          )
        )
      )
    }
  }


  if (!isFALSE(console)){
    if (isTRUE(console)) console <- 400
    l$add_appender(
      name = "console",
      AppenderConsole$new(
        threshold = console,
        layout = LayoutFormat$new(
          colors = getOption("lgr.colors"),
          fmt = console_fmt,
          timestamp_fmt = console_timestamp_fmt
        )
      )
    )
  }

  if  (!isFALSE(memory)){
    if (isTRUE(memory)) memory <- NA
    l$add_appender(name = "memory", AppenderBuffer$new(
      threshold = memory,
      should_flush = function(event) FALSE
    ))
  }


  lgr
}
