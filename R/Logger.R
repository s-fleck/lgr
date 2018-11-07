


#' A simple Logger.
#'
#' This is an R6 class that implements a concurrency safe package cache.
#'
#'
#' By default these fields are included for every package:
#' * `fullpath` Full package path.
#' * `path` Package path, within the repository.
#' * `package` Package name.
#' * `url` URL it was downloaded from.
#' * `etag` ETag for the last download, from the given URL.
#' * `md5` MD5 of the file, to make sure if it has not changed.
#'
#' Additional fields can be added as needed.
#'
#' For a simple API to a session-wide instance of this class, see
#' [pkg_cache_summary()] and the other functions listed there.
#'
#' @section Usage:
#' ```
#' pc <- package_cache$new(path = NULL)
#'
#' pc$list()
#' pc$find(..., .list = NULL)
#' pc$copy_to(..., .list = NULL)
#' pc$add(file, path, md5 = tools::md5sum(file)[[1]], ..., .list = NULL)
#' pc$add_url(url, path, ..., .list = NULL, on_progress = NULL)
#' pc$async_add_url(url, path, ..., .list = NULL, on_progress = NULL)
#' pc$copy_or_add(target, urls, path, md5 = NULL, ..., .list = NULL,
#'                on_progress = NULL)
#' pc$async_copy_or_add(target, urls, path, ..., md5 = NULL, ...,
#'                .list = NULL, on_progress = NULL)
#' pc$update_or_add(target, urls, path, ..., .list = NULL,
#'                on_progress = NULL)
#' pc$async_update_or_add(target, urls, path, ..., .list = NULL,
#'                on_progress = NULL)
#' pc$delete(..., .list = NULL)
#' ```
#'
#' @section Arguments:
#' * `path`: For `package_cache$new()` the location of the cache. For other
#'   functions the location of the file inside the cache.
#' * `...`: Extra attributes to search for. They have to be named.
#' * `.list`: Extra attributes to search for, they have to in a named list.
#' * `file`:  Path to the file to add.
#' * `url`: URL attribute. This is used to update the file, if requested.
#' * `md5`: MD5 hash of the file.
#' * `on_progress`: Callback to create progress bard. Passed to
#'   [http_get()].
#' * `target`: Path to copy the (first) to hit to.
#' * `urls`: Character vector or URLs to try to download the file from.
#'
#' @section Methods:
#'
#' `Logger$new()` initializes a new logger
#'
#' \describe{
#'   \item{name}{`character` scalar. Name of the Logger. Must be unique amongst
#'     Loggers. If you define a logger for an R Package, the logger should have
#'     the same name as the Package.}
#'   \item{appenders}{`list` of [Appender]s. The appenders used by this logger
#'     to write log entries to the console, to files, etc...}
#'   \item{threshold}{`character` or `integer` scalar. The minimum log level
#'     that triggers this logger}
#'   \item{user}{`character` scalar. The current user name or email adress.
#'     This information can be used by the appenders}
#'   \item{log_levels}{named `integer` vector. The log levels supported by this
#'     Logger. It is not recommended to modify the default log levels and this
#'     feature is still experimental. Please file an issue in the yog issue
#'     tracker if you require this feature.}
#'   \item{parent}{a `Logger`. Usually the Root logger. All Loggers must be
#'     descentents of the Root logger for yog to work as intended.}
#'
#'   \item{string_formatter}{a `function` used to format the log strings passed
#'     to the logging functions (`fatal()`, `error()`, etc...). Defaults to
#'     [base::sprintf()]. Another sensible choice wuld be [glue::glue()].}
#'
#'   \item{handle_exception}{a `function` that takes a single argument `e`.
#'     The function used to handle errors that occur durring loging. Default
#'     to demoting any error to a [warning]}
#'  }
#'
#'
#'
#'
#' `pc$list()` lists all files in the cache, returns a tibble with all the
#' default columns, and potentially extra columns as well.
#'
#' `pc$find()` list all files that match the specified criteria (`fullpath`,
#' `path`, `package`, etc.). Custom columns can be searched for as well.
#'
#' `pc$copy_to()` will copy the first matching file from the cache to
#' `target`. It returns the tibble of _all_ matching records, invisibly.
#' If no file matches, it returns an empty (zero-row) tibble.
#'
#' `pc$add()` adds a file to the cache.
#'
#' `pc$add_url()` downloads a file and adds it to the cache.
#'
#' `pc$async_add_url()` is the same, but it is asynchronous.
#'
#' `pc$copy_or_add()` works like `pc$copy_to()`, but if the file is not in
#' the cache, it tries to download it from one of the specified URLs first.
#'
#' `pc$async_copy_or_add()` is the same, but asynchronous.
#'
#' `pc$update_or_add()` is like `pc$copy_to_add()`, but if the file is in
#' the cache it tries to update it from the urls, using the stored ETag to
#' avoid unnecessary downloads.
#'
#' `pc$async_update_or_add()` is the same, but it is asynchronous.
#'
#' `pc$delete()` deletes the file(s) from the cache.
#'
#' @name Logger
#' @include Filterable.R
#' @examples
#'
#' ## Although package_cache usually stores packages, it may store
#' ## arbitrary files, that can be search by metadata
#' pc <- package_cache$new(path = tempfile())
#' pc$list()
#'
#' cat("foo\n", file = f1 <- tempfile())
#' cat("bar\n", file = f2 <- tempfile())
#' pc$add(f1, "/f1")
#' pc$add(f2, "/f2")
#' pc$list()
#' pc$find(path = "/f1")
#' pc$copy_to(target = f3 <- tempfile(), path = "/f1")
#' readLines(f3)
#'
#' @export
Logger <- R6::R6Class(
  "Logger",
  inherit = Filterable,

  # public methods --------------------------------------------------------
  public = list(
    initialize = function(
      name,
      appenders = list(),
      threshold = NA,
      user = get_user(),
      log_levels = getOption("yog.log_levels"),
      parent = yog::yog,
      string_formatter = sprintf,
      handle_exception = function(e){
        warning(
          "[", format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS3"), "] ",
          "An error occured in the logging sub system: ", e
        )
      },
      propagate = TRUE
    ){

      # fields ------------------------------------------------------------
      # active
      self$log_levels <- log_levels
      self$threshold <- threshold
      self$appenders <- appenders
      self$user  <- user
      self$string_formatter <- string_formatter
      self$handle_exception <- handle_exception
      self$parent <- parent
      self$name <- name
      self$last_event <- LogRecord$new(self)
      self$propagate <- propagate


      # init log functions ----------------------------------------------
      make_logger <- function(
        level,
        ...
      ){
        force(level)
        function(msg, ...){
          self$log(
            msg = private$.string_formatter(msg, ...),
            caller = get_caller(-4L),
            level = level
          )
        }
      }

      for (i in seq_along(private$.log_levels)){
        nm  <- names(private$.log_levels)[[i]]
        lvl <- private$.log_levels[[i]]

        if (nm %in% names(self)){
          stop(
            "The following names are not allowed for log levels: ",
            paste(sort(names(self)), collapse= ", ")
          )
        }

        self[[nm]] <- make_logger(lvl)
      }

      invisible(self)
    },


    log = function(
      level,
      timestamp = Sys.time(),
      caller = get_caller(-3),
      msg
    ){
      force(caller)

      tryCatch({
        assign("level", level, envir = self$last_event)
        assign("timestamp", timestamp,  envir = self$last_event)
        assign("caller", caller, envir = self$last_event)
        assign("msg", msg, envir = self$last_event)
        #assign(".Logger", self, envir = self$last_event)


        if (self$filter(self$last_event)){
          for (app in c(self$appenders, self$ancestral_appenders)) {
            if (app$filter(self$last_event)){
              app$append(self$last_event)
            }
          }
        }
      },
      error = self$handle_exception
      )
    },


    handle_exception = NULL,


    add_appender = function(appender, name = NULL){
      assert(inherits(appender, "Appender"))
      appender <- appender$clone()
      appender$logger <- self
      private$.appenders[length(private$.appenders) + 1L] <- list(appender)

      if (!is.null(name))
        names(private$.appenders)[length(private$.appenders)] <- name

      invisible(self)
    },

    remove_appender = function(pos){
      if (is.numeric(pos)){
        assert(
          all(pos %in% seq_along(private$.appenders)),
          "'pos' is out of range of the length of appenders (1:",
          length(appenders), ")"
        )

        pos <- as.integer(pos)
      } else if (is.character(pos)) {
        assert(
          all(pos %in% names(private$.appenders)),
          "'pos' is not names of appenders (",
          paste(names(private$.appenders), collapse = ", "),
          ")"
        )
      }

      private$.appenders[pos] <- NULL
      invisible(self)
    },

    suspend = function(){
      if (length(private$suspended_loggers) > 0){
        warning("Logger is already suspended")
      } else {
        for (i in seq_along(self$log_levels)){
          nm  <- names(self$log_levels)[[i]]
          lvl <- self$log_levels[[i]]
          private$suspended_loggers[[nm]] <- self[[nm]]
          self[[nm]] <- function(...) NULL
        }
      }
    },


    unsuspend = function(){
      if (length(private$suspended_loggers) < 1){
        warning("Logger is not suspended")
      }

      for (i in seq_along(private$.log_levels)){
        nm  <- names(private$.log_levels)[[i]]
        lvl <- private$.log_levels[[i]]
        self[[nm]] <- private$suspended_loggers[[nm]]
      }
      private$suspended_loggers <- list()
    },


    show = function(
      n = 20,
      threshold = NA
    ){
      sel <- vapply(self$appenders, inherits, TRUE, "AppenderMemoryDt")

      if (!any(sel)){
        message("This logger has no memory appender (see ?AppenderMemoryDt)")
        return(invisible())

      } else {
        self$appenders[sel][[1]]$show(n = n, threshold = threshold)
      }
    },


    format = function(
      ...,
      colors = TRUE
    ){

      header <- paste(
        paste0("<", class(self)[[1]], ">"),
        style_subtle(format(self$ancestry))
      )

      ind <- "  "

      appenders <- do.call(rbind, lapply(self$appenders, srs))

      if (length(appenders) > 0){
        appenders$name <- pad_right(appenders$name)

        appenders <- paste0(
          pad_right(appenders$name), ": ",
          pad_right(
            paste0(label_levels(appenders$threshold), style_subtle(paste0(" (", appenders$threshold, ")")))
          ),
          vapply(appenders$comment, ptrunc_col, character(1), width = 128)
        )
      } else {
        appenders <- style_subtle("none")
      }

      anc_appenders <- do.call(
        rbind,
        lapply(self$ancestral_appenders, function(.x){
          cbind(data.frame(logger = .x$logger$name, srs(.x)))
        }
      ))



      if (length(anc_appenders) > 0){
        anc_appenders$logger <- pad_right(anc_appenders$logger)
        anc_appenders$name   <- pad_right(anc_appenders$name)

        anc_appenders <- paste0(
          style_subtle(paste0(anc_appenders$logger, " -> ")),
          anc_appenders$name, ": ",
          pad_right(
            paste0(label_levels(anc_appenders$threshold), style_subtle(paste0(" (", anc_appenders$threshold, ")")))
          ),
          vapply(anc_appenders$comment, ptrunc_col, character(1), width = 128)
        )
      } else {
        anc_appenders <- NULL
      }




      obsums <- object_summaries(self)

      act <- obsums[obsums == "active binding"]
      act <- paste0(names(act), ": ", act)

      funs <- obsums[grep("function", obsums)]
      funs <- paste0(names(funs), ": ", funs)

      logger_pat <-
        paste0("(^", c(names(self$log_levels), "log"), ":)", collapse = "|")

      sel <- grepl(logger_pat, funs)
      methods <- funs[!sel]
      loggers <- c(
        paste0(names(self$log_levels), ": function (msg, ...) "),
        grep("^log:", funs, value = TRUE)
      )

      c(
        header,
        paste0(ind, "Fields / Active Bindings:"),
        paste0(ind, ind, "threshold: ",  fmt_threshold(self$threshold)),
        paste0(ind, ind, "log_levels: ", sls(self$log_levels)),
        paste0(ind, ind, "string_formatter: ", sls(self$string_formatter)),
        paste0(ind, ind, "user: ", self$user),
        paste0(ind, ind, "propagate:", self$propagate),
        paste0(ind, ind, "appenders:"),
        paste0(ind, ind, ind, appenders),
        paste0(ind, ind, ind, anc_appenders)[!is.null(anc_appenders)],
        paste0(ind, "Methods:"),
        paste0(ind, ind, "Loggers:"),
        paste0(ind, ind, ind, loggers),
        paste0(ind, ind, methods)
      )
    },


    # public fields -----------------------------------------------------------
    last_event = NULL

  ),




  # active bindings ---------------------------------------------------------
  active = list(
    name = function(value){
      if (missing(value)) return(private$.name)
      assert(is_scalar_character(value))
      private$.name <- value
    },


    propagate = function(value){
      if (missing(value)) return(private$.propagate)
      assert(is_scalar_bool(value))
      private$.propagate <- value
    },


    ancestry = function(){
      structure(
        c(self$name, private$.parent$ancestry),
        class = c("ancestry", "character")
      )
    },


    parent = function(value){
      if (missing(value)) return(private$.parent)
      assert(is.null(value) || inherits(value, "Logger"))
      private$.parent <- value
    },


    log_levels = function(value){
      if (missing(value)) return(private$.log_levels)

      assert(
        is.null(private$.log_levels),
        stop("'log_levels' cannot be modified once they have been initialized")
      )

      stopifnot(
        all_are_distinct(unname(value)),
        all_are_distinct(names(value)),
        !any(value == 0),
        is_integerish(value),
        identical(length(names(value)) , length(value))
      )

      value <- setNames(as.integer(value), names(value))
      class(value) <- c("log_levels", class(value))

      private$.log_levels <- value
    },


    threshold = function(value){
      if (missing(value))
        return(private$.threshold)

      if (is.na(value)){
        value <- NA_integer_

      } else if (is_scalar_character(value)) {
        assert_valid_log_levels(value)
        value <- unlabel_levels(value, log_levels = self$log_levels)
      }

      assert_valid_log_levels(value)
      private$.threshold <- as.integer(value)
    },


    ancestral_appenders = function(){
      if (self$propagate){
        c(
          private$.parent$appenders,
          private$.parent$ancestral_appenders
        )
      } else {
        NULL
      }
    },


    appenders = function(value){
      if (missing(value)) return(c(private$.appenders))

      if (is.null(value)){
        private$.appenders <- NULL
        return(invisible())
      }

      if (inherits(value, "Appender"))
        value <- list(value)

      assert(
        is.list(value) && all(vapply(value, inherits, TRUE, "Appender")),
        "'appenders' must either be a single Appender, a list thereof, or NULL for no appenders."
      )

      value <- lapply(value, function(app){
        res <- app$clone()
        res$logger <- self
        res
      })

      private$.appenders <- value
      invisible()
    },


    user = function(value){
      if (missing(value)) return(private$.user)
      assert(
        is_scalar_character(value),
        "'user' must be a scalar character"
      )
      private$.user <- value
    },


    string_formatter = function(value){
      if (missing(value)) return(private$.string_formatter)
      assert(is.function(value))
      private$.string_formatter <- value
    }
  ),


  # private -----------------------------------------------------------------
  private = list(
    .propagate = NULL,
    .filters = list(check_threshold),
    .name = NULL,
    .parrent = NULL,
    .appenders = NULL,
    .user = NA_character_,
    .log_levels = NULL,
    .threshold = 4L,
    .string_formatter = NULL,
    suspended_loggers = list()
  ),

  lock_objects = FALSE
)
