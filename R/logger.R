#' Title
#'
#' @param appenders
#' @param user
#' @param pid
#' @param log_levels
#'
#' @return
#' @export
#'
#' @examples
Logger <- R6::R6Class(
  "Logger",

  # public methods --------------------------------------------------------
  public = list(
    initialize = function(
      appenders = list(
        AppenderConsole$new(layout = LayoutFormat$new(colors = getOption("yog.colors")))
      ),
      user = get_user(),
      log_levels = getOption("yog.log_levels"),
      threshold = NA,
      string_formatter = sprintf
    ){
      # fields ------------------------------------------------------------
        # active
        self$log_levels <- log_levels
        self$threshold <- threshold
        self$appenders <- appenders
        self$user  <- user
        self$string_formatter <- string_formatter

      # init log functions ----------------------------------------------
        make_logger <- function(
          level,
          ...
        ){
          force(level)
          function(msg, ...){
            self$log(
              msg = private$.string_formatter(msg, ...),
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


    log = function(level, timestamp = Sys.time(), caller = get_caller(), msg){

      assign("level", level, envir = self$last_value)
      assign("timestamp", timestamp,  envir = self$last_value)
      assign("caller", caller, envir = self$last_value)
      assign("msg", msg, envir = self$last_value)

      for (app in private$.appenders) {
        if (is.na(app$threshold) || level <= app$threshold){
          app$append(self$last_value)
        }
      }
    },


    add_appender = function(appender, name = NULL){
      assert(inherits(appender, "Appender"))
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
        message("This logger has no memory appender (see AppenderMempryDt)")
        return(invisible)

      } else {
        self$appenders[sel][[1]]$show(n = n, threshold = threshold)
      }
    },


    format = function(
      ...,
      colors = TRUE
    ){
      header <- paste0("<", class(self)[[1]], ">")
      ind <- "  "

      appenders <- do.call(rbind, lapply(self$appenders, srs))
      appenders$name <- pad_right(appenders$name)

      appenders <- paste0(
        pad_right(appenders$name), ": ",
        pad_right(
          paste0(label_levels(appenders$threshold), style_subtle(paste0(" (", appenders$threshold, ")")))
        ),
        vapply(appenders$comment, ptrunc, character(1), width = 128)
      )

      obsums <- object_summaries(self)

      act <- obsums[obsums == "active binding"]
      act <- paste0(names(act), ": ", act)

      funs <- obsums[grep("function", obsums)]
      funs <- paste0(names(funs), ": ", funs)


      paste0(
        header, "\n",
        paste0(ind, "threshold:\n"),
        paste0(ind, ind, fmt_threshold(self$threshold)), "\n",
        paste0(ind, "appenders:\n"),
        paste0(ind, ind, appenders, collapse= "\n")
        # paste0(ind, "Log Levels:\n"),
        # paste0(ind, ind, sls(self$log_levels))
        # paste0(ind, "Methods:\n"),
        # paste0(ind, ind, funs, collapse= "\n"), "\n",
        # paste0(ind, "Active Bindings:\n"),
        # paste0(ind, ind, act, collapse= "\n")
      )
    },



  # public fields -----------------------------------------------------------
    last_value = new.env(parent = emptyenv())
  ),


  active = list(
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

    appenders = function(value){
      if (missing(value)) return(private$.appenders)

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
    .appenders = NULL,
    .user = NA_character_,
    .log_levels = NULL,
    .threshold = 4L,
    .string_formatter = NULL,
    suspended_loggers = list()
  ),

  lock_objects = FALSE
)
