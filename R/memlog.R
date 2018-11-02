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
Memlog <- R6::R6Class(
  "Memlog",


  # public methods --------------------------------------------------------
  public = list(
    initialize = function(
      appenders = list(AppenderConsole$new()),
      user = guess_user(),
      log_levels = c(
        "fatal" = 1,
        "error" = 2,
        "warn"  = 3,
        "info"  = 4,
        "debug" = 5,
        "trace" = 6
      ),
      threshold = "info",
      string_formatter = sprintf,
      fmt = "%L [%t] %m",
      timestamp_fmt = "%H:%M:%S",
      colors = getOption("memlog.colors")
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

      for (app in private$.appenders)  app$append(self$last_value)
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


    format = function(
      ...,
      colors = TRUE
    ){
      header <- paste0("<", class(self)[[1]], ">")
      ind <- "  "

      appenders <- vapply(
        self$appenders,
        format,
        character(1),
        single_line_summary = TRUE,
        colors = colors
      )

      paste0(
        header, "\n",
        paste0(ind, "Log Levels:\n"),
        paste0(ind, ind, sls(self$log_levels), "\n"),
        paste0(ind, "Appenders:\n"),
        paste0(ind, ind, appenders, collapse= "\n")
      )
    },


  # public fields -----------------------------------------------------------
    last_value = environment()
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
      if (missing(value)) return(private$.threshold)
      if (is_scalar_character(value)){
        value <- unlabel_levels(value, log_levels = self$log_levels)
      }
      assert_valid_threshold(value, log_levels = self$log_levels)
      private$.threshold <- as.integer(value)
    },

    collector = function(value){
      if (missing(value)) {
        return(private$.collector)
      }

      assert(
        is.null(private$.collector),
        stop("The 'collector' cannot be modified after it has been initialized")
      )

      private$.collector <- value
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

  private = list(
    .collector = NULL,
    .appenders = NULL,
    .user = NA_character_,
    .log_levels = NULL,
    .threshold = 4L,
    .string_formatter = NULL,
    suspended_loggers = list()
  ),

  lock_objects = FALSE
)
