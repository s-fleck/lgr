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
  public = list(
    initialize = function(
      collector = CollectorDefault$new(
        level = NA_integer_,
        timestamp = Sys.time,
        msg = NA_character_,
        caller = get_caller
      ),
      appenders = list(AppenderConsoleMinimal$new()),
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
      colors = list(
        "fatal" = function(x) colt::clt_emph2(colt::clt_error(x)),
        "error" = colt::clt_error,
        "warn"  = colt::clt_warning,
        "info"  = colt::clt_info,
        "debug" = colt::clt_chr,
        "trace" = colt::clt_chr
      )
    ){



      # fields ------------------------------------------------------------
        # active
        self$collector  <- collector
        self$log_levels <- log_levels
        self$threshold <- threshold
        self$appenders <- appenders

        self$user  <- user
        self$string_formatter <- string_formatter
        self$fmt <- fmt
        self$timestamp_fmt <- timestamp_fmt
        self$colors <- colors


      # init log functions ----------------------------------------------
        make_logger <- function(
          level,
          ...
        ){
          force(level)
          function(msg, ...){
            private$.collector$log(
              msg = private$.string_formatter(msg, ...),
              level = level
            )
            for (app in private$.appenders){
              app$append()
            }
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

      showdt = function(n = NULL, threshold = Inf) {
        if (is.null(threshold)) threshold <- private$.threshold
        if (is.character(threshold)) threshold <- private$.log_levels[[threshold]]
        dd <- private$.collector$data
        dd <- dd[dd$level <= threshold & dd$level > 0, ]
        dd <- dd[order(dd$id), ]

        if (is.null(n)){
          dd
        } else {
          tail(dd, n)
        }
      },

      show = function(
        n = 20,
        threshold = Inf,
        ...
      ){
        if (is.null(threshold)) {
          threshold <- private$.threshold
        }
        dd <- tail(self$showdt(threshold), n)

        cat(
          format(
            dd,
            fmt = self$fmt,
            timestamp_fmt = self$timestamp_fmt,
            colors = self$colors,
            ml = self
          ),
          sep = "\n"
        )
      },

      log = function(
        level,
        timestamp,
        caller,
        msg
      ){
        if (!identical(length(msg), 1L)) stop("'msg' must be a vector of length 1")

        private$current_row <- private$current_row + 1L
        private$id <- private$id  + 1L

        val <- list(id = private$id, level = level, timestamp = timestamp, caller = caller, msg = msg)

        data.table::set(
          private$data,
          private$current_row,
          j = c("id", "level", "timestamp",  "caller", "msg"),
          value = val
        )

        # caching the last row for easy access brings significant speed
        # improvement for the appender
        private$last_row <- val

        for (appender in private$.appenders) {
          appender$append()
        }

        if (private$current_row >= nrow(private$data))
          private$allocate(1000L)

        invisible(msg)
      },

    label_levels = function(x){
      if (!is.numeric(x)) stop("Expected numeric 'x'")
      names(private$.log_levels)[match(x, private$.log_levels)]
    },

    unlabel_levels = function(x){
      if (!is.character(x)) stop("Expected character 'x'")
      private$.log_levels[match(x, names(private$.log_levels))]
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
    }
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

      private$.log_levels <- value
    },

    threshold = function(value){
      if (missing(value)) return(private$.threshold)
      if (is_scalar_character(value)){
        value <- self$unlabel_levels(value)
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

      value <- lapply(value, function(.x){
        res <- .x$clone()
        res$parent_memlog <- self
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
