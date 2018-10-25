memlog <- R6::R6Class(
  "memlog",
  public = list(
    initialize = function(
      timer   = Sys.time,
      console_appender = appender_console,
      appenders = list(),
      user = whoami::email_address(whoami::fullname()),
      pid = Sys.getpid(),
      log_levels = c(
        "fatal" = 1,
        "error" = 2,
        "warn"  = 3,
        "info"  = 4,
        "debug" = 5,
        "trace" = 6
      )
    ){
      stopifnot(
        unique(unname(log_levels)) == unname(log_levels),
        unique(names(log_levels)) == names(log_levels),
        !any(log_levels == 0),
        is.function(console_appender)
      )


      private$pid   <- pid
      private$user  <- user
      private$timer <- timer
      private$log_levels <- log_levels
      private$console_appender <- console_appender
      private$appenders <- appenders
      private$data <- data.table::data.table(
        level = vector("integer", 100L),
        timestamp = timer(),
        user = NA_character_,
        pid = NA_integer_,
        caller = NA_character_,
        msg = NA_character_
      )

      make_logger <- function(
        level
      ){
        force(level)

        function(
          msg,
          ...
        ){
          msg <- sprintf(msg, ...)
          caller <- self$get_caller()
          force(caller)

          self$log(
            level = level,
            timestamp = private$timer(),
            user = private$user,
            pid = private$pid,
            caller = caller,
            msg = msg
          )
        }
      }

      for (i in seq_along(private$log_levels)){
        nm  <- names(private$log_levels)[[i]]
        lvl <- private$log_levels[[i]]

        if (nm %in% names(self)){
          stop(
            "The following names are not allowed for log levels: ",
            paste(sort(names(self)), collapse= ", ")
          )
        }

        self[[nm]] <- make_logger(lvl)
      }

    },

    showdt = function(threshold = 4L, n = 20) {
      if (is.character(threshold)) threshold <- private$log_levels[[threshold]]
      tail(private$data[private$data$level <= threshold & private$data$level > 0, ], n)
    },

    show = function(threshold = 4L, n = 20, appender = private$console_appender){
      l <- self$showdt(threshold, n)
      l$level_name <- names(private$log_levels)[match(l$level, private$log_levels)]
      appender(l)
    },

    log = function(
      level,
      timestamp,
      user,
      pid,
      caller,
      msg
    ){
      i <- private$used + 1L
      if (i > nrow(private$data))  private$allocate(100)
      if (is.null(caller))         caller <- self$get_caller()

      data.table::set(
        private$data,
        i,
        j = c("level", "timestamp", "user", "pid", "caller", "msg"),
        value = list(level, timestamp, user, pid, caller, msg)
      )

      if (level <= private$threshold){
        l <- private$data[i, ]
        l$level_name <- names(private$log_levels)[match(l$level, private$log_levels)]

        private$console_appender(l)
        for (appender in private$appenders) {
          appender(l)
        }

      }

      private$used <- i
      invisible(private$data[i, ])
    },

    get_caller = function(where = -2L){
      res <- tryCatch(
        deparse(sys.call(where)[[1]]),
        error = function(e) "NULL"
      )
      if (identical(res, "NULL"))
        "(shell)"
      else
        res
    }
  ),

  private = list(
    timer = NULL,
    used  = 0L,
    pid = Sys.getpid(),
    user = NA_character_,
    log_levels = NULL,
    threshold = 4L,
    appenders = NULL,
    allocate = function(n = 100L){
      end   <- nrow(private$data)
      alloc <- list(level = vector("integer", 100L))
      private$data <- data.table::rbindlist(list(private$data, alloc), fill = TRUE)
    },
    data = NULL
  ),
  lock_objects = FALSE
)


