#' Title
#'
#' @param timer
#' @param appenders
#' @param formatter
#' @param user
#' @param pid
#' @param log_levels
#'
#' @return
#' @export
#'
#' @examples
memlog <- R6::R6Class(
  "memlog",
  public = list(
    initialize = function(
      timer   = Sys.time,
      appenders = list(appender_console),
      formatter = formatter_simple,
      user = whoami::email_address(whoami::fullname()),
      pid = Sys.getpid(),
      log_levels = c(
        "fatal" = 1,
        "error" = 2,
        "warn"  = 3,
        "info"  = 4,
        "debug" = 5,
        "trace" = 6
      ),
      cache_size = 3e5
    ){
      stopifnot(
        unique(unname(log_levels)) == unname(log_levels),
        unique(names(log_levels)) == names(log_levels),
        !any(log_levels == 0)
      )


      # fields ------------------------------------------------------------------
        private$id   <- 0L
        private$pid   <- pid
        private$user  <- user
        private$timer <- timer
        private$log_levels <- log_levels
        private$data <- data.table::data.table(
          id = rep(NA_real_, min(1000, cache_size)),
          level = 0,
          timestamp = timer(),
          user = NA_character_,
          pid = NA_integer_,
          caller = NA_character_,
          msg = NA_character_
        )
        private$cache_size = cache_size

      # methods
        private$appenders <- appenders
        private$formatter <- formatter

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

      showdt = function(threshold = NULL, n = NULL) {
        if (is.null(threshold)) threshold <- private$threshold
        if (is.character(threshold)) threshold <- private$log_levels[[threshold]]
        res <- private$data[private$data$level <= threshold & private$data$level > 0, ]
        res <- with(res, res[order(id), ])
        if (is.null(n)){
          res
        } else {
          tail(res, n)
        }
      },

      show = function(threshold = NULL, n = 20, formatter = private$formatter){
        if (is.null(threshold)) threshold <- private$threshold
        cat(formatter(tail(self$showdt(threshold), n), self))
      },

      log = function(
        level,
        timestamp,
        user,
        pid,
        caller,
        msg
      ){
        if (!identical(length(msg), 1L)) stop("'msg' must be a vector of length 1")

        private$current_row <- private$current_row + 1L
        private$id  <- private$id  + 1L

        data.table::set(
          private$data,
          private$current_row,
          j = c("id", "level", "timestamp", "user", "pid", "caller", "msg"),
          value = list(private$id, level, timestamp, user, pid, caller, msg)
        )

        if (private$current_row >= nrow(private$data))  private$allocate(1000L)
        for (appender in private$appenders) {
          appender(
            private$data[private$current_row, ],
            ml = self
          )
        }

        invisible(msg)
      },

      get_caller = function(where = -2L){
        res <- try(sys.call(where)[[1]])
        if (is.null(res) || inherits(res, "try-error"))
          "(shell)"
        else if (grepl("::", res, fixed = TRUE))
          deparse(res)
        else {
          res <- deparse(res)
          ns <- format(findFunction(res)[[1]])
          if (grepl("namespace", ns)){
            paste0(sub(".*namespace:([^>]+)>.*", "\\1", ns), "::", res)
          } else {
            paste0(sub(".*environment:([^>]+)>.*", "\\1", ns), "::", res)
          }

        }
      },

    label_levels = function(x){
      names(private$log_levels)[match(x, private$log_levels)]
    },

    get_threshoold = function(){
      private$threshold
    },

    format = function(x){
      private$formatter(x, self)
    }

  ),

  private = list(
    timer = NULL,
    current_row = 0L,
    id = 0L,
    pid = Sys.getpid(),
    user = NA_character_,
    log_levels = NULL,
    threshold = 4L,
    appenders = NULL,
    formatter = NULL,
    allocate = function(n = 1000L){
      end   <- nrow(private$data)
      if (end >= private$cache_size){
        private$current_row <- 0L
      } else {
        n <- min(n, private$cache_size)
        alloc <- list(level = vector("integer", n))
        private$data <- data.table::rbindlist(list(private$data, alloc), fill = TRUE)
      }
    },
    cache_size = NULL,
    data = NULL
  ),
  lock_objects = FALSE
)


