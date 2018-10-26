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
      appenders = list(appender_console_minimal$new()),
      formatter = format,
      user = whoami::email_address(whoami::fullname()),
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
        all_are_distinct(unname(log_levels)),
        all_are_distinct(names(log_levels)),
        !any(log_levels == 0)
      )

      # fields ------------------------------------------------------------------
        private$id   <- 0L
        private$user  <- user
        private$timer <- timer
        private$log_levels <- log_levels
        private$data <- structure(
          data.table::data.table(
            id = rep(NA_real_, min(1000, cache_size)),
            level = 0,
            timestamp = timer(),
            caller = NA_character_,
            msg = NA_character_
          ),
          class = c("memlog_data", "data.table", "data.frame")
        )
        private$cache_size = cache_size

      # methods
        if (inherits(appenders, "appender")){
          appenders <- list(appenders)
        }

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
            caller <- private$get_caller()
            force(caller)

            self$log(
              level = level,
              timestamp = private$timer(),
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

      show = function(threshold = NULL, n = 20, ...){
        if (is.null(threshold)) threshold <- private$threshold
        dd <- tail(self$showdt(threshold), n)
        self$print_data(dd, ..., ml = self)
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

        for (appender in private$appenders) {
          appender$append(self)
        }

        if (private$current_row >= nrow(private$data))
          private$allocate(1000L)

        invisible(msg)
      },

    label_levels = function(x){
      if (!is.numeric(x)) stop("Expected numeric 'x'")
      names(private$log_levels)[match(x, private$log_levels)]
    },

    unlabel_levels = function(x){
      if (!is.character(x)) stop("Expected character 'x'")
      private$log_levels[match(x, names(private$log_levels))]
    },

    get_threshold = function(){
      private$threshold
    },

    set_threshold = function(x){
      if (is_scalar_character(x)){
        x <- self$unlabel_levels(x)
      }
      assert(
        !is.na(x) && is_scalar_integerish(x),
        "'x' must either the numeric or character representation of one of the following log levels: ",
        paste(sprintf("%s (%s)", names(private$log_levels), private$log_levels), collapse = ", ")
      )

      private$threshold <- as.integer(x)
    },

    format_data = function(x, ..., ml = self){
      private$formatter(x, ..., ml = ml)
    },

    print_data = function(x, ..., ml = self){
      cat(self$format_data(x, ...,ml = ml), sep = "\n")
    },

    get_log_levels = function(){
      private$log_levels
    },

    get_row = function(i = private$current_row){
      private$data[i, ]
    },

    get_last_row = function(){
      private$last_row
    }

  ),

  private = list(
    timer = NULL,
    current_row = 0L,
    id = 0L,
    user = NA_character_,
    log_levels = NULL,
    threshold = 4L,
    appenders = NULL,
    formatter = NULL,
    cache_size = NULL,
    data = NULL,
    last_row = NULL,

    allocate = function(n = 1000L){
      end   <- nrow(private$data)
      if (end >= private$cache_size){
        private$current_row <- 0L
      } else {
        n <- min(n, private$cache_size)
        alloc <- list(level = vector("integer", n))
        private$data <- data.table::rbindlist(list(private$data, alloc), fill = TRUE)
        data.table::setattr(private$data, "class", c("memlog_data", "data.table", "data.frame"))
      }
    },

    get_caller = function(
      where = -2L
    ){
      res <- try(sys.call(where)[[1]], silent = TRUE)

      if (is.null(res) || inherits(res, "try-error")){
        "(shell)"

      } else if (grepl("::", res, fixed = TRUE)){
        deparse(res)

      } else {
        res <- deparse(res)
        ns <- try(format(findFunction(res)[[1]]), silent = TRUE)
        if (inherits(ns, "try-error")){
          res
        } else if (grepl("namespace", ns)){
          paste0(sub(".*namespace:([^>]+)>.*", "\\1", ns), "::", res)
        } else {
          paste0(sub(".*environment:([^>]+)>.*", "\\1", ns), "::", res)
        }
      }
    }
  ),
  lock_objects = FALSE
)


