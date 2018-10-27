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
memlog <- R6::R6Class(
  "memlog",
  public = list(
    initialize = function(
      collector = collector_dt$new(
        level = NA_integer_,
        timestamp = Sys.time,
        msg = NA_character_,
        caller = get_caller,
        .cache_size = 1e5
      ),
      appenders = list(appender_console_minimal$new()),
      user = whoami::email_address(whoami::fullname()),
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
        all_are_distinct(unname(log_levels)),
        all_are_distinct(names(log_levels)),
        !any(log_levels == 0)
      )


      # init --------------------------------------------------------------
      if (inherits(appenders, "appender")) appenders <- list(appenders)


      # fields ------------------------------------------------------------
        private$user  <- user
        private$log_levels <- log_levels
        private$collector <- collector
        private$appenders <- appenders


        # init log functions ----------------------------------------------

        make_logger <- function(
          level,
          ...
        ){
          force(level)
          function(msg, ...){
            private$collector$log(msg = sprintf(msg, ...), level = level)
            for (app in private$appenders){
              app$append(self)
            }
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
        dd <- private$collector$get_data()
        dd <- dd[dd$level <= threshold & dd$level > 0, ]
        dd <- dd[order(dd$id), ]

        if (is.null(n)){
          dd
        } else {
          tail(dd, n)
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
    },

    get_appenders = function(){
      private$appenders
    },

    get_collector = function(){
      private$collector
    }
  ),

  private = list(
    collector = NULL,
    appenders = NULL,
    user = NA_character_,
    log_levels = NULL,
    threshold = 4L
  ),

  lock_objects = FALSE
)


