#' Appenders
#'
#' Appenders are assigned to [Loggers] and manage the output of the [LogEvents]
#' to a destination, such as the console or a text file. An appender must have
#' a single [Layout] that tells it how to format the LogEvent. For details
#' please refer to the documentations of the specific Appenders.
#'
#' @section Creating a new Appender:
#'
#' General properties that are shared among all Appenders:
#'
#' \describe{
#'   \item{threshold}{`character` or `integer` scalar. The minimum log level
#'     that triggers this logger. See [log levels]}
#'   \item{layout}{A [Layout]. See examples.}
#'  }
#'
#' @name Appender
#' @aliases Appenders
#' @family Appenders
#' @include print.R
#' @include utils.R
#' @include utils-sfmisc.R
#' @include Filterable.R
NULL




# Appender ----------------------------------------------------------------

#' @export
Appender <- R6::R6Class(
  "Appender",
  inherit = Filterable,
  cloneable = FALSE,


  # +- public --------------------------------------------------------------
  public = list(
    initialize = function(
      layout = Layout$new(),
      threshold = NA_integer_
    ){
      self$layout    <- layout
      self$threshold <- threshold
    },

    append = function(event){
      private$.layout$format_event(event)
    }
  ),


  # +- active ---------------------------------------------------------------
  active = list(
    threshold = function(value){
      if (missing(value)) return(private$.threshold)
      assert_valid_threshold(value)

      if (is_scalar_character(value))
        value <- unlabel_levels(value)

      private$.threshold <- as.integer(value)
    },

    layout = function(value){
      if (missing(value)) return(private$.layout)
      assert(inherits(value, "Layout"))
      private$.layout <- value
    },

    logger = function(value){
      if (missing(value)) return(private$.logger)
      assert(inherits(value, "Logger"))
      private$.logger <- value
    },

    destination = function() NULL
  ),

  private = list(
    .filters = list(check_threshold),
    .threshold = NA,
    .layout = NULL,
    .logger = NULL
  )
)



# AppenderConsole ---------------------------------------------------------

#' AppenderConsole
#'
#' A simple Appender that outputs to the console. If you have the packages
#' **colt** and **crayon** installed log levels will be coloured by default.
#'
#' @inheritSection Appender Creating a new Appender
#'
#' @export
#' @seealso [LayoutFormat], [LayoutGlue]
#'
#' @examples
#' # create a new logger with propagate = FALSE to prevent routing to the root
#' # logger. Please look at the section "Logger Hirarchies" in the package
#' # vignette for more info.
#' logger  <- Logger$new("testlogger", propagate = FALSE)
#'
#' logger$add_appender(AppenderConsole$new())
#' logger$add_appender(AppenderConsole$new(
#'   layout = LayoutFormat$new("[%t] %c(): [%n] %m from user %u", colors = getOption("yog.colors"))))
#'
#' # Will output the message twice because we attached two console appenders
#' logger$warn("A test message")
#'
#' @family Appenders
#' @name AppenderConsole
NULL

#' @export
AppenderConsole <- R6::R6Class(
  "AppenderConsole",
  inherit = Appender,
  public = list(
    initialize = function(
      threshold = NA_integer_,
      layout = LayoutFormat$new(
        fmt = "%L [%t] %m",
        timestamp_fmt = "%H:%M:%OS3",
        colors = getOption("yog.colors")
      )
    ){
      self$threshold <- threshold
      self$layout <- layout
    },

    append = function(event){
      cat(private$.layout$format_event(event), sep = "\n")
      return(invisible())
    }
  ),

  active = list(
    destination = function() "console"
  )
)






# AppenderFile ------------------------------------------------------------

#' AppenderFile
#'
#' A simple Appender that outputs to a file in the file system.
#'
#' @inheritSection Appender Creating a new Appender
#'
#' @section Creating a new AppenderFile:
#'
#' \describe{
#'   \item{file}{`character` scalar. Path to the desired log file. If the file
#'     does not exist it will be created}
#'  }
#'
#' @inherit Appender
#'
#' @export
#' @seealso [LayoutFormat], [LayoutJson], [LayoutGlue]
#'
#' @examples
#' logger <- Logger$new()
#' default <- tempfile()
#' fancy <- tempfile()
#' json <- tempfile()
#'
#' logger$add_appender(AppenderFile$new(default), "default")
#' logger$add_appender(
#'   AppenderFile$new(fancy, layout = LayoutFormat$new("[%t] %c(): %L %m from user %u")), "fancy"
#' )
#' logger$add_appender(
#'   AppenderFile$new(json, layout = LayoutJson$new()), "json"
#' )
#'
#' logger$info("A test message")
#'
#' readLines(default)
#' readLines(fancy)
#' readLines(json)
#' @family Appenders
#' @name AppenderFile
NULL




#' @export
AppenderFile <- R6::R6Class(
  "AppenderFile",
  inherit = Appender,
  public = list(
    initialize = function(
      file,
      threshold = NA_integer_,
      layout = LayoutFormat$new()
    ){
      self$file <- file
      self$threshold <- threshold
      self$layout <- layout
    },

    append = function(event){
      cat(
        private$.layout$format_event(event),
        sep = "\n", file = private$.file, append = TRUE
      )
      return(invisible())
    }
  ),


  # +- active ---------------------------------------------------------------
  active = list(
    file = function(value){
      if (missing(value)) return(private$.file)
      private$.file <- value
    },

    destination = function() self$file
  ),

  private = list(
    .file = NULL
  )
)






# AppenderMemory ----------------------------------------------------------

#' @aliases yog_data
#' @family Appenders
#' @name AppenderMemoryDt
NULL

#' @export
AppenderMemoryDt <- R6::R6Class(
  "AppenderMemoryDt",
  inherit = Appender,
  public = list(
    initialize = function(
      threshold = NA_integer_,
      layout = LayoutFormat$new(
        fmt = "%L [%t] %m",
        timestamp_fmt = "%H:%M:%S",
        colors = getOption("yog.colors")
      ),
      prototype = data.table::data.table(
        .id  = NA_integer_,
        level = NA_integer_,
        timestamp = Sys.time(),
        caller = NA_character_,
        msg = NA_character_
      ),
      cache_size = 1e5
    ){
      assert(is_scalar_integerish(cache_size))
      assert(is.integer(prototype$.id))
      private$current_row <- 0L
      private$id <- 0L
      self$data <- prototype
      self$threshold <- threshold
      self$layout <- layout

      # initialize empty dt
      for (j in seq_along(private$.data)){
        data.table::set(private$.data, i = 1L, j = j, value = NA)
      }
      dd <- list(
        private$.data,
        list(.id = rep(private$.data[[1]], cache_size - 1L))
      )
      private$.data <- data.table::rbindlist(
        dd,
        fill = TRUE
      )
      data.table::setattr(
        private$.data,
        "class",
        c("yog_data", "data.table", "data.frame")
      )

      invisible(self)
    },


    append = function(
      event
    ){
      vals <- event[["values"]]
      lengths <- vapply(vals, length, integer(1), USE.NAMES = FALSE)
      lenmax  <- max(lengths)
      assert(all(lengths %in% c(1, lenmax)))

      if (lenmax > nrow(private$.data)){
        vals <- lapply(vals, trim_last_event, nrow(private$.data))
        # ensure .id would be the same as without cycling
        private[["id"]] <- private[["id"]] + lenmax - nrow(private$.data)
        lenmax <- nrow(private$.data)
      }

      i   <- seq_len(lenmax)
      ids <- i + private[["id"]]

      if (private[["current_row"]] + lenmax <= nrow(private$.data)){
        i   <- i + private[["current_row"]]
        private[["current_row"]] <- private[["current_row"]] + lenmax
      } else {
        # cycle cache
        private[["current_row"]] <- lenmax
      }

      data.table::set(
        private$.data,
        i,
        j = c(".id", names(vals)),
        value = c(list(ids), vals)
      )

      private[["id"]] <- private[["id"]] + lenmax
    },


    show = function(
      n = 20,
      threshold = NA_integer_
    ){
      if (is.na(threshold)) threshold <- Inf
      dd <- self$data
      dd <- tail(dd[dd$level <= threshold], n)
      dd <- as.environment(dd)
      assign("logger", self$logger, dd)
      cat(self$layout$format_event(dd), sep = "\n")
    }
  ),



  # +- active ---------------------------------------------------------------
  active = list(
    data = function(value){
      if (missing(value)){
        tmp <- private$.data[!is.na(private$.data$.id), ]
        return(tmp[order(tmp$.id), ])
      }
      assert(
        is.null(private$.data),
        stop("'data' cannot be modified once it has been initialized")
      )
      assert(data.table::is.data.table(value))
      private$.data <- value
    },

    destination = {
      function() "in memory data.table"
    }
  ),


  private = list(
    id = NULL,
    current_row = NULL,
    .data = NULL
  )
)




# AppenderMemoryBuffer --------------------------------------------------
AppenderMemoryBuffer <- R6::R6Class(
  "AppenderMemoryBuffer",
  inherit = Appender,
  public = list(
    initialize = function(
      threshold = NA_integer_,
      appenders = NULL,
      should_flush =
        function(x){if (any(get("level", envir = x) <= 200)) TRUE else FALSE},
      layout = LayoutFormat$new(
        fmt = "%L [%t] %m",
        timestamp_fmt = "%H:%M:%S",
        colors = getOption("yog.colors")
      ),
      cache_size = 1e3
    ){
      assert(is_scalar_integerish(cache_size))
      self$threshold <- threshold
      self$should_flush <- should_flush
      self$appenders <- appenders
      self$buffered_events <- list()  # no speed advantage in pre allocating lists in R!
      self$cache_size <- cache_size
      invisible(self)
    },


    append = function(
      x
    ){
      self$buffered_events[[length(self$buffered_events) + 1L]] <- x$clone()
      if (self$should_flush(x) || length(self$buffered_events) > self$cache_size)
        self$flush()
      invisible(NULL)
    },


    buffered_events = NULL,


    cache_size = NULL,


    flush = function(
      x = self$buffered_events
    ){
      for (event in self$buffered_events){
        for (app in self$appenders) {
          if (app$filter(event))  app$append(event)
        }
      }
      self$buffered_events <- list()
    },


    add_appender = function(
      appender,
      name = NULL
    ){
      assert(inherits(appender, "Appender"))
      appender <- appender$clone(deep = TRUE)
      appender$logger <- self$logger
      private$.appenders[length(private$.appenders) + 1L] <- list(appender)

      if (!is.null(name))
        names(private$.appenders)[length(private$.appenders)] <- name

      invisible(self)
    },


    remove_appender = function(
      pos
    ){
      assert(is_scalar(pos))

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
          "'pos' is not a name of any attached appender (",
          paste(names(private$.appenders), collapse = ", "),
          ")"
        )
      }

      try(private$.appenders[[pos]]$finalize(), silent = TRUE)
      private$.appenders[[pos]] <- NULL
      invisible(self)
    },


    finalize = function(){
      self$flush()
    },

    should_flush = NULL
  ),

  active = list(
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
        # logger gets assigned to sub-appenders inside the `logger` active binding
        res
      })

      private$.appenders <- value
      invisible()
    },

    logger = function(value){
      if (missing(value)) return(private$.logger)
      assert(inherits(value, "Logger"))
      private$.logger <- value
      for (app in self$appenders){
        app$logger <- value
      }
    }
  ),


  private = list(
    .appenders = list()
  )
)



# utils -------------------------------------------------------------------

trim_last_event <- function(x, max_len){
  if (length(x) == 1L)
    x
  else
    x[seq.int(length(x) - max_len + 1L, length(x))]
}



get_backup_index <- function(
  x
){
  vapply(
    strsplit(x, ".", fixed = TRUE),
    function(.x) {
      .r <- .x[[length(.x)]]
      if (identical(.r, "zip"))  .r <- .x[[length(.x) - 1L]]
      assert(!is.na(as.integer(.r)))
      .r
    },
    character(1)
  )
}


get_backup_timestamp <- function(
  x
){
  vapply(
    strsplit(x, ".", fixed = TRUE),
    function(.x) {
      .r <- .x[[length(.x)]]
      if (identical(.r, "zip"))  .r <- .x[[length(.x) - 1L]]
      .r
    },
    character(1)
  )
}



autopad_backup_index <- function(
  x
){
  assert(is.character(x) && length(x) > 0)
  bn  <- gsub("\\.\\d*(zip){0,1}", "", x)
  idx <- get_backup_index(x)
  int <- as.integer(idx)
  ext <- ifelse(tools::file_ext(x) == "zip", ".zip", "")
  new_idx <- pad_left(int, max(nchar(as.character(int))), "0")
  paste0(bn, ".", new_idx, ext)
}

