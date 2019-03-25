#' Appenders
#'
#' @description
#' Appenders are assigned to [Loggers] and
#' manage the output of the [LogEvents] to a destination, such as the console or
#' a text file. An Appender must have a single [Layout] that tells it how to
#' format the LogEvent. For details please refer to the documentations of the
#' specific Appenders.
#'
#' **Appender is not designed for direct usage**, but it is the basis on which all
#' other Appenders are built. Please see the **see also** section towards the
#' end of this document a list of available Appenders.
#'
#' @eval r6_usage(Appender)
#'
#' @section Creating a New Appender:
#'
#' New Appenders are instantiated with `<AppenderSubclass>$new()`. For the
#' arguments to `new()` please refer to the section *Fields*. You can also
#' modify those fields after the Appender has been created with setters in the
#' form of `appender$set_<fieldname>(value)`
#'
#' @inheritSection Filterable Fields
#' @inheritSection Filterable Methods
#'
#' @section Fields:
#'
#' \describe{
#'   \item{`threshold`, `set_threshold(level)`}{`character` or `integer` scalar.
#'     The minimum log level that triggers this logger. See [log_levels]}
#'   \item{`layout`, `set_layout(layout)`}{a `Layout` that will be used for
#'     formatting the `LogEvents` passed to this Appender}
#'   \item{`destination`}{The output destination of the `Appender` in
#'     human-readable form (mainly for print output)}
#'  }
#'
#' @section Methods:
#'  \describe{
#'     \item{`append(event)`}{Tell the Appender to process a [LogEvent] `event`.
#'       This method is usually not called by the user, but invoked by a
#'       [Logger]
#'     }
#'  }
#'
#' @name Appender
#' @aliases Appenders
#' @family Appenders
#' @include utils.R
#' @include utils-sfmisc.R
#' @include Filterable.R
#' @keywords internal
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
      self$set_layout(layout)
      self$set_threshold(threshold)
    },

    append = function(event){
      private$.layout$format_event(event)
    },

    set_threshold = function(level){
      level <- standardize_threshold(level)
      private$.threshold <- as.integer(level)
      invisible(self)
    },

    set_layout = function(layout){
      assert(inherits(layout, "Layout"))
      private$.layout <- layout
      invisible(self)
    }
  ),


  # +- active ---------------------------------------------------------------
  active = list(
    threshold = function() private$.threshold,

    layout = function() private$.layout,

    destination = function() ""
  ),

  private = list(
    .threshold = NA,
    .layout = NULL
  )
)




# AppenderConsole ---------------------------------------------------------

#' Log to the Console
#'
#' A simple Appender that outputs to the console. If you have the package
#' **crayon** installed log levels will be coloured by default
#' (but you can modify this behaviour by passing a custom [Layout]).
#'
#' @eval r6_usage(AppenderConsole)
#'
#' @inheritSection Appender Creating a New Appender
#' @inheritSection Appender Fields
#' @inheritSection Appender Methods
#'
#' @family Appenders
#' @name AppenderConsole
#' @export
#' @seealso [LayoutFormat]
#'
#' @examples
#' # create a new logger with propagate = FALSE to prevent routing to the root
#' # logger. Please look at the section "Logger Hirarchies" in the package
#' # vignette for more info.
#' logger  <- Logger$new("testlogger", propagate = FALSE)
#'
#' logger$add_appender(AppenderConsole$new())
#' logger$add_appender(AppenderConsole$new(
#'   layout = LayoutFormat$new("[%t] %c(): [%n] %m", colors = getOption("lgr.colors"))))
#'
#' # Will output the message twice because we attached two console appenders
#' logger$warn("A test message")
#'
NULL




#' @export
AppenderConsole <- R6::R6Class(
  "AppenderConsole",
  inherit = Appender,
  cloneable = FALSE,
  public = list(
    initialize = function(
      threshold = NA_integer_,
      layout = LayoutFormat$new(
        fmt = "%L [%t] %m %f",
        timestamp_fmt = "%H:%M:%OS3",
        colors = getOption("lgr.colors", list())
      ),
      filters = NULL
    ){
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)
    },

    append = function(event){
      cat(private$.layout$format_event(event), sep = "\n")
    }
  ),

  active = list(
    destination = function() "console"
  )
)




# AppenderFile ------------------------------------------------------------

#' Log to a File
#'
#' A simple Appender that outputs to a file in the file system. If you plan
#' to log to text files, consider logging to JSON files and take a look at
#' [AppenderJson], which is more or less a shortcut for `AppenderFile` with
#' [`LayoutJson`] and a few extra methods for convenience.
#'
#' @eval r6_usage(AppenderFile)
#'
#' @inheritSection Appender Creating a New Appender
#' @inheritSection Appender Fields
#' @inheritSection Appender Methods
#'
#' @section Fields:
#'
#' \describe{
#'   \item{`file`, `set_file(file)`}{`character` scalar. Path to the desired log
#'   file. If the file does not exist it will be created}
#'  }
#'
#'
#' @export
#' @seealso [LayoutFormat], [LayoutJson]
#' @family Appenders
#' @name AppenderFile
#'
#' @examples
#' logger <- Logger$new("loggername")
#' default <- tempfile()
#' fancy <- tempfile()
#' json <- tempfile()
#'
#' logger$add_appender(AppenderFile$new(default), "default")
#' logger$add_appender(
#'   AppenderFile$new(fancy, layout = LayoutFormat$new("[%t] %c(): %L %m")), "fancy"
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
#'
#' # cleanup
#' unlink(default)
#' unlink(fancy)
#' unlink(json)
NULL




#' @export
AppenderFile <- R6::R6Class(
  "AppenderFile",
  inherit = Appender,
  cloneable = FALSE,
  public = list(
    initialize = function(
      file,
      threshold = NA_integer_,
      layout = LayoutFormat$new(),
      filters = NULL
    ){
      self$set_file(file)
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)
    },

    append = function(event){
      cat(
        private$.layout$format_event(event),
        sep = "\n",
        file = get(".file", envir = private),
        append = TRUE
      )
    },

    set_file = function(file){
      assert(is_scalar_character(file))
      private$.file <- file
      invisible(self)
    }
  ),


  # +- active ---------------------------------------------------------------
  active = list(
    file = function() private$.file,
    destination = function() self$file
  ),

  private = list(
    .file = NULL
  )
)




# AppenderJson ------------------------------------------------------------

#' Log to a JSON File
#'
#' `AppenderJson` is a shortcut for `AppenderFile` with [`LayoutJson`], but
#' comes with an extra method `show()` and an extra active field `data` to
#' comfortably access the underlying file.
#'
#' @eval r6_usage(AppenderFile)
#'
#' @inheritSection AppenderFile Creating a New Appender
#' @section Creating a New Appender:
#'
#' @inheritSection AppenderFile Fields
#' @inheritSection AppenderTable Fields
#' @inheritSection AppenderFile Methods
#'
#' @section Fields:
#'
#' @section Methods:
#'
#' \describe{
#'   \item{`show(n, threshold)`}{Show the last `n` log entries with a log level
#'   bellow `threshold`. The log entries will be formatted as in the source
#'   JSON file}
#' }
#'
#' @family Appenders
#' @name AppenderJson
#' @export
#' @seealso [LayoutFormat], [LayoutJson]
#' @examples
#' tf <- tempfile()
#' l <- Logger$new("testlogger", appenders = AppenderJson$new(tf), propagate = FALSE)
#'
#' l$info("A test message")
#' l$info("A test message %s strings", "with format strings", and = "custom_fields")
#'
#' l$appenders[[1]]$show()
#' l$appenders[[1]]$data
#'
#' # cleanup
#' unlink(tf)
NULL




#' @export
AppenderJson <- R6::R6Class(
  "AppenderJson",
  inherit = AppenderFile,
  cloneable = FALSE,
  public = list(
    initialize = function(
      file,
      threshold = NA_integer_,
      layout = LayoutJson$new(),
      filters = NULL
    ){
      self$set_file(file)
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)
    },

    show = function(
      threshold = NA_integer_,
      n = 20L
    ){
      assert(is_scalar_integerish(n))
      threshold <- standardize_threshold(threshold)

      if (!is.na(threshold)){
        sel <- self$data$level <= threshold
      } else {
        sel <- TRUE
      }
      dd <- tail(readLines(self$file)[sel], n)
      cat(dd, sep = "\n")
      invisible(dd)
    }
  ),
  active = list(
    data = function(){
      read_json_lines(self$file)
    },

    dt = function(){
      data.table::as.data.table(self$data)
    }
  )
)




# AppenderTable -----------------------------------------------------------

#' Abstract Class for Logging to Tabular Structures
#'
#' @template abstract_class
#'
#' @description
#' AppenderTable is extended by Appenders that write to a data source that
#' can be interpreted as tables, (usually a `data.frame`). Examples are
#' [AppenderDbi], [AppenderRjdbc] and [AppenderDt].
#'
#' @inheritSection Appender Fields
#' @inheritSection Appender Methods
#'
#' @section Fields:
#'
#' \describe{
#'   \item{`data`}{Get the log recorded by this `Appender` as a `data.frame`}
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{`show(n, threshold)`}{Show the last `n` log entries with a
#'   log level bellow `threshold`.}
#' }
#'
#' @family Appenders
#' @name AppenderTable
NULL




# exclude from coverage because AppenderTable is just a metaclass
# nocov start
#' @export
AppenderTable <- R6::R6Class(
  "AppenderTable",
  inherit = Appender,
  cloneable = FALSE,

  public = list(
    show = function(threshold = NA_integer_, n = 20L) NULL
  ),

  active = list(
    data = function() as.data.frame(self$dt),
    dt   = function() NULL
  )
)




# nocov end



# AppenderDt ----------------------------------------------------------

#' Log to an In-Memory Data.Table
#'
#' An Appender that outputs to an in-memory `data.table`. This kind of
#' Appender is useful for interactive use, and has very little overhead.
#'
#' @section Custom Fields:
#'
#' `AppenderDt` supports [custom fields][LogEvent], but they have to be
#' pre-allocated in the `prototype` argument. Custom fields that are not
#' part of the prototype are discarded. If you want an Appender that retains
#' all custom fields (at the cost of slightly less performance), take a look at
#' [AppenderBuffer].
#'
#' With the default settings, the custom field `value` is included in the
#' `data.table` as a list column to store arbitrary \R objects (see example).
#' It is recommended to use this feature only `TRACE` level.
#'
#' @eval r6_usage(AppenderDt)
#'
#' @inheritSection AppenderTable Fields
#' @inheritSection Appender Methods
#'
#'
#' @section Creating a Data Table Appender:
#'
#' In addition to the usual fields, `AppenderDt$new()` requires that you supply
#' a `buffer_size` and a `prototype`. These determine the structure of the
#' `data.table` used to store the log this appender creates and cannot be
#' modified anymore after the instantiation of the appender.
#'
#' The [Layout] for this Appender is used only to format console output of
#' its `$show()` method.
#'
#' \describe{
#'   \item{buffer_size}{`integer` scalar. Number of rows of the in-memory
#'   `data.table`}
#'   \item{prototype}{A prototype `data.table`. The prototype must be a
#'     `data.table` with the same columns and column types as the data
#'     you want to log. The actual content of the columns is irrelevant.
#'     There are a few columns that have special meaning, based on their name:
#'     \itemize{
#'       \item{`.id`: `integer` (mandatory). Must always be the first column
#'         and is used internally by the Appender}
#'       \item{`.custom`: `list` (optional). If present all custom values of the
#'         event (that are not already part of the prototype) are stored in
#'         this list column.}
#'     }
#'   }
#' }
#'
#' @section Fields:
#'
#' \describe{
#'   \item{`dt`}{Get the log recorded by this `Appender` as a `data.table`
#'     with a maximum of `buffer_size` rows}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{`show(n, threshold)`}{Show the last `n` log entries with a log level
#'   bellow `threshold`. The log entries will be formatted for console output
#'   via this Appenders [Layout]}
#'  }
#'
#' @section Comparison AppenderBuffer and AppenderDt:
#'
#' Both [AppenderBuffer] and [AppenderDt] do in memory buffering of events.
#' AppenderBuffer retains a copies of the events it processes and has the
#' ability to pass the buffered events on to other Appenders. AppenderDt
#' converts the events to rows in a `data.table` and is a bit harder to
#' configure. Used inside loops (several hundred iterations),
#' AppenderDt has much less overhead than AppenderBuffer. For single logging
#' calls and small loops, AppenderBuffer is more performant. This is related to
#' how memory pre-allocation is handled by the appenders.
#'
#' In short: Use AppenderDt if you want an in-memory log for interactive use,
#' and AppenderBuffer if you actually want to buffer events
#'
#' @export
#' @seealso [LayoutFormat], [simple_logging], [data.table::data.table]
#' @aliases lgr_data
#' @name AppenderDt
#'
#' @examples
#' lg <- Logger$new(
#'   "test",
#'   appenders = list(memory = AppenderDt$new()),
#'   threshold = NA,
#'   propagate = FALSE  # to prevent routing to root logger for this example
#' )
#' lg$debug("test")
#' lg$error("test")
#'
#' # Displaying the log
#' lg$appenders$memory$data
#' lg$appenders$memory$show()
#' show_log(target = lg$appenders$memory)
#'
#' # If you pass a Logger to show_log(), it looks for the first AppenderDt
#' # that it can find.
#' show_log(target = lg)
#'
#' # Custom fields are stored in the list column .custom by default
#' lg$info("the iris data frame", caps = LETTERS[1:5])
#' lg$appenders$memory$data
#' lg$appenders$memory$data$.custom[[3]]$caps
#'
NULL




#' @export
AppenderDt <- R6::R6Class(
  "AppenderDt",
  inherit = Appender,
  cloneable = FALSE,
  public = list(
    initialize = function(
      threshold = NA_integer_,
      layout = LayoutFormat$new(
        fmt = "%L [%t] %m %f",
        timestamp_fmt = "%H:%M:%OS3",
        colors = getOption("lgr.colors", list())
      ),
      prototype = data.table::data.table(
        .id  = NA_integer_,
        level = NA_integer_,
        timestamp = Sys.time(),
        logger = NA_character_,
        caller = NA_character_,
        msg = NA_character_,
        .custom = list(list())
      ),
      buffer_size = 1e5,
      filters = NULL
    ){
      assert(is_scalar_integerish(buffer_size))
      assert(
        data.table::is.data.table(prototype) && is.integer(prototype$.id),
        "'prototype' must be a data.table with an integer column '.id'"
      )

      if (".custom" %in% names(prototype) && !is.list(prototype$.custom)){
        warning(
          "`prototype` has the special column `.custom` but it is ",
          class_fmt(prototype$.custom), " instead of a list-column. ",
          "Coercing to list-column."
        )
      }

      private$current_row <- 0L
      private$id <- 0L
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)

      # initialize empty dt
      prototype <- data.table::copy(prototype)
      for (j in seq_along(prototype)){
        if (is.list(prototype[[j]])){
          data.table::set(prototype, i = 1L, j = j, value = list(list(NULL)))
        } else {
          data.table::set(prototype, i = 1L, j = j, value = NA)
        }
      }
      dd <- list(
        prototype,
        list(.id = rep(prototype[[1]], buffer_size - 1L))
      )
      private$.data <- data.table::rbindlist(
        dd,
        fill = TRUE
      )

      # store names list columsn for use in append()
      list_cols <- vapply(private$.data, is.list, logical(1))
      private$list_cols <- names(list_cols[list_cols])

      invisible(self)
    },


    append = function(
      event
    ){
      # AppenderDt is designed for minimum overhead, so it does not use a
      # Layout for transforming the log event into a tabular structure but
      # rather the process is hardcoded
        dt <- get(".data", private)
        datanames <- names(dt)
        valnames  <- setdiff(datanames, ".id")

      # Select and prepare event values to be inserted into data
        vals <- event[["values"]]

        # handle .custom
        if (".custom" %in% datanames){
          vals[[".custom"]] <- vals[!names(vals) %in% valnames]
        }

        vals <- vals[valnames]
        names(vals) <- valnames


        # handle list-columns
        vals[vapply(vals, is.null, FALSE)] <- list(NULL)
        list_cols <- get("list_cols", private)
        vals[list_cols] <- lapply(vals[list_cols], list)

      # Prepare values for vectorized insert (if necessary)
        lengths <- vapply(vals, length, 1L, USE.NAMES = FALSE)
        lenmax  <- max(lengths)
        assert(all(lengths %in% c(1, lenmax)))

        # take special care if vectorized insert is bigger than buffer size
        if (lenmax > nrow(dt)){
          vals <- lapply(vals, trim_to_buffer_size, nrow(dt))
          private[["id"]] <- get("id", envir = private) + lenmax - nrow(private$.data)
          lenmax <- nrow(dt)
        }
        i   <- seq_len(lenmax)

      # generate new ids
        ids <- i + get("id", private)

      # check if rotation is necessary
        if (get("current_row", private) + lenmax <= nrow(dt)){
          i   <- i + get("current_row", envir = private)
          private[["current_row"]] <- get("current_row", envir = private) + lenmax
        } else {
          # rotate buffer
          assign("current_row", lenmax, envir = private)
        }

      # Perform the insert
        data.table::set(
          dt,
          i,
          j = c(".id", names(vals)),
          value = c(list(ids), vals)
        )

      private[["id"]] <- get("id", envir = private) + lenmax
    },


    show = function(
      threshold = NA_integer_,
      n = 20L
    ){
      assert(is_scalar_integerish(n))
      threshold <- standardize_threshold(threshold)

      if (is.na(threshold)) threshold <- Inf
      dd <- self$dt

      if (identical(nrow(dd),  0L)){
        cat("[empty log]")
        return(invisible(NULL))
      }

      res <- tail(dd[dd$level <= threshold, ], n)

      # construct a hackish pseudo-log event out of the data.table. This is
      # guranteed to work with LayoutFormat, other layouts might run into
      # issues
      walk(
        as_LogEvent_list.data.frame(res),
        function(.x){
          cat(self$layout$format_event(.x), "\n", sep = "")
        }
      )

      invisible(res)
    },

    set_layout = function(layout){
      assert(inherits(layout, "Layout"))
      if (!inherits(layout, "LayoutFormat")){
        warning(
          "AppenderDt currently only fully supports LayoutFormat. Accessing",
          "event$values or event$.logger from other Layouts is not possible.",
          "if you run into issues, don't hesitate to file a bug report",
          "or feature request on github."
        )
      }
      private$.layout <- layout
      invisible(self)
    }
  ),



  # +- active ---------------------------------------------------------------
  active = list(
    dt = function(){
      tmp <- private$.data[!is.na(private$.data$.id), ]
      tmp[order(tmp$.id), ]
    },

    data = function(){
      as.data.frame(self$dt)
    },

    destination = {
      function() "in memory data.table"
    }
  ),


  private = list(
    id = NULL,
    current_row = NULL,
    .data = NULL,
    list_cols = NULL
  )
)




# AppenderMemory  --------------------------------------------------

#' Abstract Class for Logging to Memory Buffers
#'
#' @template abstract_class
#'
#' @description
#' AppenderMemory is extended by Appenders that retain an in-memory event
#' buffer, such as [AppenderBuffer] and [AppenderPushbullet].
#'
#' @eval r6_usage(AppenderMemory)
#'
#' @inheritSection AppenderDt Methods
#' @inheritSection AppenderDt Fields
#'
#' @section Fields:
#'
#' \describe{
#'   \item{`buffer_size, set_buffer_size(x)`}{`integer` scalar `>= 0` Number of
#'     [LogEvents] to buffer.
#'   }
#'
#'   \item{`buffer_events`, `buffer_df`, `buffer_dt`}{
#'     The contents of the buffer as a `list` of [LogEvents][LogEvent], a
#'     `data.frame` or a `data.table`.
#'   }
#'
#'  \item{`flush_threshold`, `set_flush_threshold()`}{`integer` or `character`
#'     [log level][log_level]. Minimum event level that will trigger flushing of
#'     the buffer. This behaviour is implemented through `should_flush()`,
#'     and you can modify that function for different behaviour.
#'   }
#'   \item{`should_flush(event)`, `set_should_flush(x)`}{
#'     A function with exactly one arguments: `event`.
#'     If the function returns `TRUE`, flushing of the buffer
#'     is triggered. Defaults to flushing if an event of level `error`
#'     or higher is registered.}
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{`flush()`}{Manually trigger flushing of the buffer}
#' }
#'
#' @export
#' @seealso [LayoutFormat]
#' @name AppenderMemory
NULL




#' @export
AppenderMemory <- R6::R6Class(
  "AppenderMemory",
  inherit = Appender,
  cloneable = FALSE,
  public = list(
    append = function(event){
      i <- get("insert_pos", envir = private) + 1L
      assign("insert_pos", i, envir = private, inherits = TRUE)
      private[["last_event"]] <- private[["last_event"]] + 1L
      private[["event_order"]][[i]] <- private[["last_event"]]
      private[[".buffer_events"]][[i]] <- event

      bs <- get(".buffer_size", envir = private)

      sf <- get(".should_flush", envir = private)(event)
      if (!is_scalar_bool(sf)){
        warning(
          "`should_flush()` did not return `TRUE` or `FALSE` but ",
          preview_object(sf), ". ",
          "Please set a proper filter function with `$set_should_flush()`"
        )
        sf <- FALSE
      }

      if (sf){
        self[["flush"]]()
      } else if (i > bs){
        if (get(".flush_on_rotate", envir = private) ){
          self[["flush"]]()
        } else {
          assign("insert_pos", 0L, envir = private)
        }
      }

    NULL
    },

    flush = function(){},


    set_buffer_size = function(x){
      assert(is_scalar_integerish(x))
      private$.buffer_size <- x
      invisible(self)
    },

    set_should_flush = function(x){
      if (is.null(x)) x <- function(event) FALSE
      assert_filter(x)
      private$.should_flush <- x
      invisible(self)
    },

    set_flush_on_exit = function(x){
      assert(is_scalar_bool(x))
      private$.flush_on_exit <- x
      invisible(self)
    },

    set_flush_on_rotate = function(x){
      assert(is_scalar_bool(x))
      private$.flush_on_rotate <- x
      invisible(self)
    },

    set_flush_threshold = function(level){
      level <- standardize_threshold(level)
      private$.flush_threshold <- level
      invisible(self)
    },

    show = function(threshold = NA_integer_, n = 20L){
      assert(is_scalar_integerish(n))
      threshold <- standardize_threshold(threshold)

      if (is.na(threshold)) threshold <- Inf
      dd <- get("buffer_dt", envir = self)

      if (identical(nrow(dd),  0L)){
        cat("[empty log]")
        return(invisible(NULL))
      }

      dd <- tail(dd[dd$level <= threshold, ], n)
      dd <- as.environment(dd)
      assign("logger", self[[".logger"]], dd)
      cat(self$layout$format_event(dd), sep = "\n")
      invisible(dd)
    }
  ),



  # +- active ---------------------------------------------------------------
  active = list(
    flush_on_exit = function() {
      get(".flush_on_exit", private)
    },

    flush_on_rotate = function() {
      get(".flush_on_rotate", private)
    },

    should_flush = function(){
      get(".should_flush", private)
    },

    buffer_size = function() {
      get(".buffer_size", private)
    },

    flush_threshold = function(){
      get(".flush_threshold", private)
    },

    buffer_events = function() {
      ord <- get("event_order", envir = private)
      ord <- ord - min(ord) + 1L
      ord <- order(ord)
      res <- get(".buffer_events", envir = private)[ord]
      res[!vapply(res, is.null, FALSE)]
    },

    buffer_df = function() {
      as.data.frame(self[["buffer_dt"]])
    },

    buffer_dt = function(){
      assert_namespace("data.table")
      dd <- lapply(
        get("buffer_events", self),
        function(.x){
          vals <- .x$values
          list_cols <- !vapply(vals, is.atomic, TRUE)
          vals[list_cols] <- lapply(vals[list_cols], list)
          data.table::as.data.table(vals)
        }
      )

      data.table::rbindlist(dd, fill = TRUE, use.names = TRUE)
    }
  ),

  # +- private  ---------------------------------------------------------
  private = list(
    initialize_buffer = function(buffer_size){
      assert(is_n0(buffer_size))
      private[["insert_pos"]]  <- 0L
      private[["last_event"]]  <- 0L
      private[["event_order"]] <- seq_len(buffer_size)
      private[[".buffer_size"]] <- buffer_size
      private[[".buffer_events"]] <- list()
    },

    insert_pos = NULL,
    last_event = NULL,
    event_order = NULL,

    .flush_threshold = NULL,
    .should_flush = NULL,
    .flush_on_exit = NULL,
    .flush_on_rotate = NULL,
    .buffer_size = NULL,
    .buffer_events = NULL
  )
)




# AppenderBuffer --------------------------------------------------

#' Log to a Memory Buffer
#'
#' An Appender that Buffers LogEvents in-memory and and redirects them to other
#' Appenders once certain conditions are met.
#'
#' @eval r6_usage(AppenderBuffer)
#'
#' @section Creating a Buffer Appender:
#'
#' The [Layout] for this Appender is used only to format console output of
#' its `$show()` method.
#'
#' @inheritSection AppenderMemory Methods
#' @inheritSection AppenderMemory Fields
#'
#' @section Fields:
#'
#' \describe{
#'   \item{`appenders`, `set_appenders()`}{Like for a [Logger]. Buffered events
#'     will be passed on to these Appenders once a flush is triggered}
#'   \item{`flush_on_exit, set_flush_on_exit(x)`}{`TRUE` or `FALSE`: Whether the
#'     buffer should be flushed when the Appender is garbage collected (f.e when
#'     you close \R)}
#'   \item{`flush_on_rotate, set_flush_on_rotate`}{`TRUE` or `FALSE`: Whether
#'     the buffer should be flushed when the Buffer is full (f.e when you close
#'     \R). Setting this to off can have slightly negative performance impacts.}
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{`flush()`}{Manually trigger flushing}
#'
#'   \item{`add_appender(appender, name = NULL)`, `remove_appender(pos)`}{
#'     Add or remove an [Appender]. Supplying a `name` is optional but
#'     recommended. After adding an Appender with
#'     `appender$add_appender(AppenderConsole$new(), name = "console")` you can
#'      refer to it via `appender$appenders$console`. `remove_appender()` can
#'      remove an Appender by position or name.
#'    }
#'
#' }
#'
#' @inheritSection AppenderDt Comparison AppenderBuffer and AppenderDt
#'
#' @export
#' @seealso [LayoutFormat]
#' @family Appenders
#' @name AppenderBuffer
NULL




#' @export
AppenderBuffer <- R6::R6Class(
  "AppenderBuffer",
  inherit = AppenderMemory,
  cloneable = FALSE,
  public = list(
    initialize = function(
      threshold = NA_integer_,
      layout = LayoutFormat$new(
        fmt = "%L [%t] %m",
        timestamp_fmt = "%H:%M:%S",
        colors = getOption("lgr.colors")
      ),
      appenders = NULL,
      buffer_size = 1e3,
      flush_threshold = "fatal",
      flush_on_exit = TRUE,
      flush_on_rotate = TRUE,
      should_flush = default_should_flush,
      filters = NULL
    ){
      self$set_threshold(threshold)

      private$initialize_buffer(buffer_size)

      self$set_should_flush(should_flush)
      self$set_flush_threshold(flush_threshold)
      self$set_flush_on_exit(flush_on_exit)
      self$set_flush_on_rotate(flush_on_rotate)
      self$set_filters(filters)

      self$set_appenders(appenders)
      self$set_layout(layout)

      invisible(self)
    },

    flush = function(){
      for (event in get("buffer_events", envir = self)){
        for (app in self$appenders) {
          if (app$filter(event))  app$append(event)
        }
      }

      assign("insert_pos", 0L, envir = private)
      private$.buffer_events <- list()
      invisible(self)
    },



    set_appenders = function(x){
      if (is.null(x)){
        private$.appenders <- list()
        return(invisible())
      }
      if (inherits(x, "Appender"))  x <- list(x)

      assert(
        is.list(x) && all(vapply(x, inherits, TRUE, "Appender")),
        "'appenders' must either be a single Appender, a list thereof, or ",
        "NULL for no appenders."
      )

      private$.appenders <- list()

      for (i in seq_along(x))
        self$add_appender(x[[i]], name = names(x)[[i]])

      invisible(self)
    },


    add_appender = function(
      appender,
      name = NULL
    ){
      assert(inherits(appender, "Appender"))

      private$.appenders[[length(private$.appenders) + 1L]] <- appender
      if (!is.null(name))
        names(private$.appenders)[length(private$.appenders)] <- name

      invisible(self)
    },


    remove_appender = function(
      pos
    ){
      if (is.numeric(pos)){
        assert(
          all(pos %in% seq_along(private$.appenders)),
          "'pos' is out of range of the length of appenders (1:",
          length(private$.appenders), ")"
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

      for (nm in pos){
        private$.appenders[[nm]] <- NULL
      }

      invisible(self)
    }
  ),



  # +- active ---------------------------------------------------------------
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

      private$.appenders <- value
      invisible()
    },

    destination = function() paste(length(self$appenders), "child Appenders")
  ),

  # +- private  ---------------------------------------------------------
  private = list(
    finalize = function(){
      if (self$flush_on_exit) self$flush()
      # Ensure child appenders are gc'ed first. This ensures more predictable
      # behaviour when destroying an AppenderBuffer.
      for (i in rev(seq_along(self$appenders))){
        self$remove_appender(i)
        gc()
      }
      invisible()
    },

    .appenders = list()
  )
)




# AppenderDbi -------------------------------------------------------------


#' Log to Databases via DBI
#'
#' Log to a database table with any **DBI** compatible backend. Please be
#' aware that AppenderDbi does *not* support case sensitive / quoted column
#' names, and you advised to only use all-lowercase names for
#' custom fields (see `...` argument of [LogEvent]).
#' When appending to a database table all LogEvent values for which a column
#' exists in the target table will be appended, all others are ignored.
#'
#' @section Buffered Logging:
#'
#' AppenderDbi does not write directly to the database but to an in memory
#' buffer. With the default settings, this buffer is written to the database
#' whenever the buffer is full (`buffer_size`, default is 10 LogEvents),
#' whenever a LogEvent with a level of `fatal` or `error` is encountered
#' (`flush_threshold`) or when the Appender is garbage collected
#' (`flush_on_exit`), i.e. when you close the \R session or shortly after you
#' remove the Appender object via `rm()` or the likes. If you want to avoid
#' this behaviour, just set `buffer_size` to `0`.
#'
#' @eval r6_usage(AppenderDbi)
#'
#' @inheritSection Appender Creating a New Appender
#' @inheritSection AppenderMemory Fields
#' @inheritSection AppenderMemory Methods
#'
#' @section Creating a New Appender:
#'
#' An AppenderDbi is linked to a database table via its `table` argument. If
#' the table does not exist it is created either when the Appender is first
#' instantiated or (more likely) when the first LogEvent would be written to
#' that table. Rather than to rely on this feature, it is recommended that you
#' create the target log table first manually using an `SQL CREATE TABLE`
#' statement as this is safer and more flexible. See also [LayoutDbi].
#'
#'
#' @section Fields:
#'
#' Note: `$data` and `show()` query the data from the remote database and might
#'   be slow for very large logs.
#'
#' \describe{
#'   \item{`close_on_exit`, `set_close_on_exit()`}{`TRUE` or `FALSE`. Close the
#'   Database connection when the Logger is removed?}
#'   \item{`conn`, `set_conn(conn)`}{a [DBI connection][DBI::dbConnect]}
#'   \item{`table`}{Name of the target database table}
#' }
#'
#' @section Choosing the Right DBI Layout:
#'
#' Layouts for relational database tables are tricky as they have very strict
#' column types and further restrictions. On top of that implementation details
#' vary between database backends.
#'
#' To make setting up `AppenderDbi` as painless as possible, the helper
#' function [select_dbi_layout()] tries to automatically determine sensible
#' [LayoutDbi] settings based on `conn` and - if it exists in the database
#' already - `table`. If `table` does not
#' exist in the database and you start logging, a new table will be created
#' with the `col_types` from `layout`.
#'
#' @export
#' @family Appenders
#' @name AppenderDbi
NULL




#' @export
AppenderDbi <- R6::R6Class(
  "AppenderDbi",
  inherit = AppenderMemory,
  cloneable = FALSE,
  public = list(
    initialize = function(
      conn,
      table,
      threshold = NA_integer_,
      layout = select_dbi_layout(conn, table),
      close_on_exit = TRUE,
      buffer_size = 10,
      flush_threshold = "error",
      flush_on_exit = TRUE,
      flush_on_rotate = TRUE,
      should_flush = default_should_flush,
      filters = NULL
    ){
      assert_namespace("DBI", "data.table")

      # appender
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)

      # buffer
      private$initialize_buffer(buffer_size)

      # flush conditions
      self$set_should_flush(should_flush)
      self$set_flush_threshold(flush_threshold)
      self$set_flush_on_exit(flush_on_exit)
      self$set_flush_on_rotate(flush_on_rotate)

      # database
      self$set_conn(conn)
      private[[".table"]] <- table
      self$set_close_on_exit(close_on_exit)

      if (DBI::dbExistsTable(self$conn, table)){
        # do nothing
      } else if (is.null(self$layout$col_types)) {
        message(paste0("Creating '", fmt_tname(table), "' on first log. "))

      } else {
        message("Creating '", fmt_tname(table), "' with manually specified column types")
        DBI::dbExecute(conn, layout$sql_create_table(table))
      }
    },

    set_close_on_exit = function(x){
      assert(is_scalar_bool(x))
      private$.close_on_exit <- x
      invisible(self)
    },

    set_conn = function(conn){
      assert(inherits(conn, "DBIConnection"))
      private$.conn <- conn
      invisible(self)
    },

    show = function(
      threshold = NA_integer_,
      n = 20
    ){
      assert(is_n0(n))
      threshold <- standardize_threshold(threshold)
      if (is.na(threshold)) threshold <- Inf

      dd <- tail(self$data[self$data$level <= threshold, ], n)
      colors <- getOption("lgr.colors")
      lo <- get(".layout", envir = private)

      if (identical(nrow(dd),  0L)){
        cat("[empty log]")
      } else {
        walk(
          as_LogEvent_list.data.frame(dd, na.rm = TRUE),
          function(.x){
            cat(lo$format_event(.x), "\n", sep = "")
          }
        )
      }

      invisible(dd)
    },

    flush = function(){
      lo <- get(".layout", envir = private)

      table  <- get("table", envir = self)
      buffer <- get("buffer_dt", envir = self)

      if (length(buffer)){
        dd <- lo[["format_data"]](buffer)
        cn <- names(get("col_types", envir = self))

        if (!is.null(cn))
          dd <- dd[, intersect(cn, names(dd))]

        DBI::dbWriteTable(
          conn  = get(".conn", envir = private),
          name  = table,
          value = dd,
          row.names = FALSE,
          append = TRUE
        )
      }

      assign("insert_pos", 0L, envir = private)
      private$.buffer_events <- list()
      invisible(self)
    }
  ),


  # +- active ---------------------------------------------------------------
  active = list(
    destination = function() private$.table,
    conn = function(){
      private$.conn
    },

    close_on_exit = function(){
      private$.close_on_exit
    },

    col_types = function(){
      if (is.null(get(".col_types", envir = private))){
        ct <- get_col_types(private[[".conn"]], self[["table"]])
        if (is.null(ct)) return (NULL)
        names(ct) <- get("layout", envir = self)[["format_colnames"]](names(ct))
        private$set_col_types(ct)
        return(ct)
      } else {
        get(".col_types", envir = private)
      }
    },

    table = function(){
      self[["layout"]][["format_table_name"]](get(".table", envir = private))
    },

    data = function(){
      tbl <- get("table", envir = self)

      if (DBI::dbExistsTable(private[[".conn"]], tbl)){
        dd <- DBI::dbReadTable(private[[".conn"]], tbl)
      } else {
        return(NULL)
      }

      names(dd) <- tolower(names(dd))
      dd[["timestamp"]] <- as.POSIXct(dd[["timestamp"]])
      dd[["level"]] <- as.integer(dd[["level"]])
      dd
    }
  ),

  # +- private -------------------------------------------------------------
  private = list(
    finalize = function() {
      if (self$flush_on_exit)
        self$flush()

      if (self$close_on_exit){
        try(DBI::dbDisconnect(private$.conn), silent = TRUE)
      }
    },

    set_col_types = function(x){
      if (!is.null(x)){
        assert(is.character(x))
        assert(identical(length(names(x)), length(x)))
      }
      private$.col_types <- x
      invisible(self)
    },

    .col_types = NULL,
    .conn = NULL,
    .table = NULL,
    .close_on_exit = NULL
  )
)




# AppenderRjdbc -------------------------------------------------------------

#' Log to Databases via RJDBC
#'
#' Log to a database table with the **RJDBC** package. **RJDBC** is only
#' somewhat  **DBI** compliant and does not work with [AppenderDbi].
#' **I do not recommend using RJDBC if it can be avoided.**. AppenderRjdbc
#' is only tested for DB2 databases, and it is likely it will not work properly
#' for other databases. Please file a bug report if you encounter any issues.
#'
#' @inheritSection AppenderDbi Creating a New Appender
#' @inheritSection AppenderDbi Choosing the Right DBI Layout
#' @inheritSection AppenderDbi Fields
#' @inheritSection AppenderDbi Methods
#'
#' @eval r6_usage(AppenderRjdbc)
#'
#'
#' @section Fields:
#' @section Methods:
#'
#' @export
#' @seealso [LayoutFormat], [simple_logging], [data.table::data.table]
#' @family Appenders
#' @name AppenderRjdbc
NULL




# exclude from coverage because relies on external ressources
# nocov start
#' @export
AppenderRjdbc <- R6::R6Class(
  "AppenderRjdbc",
  inherit = AppenderDbi,
  cloneable = FALSE,
  public = list(
    initialize = function(
      conn,
      table,
      threshold = NA_integer_,
      layout = select_dbi_layout(conn, table),
      close_on_exit = TRUE,
      buffer_size = 10,
      flush_threshold = "error",
      flush_on_exit = TRUE,
      flush_on_rotate = TRUE,
      should_flush = default_should_flush,
      filters = NULL
    ){
      assert_namespace("DBI", "RJDBC", "data.table")

      # appender
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)

      # buffer
      private$initialize_buffer(buffer_size)

      # flush conditions
      self$set_should_flush(should_flush)
      self$set_flush_threshold(flush_threshold)
      self$set_flush_on_exit(flush_on_exit)
      self$set_flush_on_rotate(flush_on_rotate)

      # database
      private[[".conn"]]  <- conn
      private[[".table"]] <- table
      self$set_close_on_exit(close_on_exit)

      table_exists <- try(DBI::dbGetQuery(conn, paste("SELECT 1 FROM", table)), silent = TRUE)
      table_exists <- !inherits(table_exists, "try-error")

      if (!table_exists) {
        message("Creating '", fmt_tname(table), "' with manually specified column types")
        RJDBC::dbSendUpdate(conn, layout$sql_create_table(toupper(table)))
      }
    },


    flush = function(){
      lo <- get(".layout", envir = private)

      table  <- get("table", envir = self)
      buffer <- get("buffer_dt", envir = self)

      if (length(buffer)){
        dd <- lo[["format_data"]](buffer)
        cn <- names(get("col_types", envir = self))

        if (!is.null(cn))
          dd <- dd[, intersect(cn, names(dd))]

        for (i in seq_len(nrow(dd))){
          data <- as.list(dd[i, ])
          q <-  sprintf(
            "INSERT INTO %s (%s) VALUES (%s)",
            private$.table,
            paste(names(data), collapse = ", "),
            paste(rep("?", length(data)), collapse = ", ")
          )
          RJDBC::dbSendUpdate(get(".conn", private), q, list=data)
        }

        assign("insert_pos", 0L, envir = private)
        private$.buffer_events <- list()
        invisible(self)
      }
    }
  ),


  active = list(
    data = function(){
      dd <- try(DBI::dbGetQuery(self$conn, paste("SELECT * FROM", self$table)))

      if (inherits(dd, "try-error"))
        return(NULL)

      names(dd) <- tolower(names(dd))

      dd[["timestamp"]] <- as.POSIXct(dd[["timestamp"]])
      dd[["level"]] <- as.integer(dd[["level"]])
      dd
    }
  ),


  private = list(
    .conn = NULL,
    .table = NULL
  )
)




# nocov end



# AppenderDigest --------------------------------------------------------

#' Abstract Class for Digests
#'
#' @template abstract_class
#'
#' @description
#' Abstract class for Appenders that transmit digests of several log events
#' at once, for example [AppenderPushbullet], [AppenderGmail] and
#' [AppenderSendmail].
#'
#' @inheritSection AppenderMemory Methods
#' @inheritSection AppenderMemory Fields
#'
#' @section Fields:
#'
#' \describe{
#'   \item{`subject_layout`, `set_layout(subject_layout)`}{Like `layout`, but
#'     used to format the subject/title of the digest. While `layout` is applied
#'     to each LogEvent of the digest, `subject_layout` is only applied to
#'     the last one.
#'   }
#' }
#'
#' @section Methods:
#'
#' @export
#' @seealso [LayoutFormat], [LayoutGlue]
#' @name AppenderDigest
NULL




AppenderDigest <-  R6::R6Class(
    "AppenderDigest",
    inherit = AppenderMemory,
    cloneable = FALSE,

    # +- public --------------------------------------------------------------
    public = list(

      set_subject_layout = function(layout){
        assert(inherits(layout, "Layout"))
        private$.subject_layout <- layout
        invisible(self)
      }
    ),

    active = list(
      subject_layout = function() get(".subject_layout", private)
    ),

    private = list(
      .subject_layout = NULL
    )
  )




# AppenderPushbullet --------------------------------------------------------


#' Send Push-Notifications via RPushbullet
#'
#' Send push notifications via [pushbullet](https://www.pushbullet.com/). This
#' Appender keeps an in-memory buffer like [AppenderBuffer]. If the buffer is
#' flushed, usually because an event of specified magnitude is encountered, all
#' buffered events are concatenated to a single message that is sent to
#' [RPushbullet::pbPost()]. The default behaviour is to push the last 7 log
#' events in case a `fatal` event is encountered.
#'
#' @eval r6_usage(
#'   AppenderPushbullet,
#'   ignore = c(
#'     "set_flush_on_exit",
#'     "set_flush_on_rotate",
#'     "flush_on_exit",
#'     "flush_on_rotate")
#' )
#'
#'
#' @inheritSection AppenderMemory Methods
#' @inheritSection AppenderMemory Fields
#'
#' @section Fields:
#'
#' \describe{
#'   \item{`apikey`, `recipients`, `email`, `channel`, `devices`}{
#'     See [RPushbullet::pbPost()]
#'   }
#' }
#'
#' @section Methods:
#'
#' @export
#' @family Appenders
#' @seealso [LayoutFormat], [LayoutGlue]
#' @name AppenderPushbullet
NULL




#' @export
AppenderPushbullet <- R6::R6Class(
  "AppenderPushbullet",
  inherit = AppenderDigest,
  cloneable = FALSE,

  # +- public --------------------------------------------------------------
  public = list(
    initialize = function(
      threshold = NA_integer_,
      flush_threshold = "fatal",
      layout = LayoutFormat$new(fmt = "%K  %t> %m %f", timestamp_fmt = "%H:%M:%S"),
      subject_layout = LayoutFormat$new(fmt = "[LGR] %L: %m"),
      buffer_size = 6,
      recipients = NULL,
      email = NULL,
      channel = NULL,
      devices = NULL,
      apikey = NULL,
      filters = NULL
    ){
      private$initialize_buffer(buffer_size)
      self$set_flush_on_rotate(FALSE)
      self$set_flush_on_exit(FALSE)
      self$set_filters(filters)

      self$set_layout(layout)
      self$set_threshold(threshold)
      self$set_flush_threshold(flush_threshold)
      self$set_apikey(apikey)
      self$set_buffer_size(buffer_size)
      self$set_subject_layout(subject_layout)
      self$set_should_flush(function(event){
        is.na(.obj()[["flush_threshold"]]) || all(event[["level"]] <= .obj()[["flush_threshold"]])
      })

      self$set_recipients(recipients)
      self$set_email(email)
      self$set_channel(channel)
      self$set_devices(devices)
    },

    flush = function(
    ){
      assign("insert_pos", 0L, envir = private)

      body <- paste(
        unlist(lapply(self$buffer_events, self$layout$format_event)),
        collapse = "\n"
      )
      le    <- self$buffer_events[[length(self$buffer_events)]]
      title <- self$subject_layout$format_event(le)

      cl <- list(
        type = "note",
        title = title,
        body  = body
      )

      if (!is.null(self$apikey)){
        cl$apikey <- self$apikey
        cl$recipients <- self$recipients
        cl$email <- self$email
        cl$channel <- self$channel
        cl$devices <- self$devices
      }

      do.call(RPushbullet::pbPost, compact(cl))
      private$.buffer_events <- list()

      invisible(self)
    },

    set_apikey = function(x){
      assert(is.null(x) || is_scalar_character(x))
      private$.apikey <- x
      invisible(self)
    },

    set_recipients = function(x){
      private$.recipients <- x
      invisible(self)
    },

    set_email = function(x){
      private$.email <- x
      invisible(self)
    },

    set_channel = function(x){
      private$.channel <- x
      invisible(self)
    },

    set_devices = function(x){
      private$.devices <- x
      invisible(self)
    }
  ),


  # +- active ---------------------------------------------------------------
  active = list(
    apikey = function() private$.apikey
  ),


  private = list(
    .apikey = NULL,
    .recipients = NULL,
    .email = NULL,
    .channel = NULL,
    .devices = NULL
  )
)




# AppenderMail ------------------------------------------------------------

#' Abstract Class for Email Appenders
#'
#' @template abstract_class
#'
#' @inheritSection AppenderDigest Fields
#' @inheritSection AppenderDigest Methods
#'
#'@section Fields:
#' \describe{
#'   \item{`to`, `from`, `cc`, `bcc`}{
#'     `character` vectors.
#'   }
#'   \item{`html`, `set_html()`}{`TRUE` or `FALSE`. Send a html email message?
#'     This does currently only formats the log contents as monospace verbatim
#'     text.
#'   }
#' }
#'
#' @name AppenderMail
#' @keywords internal
#'
AppenderMail <- R6::R6Class(
  "AppenderGmail",
  inherit = AppenderDigest,
  cloneable = FALSE,

  # +- public --------------------------------------------------------------
  public = list(
    set_to = function(x){
      private$.to <- x
      invisible(self)
    },

    set_from = function(x){
      private$.from <- x
      invisible(self)
    },

    set_cc = function(x){
      private$.cc <- x
      invisible(self)
    },

    set_bcc = function(x){
      private$.bcc <- x
      invisible(self)
    },

    set_html = function(x){
      assert(is_scalar_bool(x))
      private$.html <- x
      invisible(self)
    }
  ),


  # +- active ---------------------------------------------------------------
  active = list(
    to = function() get(".to", envir = private),
    from = function() get(".from", envir = private),
    cc = function() get(".cc", envir = private),
    bcc = function() get(".bcc", envir = private),
    html = function() get(".html", envir = private)
  ),

  private = list(
    .to = NULL,
    .from = NULL,
    .cc = NULL,
    .bcc = NULL,
    .html = NULL
  )
)




# AppenderSendmail --------------------------------------------------------

#' Send Log Emails via sendmailR
#'
#' Send mails via [sendmailR::sendmail()], which requires that you have access
#' to an SMTP server that does not require authentication. This
#' Appender keeps an in-memory buffer like [AppenderBuffer]. If the buffer is
#' flushed, usually because an event of specified magnitude is encountered, all
#' buffered events are concatenated to a single message. The default behaviour
#' is to push the last 30 log events in case a `fatal` event is encountered.
#'
#' @note The default Layout's `fmt` indents each log entry with 3 blanks. This
#'   is a workaround so that Microsoft Outlook does not mess up the line breaks.
#'
#'
#' @eval r6_usage(
#'   AppenderSendmail,
#'   ignore = c(
#'     "set_flush_on_exit",
#'     "set_flush_on_rotate",
#'     "flush_on_exit",
#'     "flush_on_rotate")
#' )
#'
#'
#' @inheritSection AppenderMail Methods
#' @inheritSection AppenderMail Fields
#'
#' @section Fields:
#'
#' \describe{
#'   \item{`headers`, `control`}{see [sendmailR::sendmail()]}
#' }
#'
#'
#' @section Methods:
#'
#'
#' @export
#' @seealso [LayoutFormat], [LayoutGlue]
#' @family Appenders
#' @name AppenderSendmail
NULL




#' @export
AppenderSendmail <- R6::R6Class(
  "AppenderSendmail",
  inherit = AppenderMail,
  cloneable = FALSE,

  # +- public --------------------------------------------------------------
  public = list(
    initialize = function(
      to,
      control,
      threshold = NA_integer_,
      flush_threshold = "fatal",
      layout = LayoutFormat$new(fmt = "   %L [%t] %m %f", timestamp_fmt = "%H:%M:%S"),
      subject_layout = LayoutFormat$new(fmt = "[LGR] %L: %m"),
      buffer_size = 29,
      from = get_user(),
      cc = NULL,
      bcc = NULL,
      html = FALSE,
      headers = NULL,
      filters = NULL
    ){
      assert_namespace("sendmailR")
      assert(is_scalar_bool(html))

      private$initialize_buffer(buffer_size)
      self$set_flush_on_rotate(FALSE)
      self$set_flush_on_exit(FALSE)
      self$set_filters(filters)

      self$set_to(to)
      self$set_control(control)
      self$set_from(from)
      self$set_cc(cc)
      self$set_bcc(bcc)
      self$set_headers(headers)
      self$set_subject_layout(subject_layout)
      self$set_html(html)

      self$set_layout(layout)
      self$set_threshold(threshold)
      self$set_flush_threshold(flush_threshold)
      self$set_should_flush(default_should_flush)
    },

    flush = function(
    ){
      assign("insert_pos", 0L, envir = private)

      # body
      body <- paste(
        unlist(lapply(self$buffer_events, self$layout$format_event)),
        collapse = "\r\n"
      )
      if (self$html) {
        body <- paste0("<pre>\n", body, "</pre>\n")
        body <- sendmailR::mime_part(body)
        body[["headers"]][["Content-Type"]] <- "text/html; charset=UTF-8;"
      } else {
        body <- sendmailR::mime_part(body)
        body[["headers"]][["Content-Type"]] <- "text/plain; charset=UTF-8;"
      }


      # title
      le    <- self$buffer_events[[length(self$buffer_events)]]
      title <- self$subject_layout$format_event(le)
      title <- try(
        iconv(title, from = "UTF-8", to = "ASCII//TRANSLIT"),
        silent = TRUE
      )


      args <- list(
        from = self$from,
        to   = self$to,
        subject = title,
        msg = body,
        cc = self$cc,
        bcc = self$bcc,
        headers = self$headers,
        control = self$control
      )

      # sendmailR expects missing() instead of NULL for default values
      args <- compact(args)

      do.call(sendmailR::sendmail, args)
      private$.buffer_events <- list()

      invisible(self)
    },

    set_control = function(x){
      private$.control <- x
    },

    set_headers = function(x){
      private$.headers <- x
    }
  ),


  # +- active ---------------------------------------------------------------
  active = list(
    control = function() get(".control", envir = private),
    headers = function() get(".headers", envir = private)
  ),

  private = list(
    .control = NULL,
    .headers = NULL
  )
)




# AppenderGmail --------------------------------------------------------


#' Send Log Emails via gmailr
#'
#' Send mails via [gmailr::send_message()]. This
#' Appender keeps an in-memory buffer like [AppenderBuffer]. If the buffer is
#' flushed, usually because an event of specified magnitude is encountered, all
#' buffered events are concatenated to a single message. The default behaviour
#' is to push the last 30 log events in case a `fatal` event is encountered.
#'
#' @eval r6_usage(
#'   AppenderGmail,
#'   ignore = c(
#'     "set_flush_on_exit",
#'     "set_flush_on_rotate",
#'     "flush_on_exit",
#'     "flush_on_rotate")
#' )
#'
#' @inheritSection AppenderMail Methods
#' @inheritSection AppenderMail Fields
#'
#' @section Fields:
#'
#' @section Methods:
#'
#' @export
#' @seealso [LayoutFormat], [LayoutGlue]
#' @family Appenders
#' @name AppenderGmail
NULL




#' @export
AppenderGmail <- R6::R6Class(
  "AppenderGmail",
  inherit = AppenderMail,
  cloneable = FALSE,

  # +- public --------------------------------------------------------------
  public = list(
    initialize = function(
      to,
      threshold = NA_integer_,
      flush_threshold = "fatal",
      layout = LayoutFormat$new(fmt = "%L [%t] %m %f", timestamp_fmt = "%H:%M:%S"),
      subject_layout = LayoutFormat$new(fmt = "[LGR] %L: %m"),
      buffer_size = 30,
      from = get_user(),
      cc = NULL,
      bcc = NULL,
      html = FALSE,
      filters = NULL
    ){
      assert_namespace("gmailr")

      private$initialize_buffer(buffer_size)
      self$set_flush_on_rotate(FALSE)
      self$set_flush_on_exit(FALSE)
      self$set_filters(filters)

      self$set_to(to)
      self$set_from(from)
      self$set_cc(cc)
      self$set_bcc(bcc)
      self$set_html(html)
      self$set_subject_layout(subject_layout)

      self$set_layout(layout)
      self$set_threshold(threshold)
      self$set_flush_threshold(flush_threshold)
      self$set_buffer_size(buffer_size)
      self$set_should_flush(default_should_flush)

      private$.flush_on_exit   <- FALSE
      private$.flush_on_rotate <- FALSE
    },

    flush = function(
    ){
      assign("insert_pos", 0L, envir = private)

      body <- paste(
        unlist(lapply(self$buffer_events, self$layout$format_event)),
        collapse = "\n"
      )
      le    <- self$buffer_events[[length(self$buffer_events)]]
      title <- self$subject_layout$format_event(le)
      title <- try(
        iconv(title, from = "UTF-8", to = "ASCII//TRANSLIT"),
        silent = TRUE
      )

      mail <- gmailr::mime()
      mail <- gmailr::to(mail, self$to)
      mail <- gmailr::from(mail, self$from)
      mail <- gmailr::cc(mail, self$cc)
      mail <- gmailr::bcc(mail, self$bcc)
      mail <- gmailr::subject(mail, title)

      if (self$html){
        mail <- gmailr::html_body(mail, paste0("<pre>\n", body, "</pre>\n"))
      } else {
        mail <- gmailr::text_body(mail, body)
      }

      gmailr::send_message(mail)
      private$.buffer_events <- list()

      invisible(self)
    }
  ),


  # +- active ---------------------------------------------------------------

  private = list(
    .to = NULL,
    .from = NULL,
    .cc = NULL,
    .bcc = NULL,
    .html = NULL
  )
)




# utils -------------------------------------------------------------------

# trim multi-valued events from vectorized inserts to the buffer size
trim_to_buffer_size <- function(x, buffer_size){
  if (length(x) <= buffer_size)
    x
  else
    x[seq.int(length(x) - buffer_size + 1L, length(x))]
}




fmt_tname <- function(x){
  if (inherits(x, "Id")){
    paste0("<Id: ", trimws(gsub("<Id>", "", utils::capture.output(print(x)))), ">")
  } else {
    x
  }
}
