#' Appenders
#'
#' @description
#' Appenders are assigned to [Loggers] and manage the output of the [LogEvents]
#' to a destination (such as the console or a text file). An Appender has a
#' single [Layout] that tells it how to format the LogEvent. For details
#' please refer to the documentations of the specific Appenders.
#'
#' The Appender class itself **is not designed for direct usage**, but it is the
#' basis on which all other Appenders are built. Please see the **see also**
#' section towards the end of this document a list of available Appenders.
#'
#' More appenders can be found in the package [lgr.app](https://github.com/s-fleck/lgr.app)
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
      if (is_scalar_list(layout)){
        # set_layout also accepts a list of length 1 containing a single Layout
        # object. This is a workaround for resolve_r6_ctors. This behaviour is
        # intentionally not documented and you should not rely on it.
        layout <- layout[[1]]
      }
      assert(
        inherits(layout, "Layout"),
        "`layout` must a `Layout` object, but is ", preview_object(layout)
      )
      private$.layout <- layout
      invisible(self)
    },

    format = function(
      color = FALSE,
      ...
    ){
      assert(is_scalar_bool(color))
      if (!color)
        style_subtle <- identity

      thr <- fmt_threshold(self$threshold, type = "character")

      header <- paste(
        paste0("<", class(self)[[1]], "> [", thr, "]")
      )

      if (!is.null(self$layout)){
        res <- c(
          header,
          paste0("  layout: ", self$layout$toString())
        )
      }

      if (!is.null(self$destination)){
        res <- c(
          res, paste("  destination:", self$destination)
        )
      }

      if (!color)
        style_error <- identity

      if (class(self)[[1]] == "Appender"){
        paste(res[[1]], style_error("[abstract class]"))
      } else {
        res
      }
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

#' Log to the console
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
#' lg  <- get_logger("test")$set_propagate(FALSE)
#'
#' lg$add_appender(AppenderConsole$new())
#' lg$add_appender(AppenderConsole$new(
#'   layout = LayoutFormat$new("[%t] %c(): [%n] %m", colors = getOption("lgr.colors"))))
#'
#' # Will output the message twice because we attached two console appenders
#' lg$warn("A test message")
#' lg$config(NULL) # reset config
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

#' Log to a file
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
#'   file. If the file does not exist it will be created.}
#'  }
#'
#'
#' @export
#' @seealso [LayoutFormat], [LayoutJson]
#' @family Appenders
#' @name AppenderFile
#'
#' @examples
#' lg <- get_logger("test")
#' default <- tempfile()
#' fancy <- tempfile()
#' json <- tempfile()
#'
#' lg$add_appender(AppenderFile$new(default), "default")
#' lg$add_appender(
#'   AppenderFile$new(fancy, layout = LayoutFormat$new("[%t] %c(): %L %m")), "fancy"
#' )
#' lg$add_appender(
#'   AppenderFile$new(json, layout = LayoutJson$new()), "json"
#' )
#'
#' lg$info("A test message")
#'
#' readLines(default)
#' readLines(fancy)
#' readLines(json)
#'
#' # cleanup
#' lg$config(NULL)
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
      assert(
        is_scalar_character(file),
        "`file` must be character scalar, not: ", preview_object(file)
      )

      assert(
        dir.exists(dirname(file)),
        "Cannot create file: directory '", dirname(file), "' does not exist."
      )

      private$.file <- file
      if (!file.exists(file)) { assert(
          file.create(file, showWarnings = FALSE),
          "Cannot create file '", file , "'"
      )}

      invisible(self)
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

#' Log to a JSON file
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
#' lg <- get_logger("test")$
#'   set_appenders(AppenderJson$new(tf))$
#'   set_propagate(FALSE)
#'
#' lg$info("A test message")
#' lg$info("A test message %s strings", "with format strings", and = "custom_fields")
#'
#' lg$appenders[[1]]$show()
#' lg$appenders[[1]]$data
#'
#' # cleanup
#' lg$config(NULL)
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
      assert_namespace("jsonlite")

      self$set_file(file)
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)
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

#' Abstract class for logging to tabular structures
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
    show = function(threshold = NA_integer_, n = 20L) NULL,

    format = function(
      color = FALSE,
      ...
    ){
      res <- super$format(color = color, ...)

      if (!color)
        style_error <- identity

      if (class(self)[[1]] == "AppenderTable"){
        paste(res[[1]], style_error("[abstract class]"))
      } else {
        res
      }
    }
  ),

  active = list(
    data = function() as.data.frame(self$dt),
    dt   = function() NULL
  )
)




# nocov end



# AppenderDt ----------------------------------------------------------

#' Log to an in-memory data.table
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
#' lg <- get_logger("test")
#' lg$config(list(
#'   appenders = list(memory = AppenderBuffer$new()),
#'   threshold = NA,
#'   propagate = FALSE  # to prevent routing to root logger for this example
#' ))
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
#' lg$config(NULL)
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
      assert_namespace("data.table")
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

#' Abstract class for logging to memory buffers
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
    },

    format = function(
      color = FALSE,
      ...
    ){
      res <- super$format(color = color, ...)

      if (!color)
        style_error <- identity

      if (class(self)[[1]] == "AppenderMemory"){
        paste(res[[1]], style_error("[abstract class]"))
      } else {
        res
      }
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

    data = function(){
      self$buffer_df
    },

    dt = function(){
      self$buffer_dt
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
          list_cols <- !vapply(vals, is_scalar_atomic, TRUE)
          list_cols[["msg"]] <- FALSE
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

#' Log to a memory buffer
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
    },


    format = function(
      ...
    ){
      res <- super$format()
      appenders <- appender_summary(self$appenders)
      if (!is.null(appenders)){
        res <-c(
          res, paste0("    ", appenders)
        )
      }

      res
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







# AppenderFileRotating ----------------------------------------------------

#' Log to a rotating file
#'
#' An extension of [AppenderFile] that rotates logfiles based on certain
#' conditions. Please refer to the documentation of [rotor::rotate()] for
#' the meanings of the extra arguments
#'
#' @eval r6_usage(list(
#'   AppenderFileRotating,
#'   AppenderFileRotatingDate,
#'   AppenderFileRotatingTime
#' ))
#'
#' @inheritSection AppenderFile Creating a New Appender
#' @inheritSection AppenderFile Fields
#' @inheritSection AppenderFile Methods
#'
#' @section Fields:
#'
#' \describe{
#'   \item{`age`, `size`, `max_backups`, `fmt`, `overwrite`, `compression`, `backup_dir`}{
#'     Please see [rotor::rotate()] for the meaning of these arguments
#'     (`fmt` is passed on as `format`).
#'   }
#'
#'   \item{`cache_backups`, `set_cache_backups(x)`}{
#'     `TRUE` or `FALSE`. If `TRUE` (the default) the list of backups is cached,
#'     if `FALSE` it is read from disk every time this appender triggers.
#'     Caching brings a significant speedup for checking whether to rotate or
#'     not based on the `age` of the last backup, but is only safe if
#'     there are no other programs/functions (except this appender) interacting
#'     with the backups.
#'   }
#'
#'   \item{`backups`}{A `data.frame` containing information on path, file size,
#'     etc... on the available backups of `file`.}
#'  }
#'
#'
#' @export
#' @seealso [LayoutFormat], [LayoutJson], [rotor::rotate()]
#' @family Appenders
#' @name AppenderFileRotating
#' @aliases AppenderFileRotatingDate AppenderFileRotatingTime
NULL




#' @export
AppenderFileRotating <- R6::R6Class(
  "AppenderFileRotating",
  inherit = AppenderFile,
  public = list(
    initialize = function(
      file,
      threshold = NA_integer_,
      layout = LayoutFormat$new(),
      filters = NULL,

      size = Inf,
      max_backups = Inf,
      compression = FALSE,
      backup_dir = dirname(file),
      create_file = TRUE
    ){
      assert_namespace("rotor")

      if (!file.exists(file))  file.create(file)

      private$bq <- rotor::BackupQueueIndex$new(
        file,
        backup_dir = backup_dir
      )

      self$set_file(file)
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)

      self$set_size(size)
      self$set_max_backups(max_backups)
      self$set_compression(compression)
      self$set_backup_dir(backup_dir)
      self$set_create_file(create_file)

      self
    },


    append = function(event){
      super$append(event)
      self$rotate()
    },


    rotate = function(
      force   = FALSE
    ){
      assert(is_scalar_bool(force))

      bq <- get("bq", private)

      if (force || bq$should_rotate(size = self$size)){
        bq$push_backup()
        bq$prune()
        file.remove(self$file)
        file.create(self$file)
      }

      self
    },


    prune = function(max_backups = self$max_backups){
      get("bq", envir = private)$prune(max_backups)
      self
    },


    set_file = function(
      file
    ){
      super$set_file(file)
      private$bq$set_file(self$file)
      self
    },


    set_size = function(
      x
    ){
      private[[".size"]] <- x
      self
    },


    set_max_backups = function(
      x
    ){
      private[["bq"]]$set_max_backups(x)
      self
    },


    set_compression = function(
      x
    ){
      private[["bq"]]$set_compression(x)
      self
    },


    set_create_file = function(
      x
    ){
      assert(is_scalar_bool(x))
      private[[".create_file"]] <- x
      self
    },


    set_backup_dir = function(
      x
    ){
      private$bq$set_backup_dir(x)
      self
    },


    format = function(
      color = false,
      ...
    ){
      res <- super$format(color = color, ...)

      if (!color)
        style_subtle <- identity

      cs <- style_subtle(
        sprintf("(current size: %s)", fmt_bytes(file.size(self$file)))
      )

      size <- {
        if (is.infinite(self$size))
          "Inf"
        else if (is.numeric(self$size))
          fmt_bytes(self$size)
        else
          self$size
      }

      size <- paste(size, cs)
      c(
        res,
        sprintf("  backups: %s/%s", private$bq$n_backups, self$max_backups),
        paste("  size:", size)
      )
    }
  ),


  active = list(
    size = function() get(".size", private),
    create_file = function() get(".create_file", private),

    compression = function() get("bq", private)$compression,
    max_backups = function() get("bq", private)$max_backups,
    backups     = function() get("bq", private)$backups,
    backup_dir  = function() get("bq", private)$backup_dir
  ),


  private = list(
    .size = NULL,
    .create_file = NULL,
    bq = NULL
  )
)


# AppenderFileRotatingTime ------------------------------------------------

#' @export
AppenderFileRotatingTime <- R6::R6Class(
  "AppenderFileRotating",
  inherit = AppenderFileRotating,
  public = list(
    initialize = function(
      file,
      threshold = NA_integer_,
      layout = LayoutFormat$new(),
      filters = NULL,

      age  = Inf,
      size = -1,
      max_backups = Inf,
      compression = FALSE,
      backup_dir = dirname(file),
      fmt = "%Y-%m-%d--%H-%M-%S",
      overwrite = FALSE,
      create_file = TRUE,
      cache_backups = TRUE
    ){
      assert_namespace("rotor")

      if (!file.exists(file))  file.create(file)

      private$bq <- rotor::BackupQueueDateTime$new(
        file,
        backup_dir = backup_dir
      )

      self$set_file(file)
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)

      self$set_fmt(fmt)
      self$set_age(age)
      self$set_size(size)
      self$set_max_backups(max_backups)
      self$set_compression(compression)
      self$set_overwrite(overwrite)
      self$set_backup_dir(backup_dir)
      self$set_create_file(create_file)
      self$set_cache_backups(cache_backups)

      self
    },


    rotate = function(
      force = FALSE,
      now   = Sys.time()
    ){
      assert(is_scalar_bool(force))
      bq <- get("bq", private)

      if (
        force ||
        bq$should_rotate(
          size = self$size,
          age = self$age,
          now = now
        )
      ){
        bq$push_backup(
          overwrite = self$overwrite,
          now = now
        )
        bq$prune()
        file.remove(self$file)
        file.create(self$file)
      }

      self
    },


    set_age = function(
      x
    ){
      private[[".age"]] <- x
      self
    },


    set_fmt = function(
      x
    ){
      get("bq", private)$set_fmt(x)
      self
    },


    set_overwrite = function(
      x
    ){
      assert(is_scalar_bool(x))
      private[[".overwrite"]] <- x
      self
    },


    set_cache_backups = function(
      x
    ){
      private$bq$set_cache_backups(x)
      self
    },

    format = function(
      color = FALSE,
      ...
    ){
      res <- super$format(color = color, ...)

      if (!color)
        style_subtle <- identity

      lr <- private$bq$last_rotation
      if (!is.null(lr))
        lr <- style_subtle(sprintf(" (last rotation: %s)", format(lr)))
      else
        lr <- ""

      c(
        res,
        sprintf("  age: %s%s", self$age, lr)
      )
    }
  ),


  active = list(
    age = function() get(".age", private),
    overwrite = function() get(".overwrite", private),
    fmt = function() get("bq", private)$fmt,
    cache_backups = function() get("bq", private)$cache_backups
  ),


  private = list(
    .age = NULL,
    .overwrite = NULL
  )
)



# AppenderFileRotatingDate ----------------------------------------------------

#' @export
AppenderFileRotatingDate <- R6::R6Class(
  "AppenderFileRotatingDate",
  inherit = AppenderFileRotatingTime,
  public = list(
    initialize = function(
      file,
      threshold = NA_integer_,
      layout = LayoutFormat$new(),
      filters = NULL,

      age  = Inf,
      size = -1,
      max_backups = Inf,
      compression = FALSE,
      backup_dir = dirname(file),
      fmt = "%Y-%m-%d",
      overwrite = FALSE,
      create_file = TRUE,
      cache_backups = TRUE
    ){
      assert_namespace("rotor")

      if (!file.exists(file))  file.create(file)

      private$bq <- rotor::BackupQueueDate$new(
        file,
        backup_dir = backup_dir
      )

      self$set_file(file)
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)

      self$set_fmt(fmt)
      self$set_age(age)
      self$set_size(size)
      self$set_max_backups(max_backups)
      self$set_compression(compression)
      self$set_overwrite(overwrite)
      self$set_create_file(create_file)
      self$set_backup_dir(backup_dir)
      self$set_cache_backups(cache_backups)

      self
    }
  )
)


# AppenderSyslog ----------------------------------------------------------

#' Log to the POSIX System Log
#'
#' An Appender that writes to the syslog on supported POSIX platforms. Requires
#' the \pkg{rsyslog} package.
#'
#' @eval r6_usage(AppenderSyslog)
#'
#' @inheritSection Appender Creating a New Appender
#' @inheritSection Appender Fields
#' @inheritSection Appender Methods
#'
#' @section Fields:
#'
#' \describe{
#'   \item{`identifier`}{`character` scalar. A string identifying the process;
#'     if `NULL` defaults to the logger name}
#'   \item{`syslog_levels`}{
#'  * a named `character` vector mapping whose names are log levels as
#'    understood by [rsyslog::syslog()] and whose values are
#'    [lgr log levels][log_levels] (either `character` or `numeric`)
#'  * a `function` that takes a vector of lgr log levels as input and returns a
#'    `character` vector of log levels for [rsyslog::syslog()].
#'  }
#'  }
#'
#' @export
#' @seealso [LayoutFormat], [LayoutJson]
#' @family Appenders
#' @name AppenderSyslog
#'
#' @examples
#' if (requireNamespace("rsyslog", quietly = TRUE)) {
#'   lg <- get_logger("rsyslog/test")
#'   lg$add_appender(AppenderSyslog$new(), "syslog")
#'   lg$info("A test message")
#'
#'   if (Sys.info()[["sysname"]] == "Linux"){
#'     system("journalctl -t 'rsyslog/test'")
#'   }
#'
#'   invisible(lg$config(NULL))  # cleanup
#' }
NULL


#' @export
AppenderSyslog <- R6::R6Class(
  "AppenderSyslog",
  inherit = Appender,
  cloneable = FALSE,
  public = list(
    initialize = function(
      identifier = NULL,
      threshold = NA_integer_,
      layout = LayoutFormat$new("%m"),
      filters = NULL,
      syslog_levels = c(
        "CRITICAL" = "fatal",
        "ERR" = "error",
        "WARNING" = "warn",
        "INFO" = "info",
        "DEBUG" = "debug",
        "DEBUG" = "trace"
      )
    ){
      if (!requireNamespace("rsyslog", quietly = TRUE)) {
        stop("The 'rsyslog' package is required for this appender.")
      }
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)
      self$set_identifier(identifier)
      self$set_syslog_levels(syslog_levels)
    },


    append = function(event){
      identifier <- get(".identifier", private)
      if (is.null(identifier)) identifier <- event$logger

      rsyslog::open_syslog(identifier = identifier)
      rsyslog::set_syslog_mask("DEBUG")
      on.exit(rsyslog::close_syslog())

      rsyslog::syslog(
        private$.layout$format_event(event),
        level = private$to_syslog_levels(event$level)
      )
    },


    set_syslog_levels = function(x){
      if (is.function(x)){
        private$.syslog_levels <- x
      } else {
        assert(all_are_distinct(unname(x)))
        assert(is_equal_length(x, names(x)))
        private$.syslog_levels <- structure(
          standardize_log_levels(unname(x)),
          names = names(x)
        )
      }

      self
    },


    set_identifier = function(x){
      assert(is.null(x) || is_scalar_character(x))
      private$.identifier <- x
      self
    }
  ),

  # +- active ---------------------------------------------------------------
  active = list(
    destination   = function() sprintf("syslog [%s]", private$.identifier),
    identifier    = function() get(".identifier", private),
    syslog_levels = function() get(".syslog_levels", private)
  ),

  private = list(
    finalize = function(){
      rsyslog::close_syslog()
    },

    to_syslog_levels = function(
      levels
    ){
      sl <- get(".syslog_levels", private)
      levels <- standardize_log_levels(levels)

      if (is.function(sl)){
        res <- sl(levels)
      } else {
        res <- names(private$.syslog_levels)[match(levels, unname(private$.syslog_levels))]
      }

      toupper(res)
    },

    .identifier = NULL,
    .syslog_levels = NULL
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
