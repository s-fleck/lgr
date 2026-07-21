# Appender ----------------------------------------------------------------

#' Appenders
#'
#' @description Appenders are attached to [Loggers] and manage the output of the
#' [LogEvents] to a destination - such as the console or a text file. An
#' Appender has a single [Layout] that tells it how to format the LogEvent. For
#' details please refer to the documentations of the specific Appenders.
#'
#' Additional Appenders that support a wide range of output destinations -
#' such as databases, email, push-notifications or Linux syslog - are available
#' from the package [lgrExtra](https://github.com/s-fleck/lgrExtra).
#'
#' @template abstract_class
#'
#' @aliases Appenders
#' @family Appenders
#' @include utils.R
#' @include utils-sfmisc.R
#' @include Filterable.R
#' @keywords internal
#' @export
Appender <- R6::R6Class(
  "Appender",
  inherit = Filterable,
  cloneable = FALSE,

  # +- public --------------------------------------------------------------
  public = list(

    #' @description Create a new Appender. Appenders are usually not
    #'   instantiated directly, but through one of their non-abstract
    #'   subclasses (such as [AppenderConsole] or [AppenderFile]).
    #' @param layout A [Layout] that determines how [LogEvents][LogEvent] are
    #'   formatted before being output by this Appender.
    #' @param threshold `character` or `integer` scalar. The minimum log level
    #'   that triggers this Appender. See [log_levels].
    initialize = function(
      layout = Layout$new(),
      threshold = NA_integer_
    ){
      self$set_layout(layout)
      self$set_threshold(threshold)
    },

    #' @description Process a [LogEvent] `event`. This method is usually not
    #' called by the user, but invoked by a [Logger]
    #' @param event a [LogEvent]
    append = function(event){
      private$.layout$format_event(event)
    },


    #' @description Set the minimum log level that triggers this Appender. See
    #' [threshold()] for examples
    #' @param level `character` or `integer` scalar log level. See [log_levels].
    set_threshold = function(level){
      level <- standardize_threshold(level)
      private$.threshold <- as.integer(level)
      invisible(self)
    },


    #' @description Set the `Layout` that this Appender will use for formatting
    #' `LogEvents`
    #' @param layout A [Layout] that determines how [LogEvents][LogEvent] are
    #'   formatted before being output by this Appender.
    set_layout = function(layout){
      if (is_scalar_list(layout)){
        # set_layout also accepts a list of length 1 containing a single Layout
        # object. This is a workaround for declarative config resolution (see
        # `instantiate_component()` in config.R). This behaviour is
        # intentionally not documented and you should not rely on it.
        layout <- layout[[1]]
      }
      assert(
        inherits(layout, "Layout"),
        "`layout` must a `Layout` object, but is ", string_repr(layout)
      )
      private$.layout <- layout
      invisible(self)
    },

    #' @description Format the Appender for printing (see also
    #'   [print.Appender()]).
    #' @param color `TRUE` or `FALSE`. Output with color? Requires the
    #'   package **crayon**.
    #' @param ... ignored
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
    #' @field threshold `integer` scalar. The minimum log level that
    #'   triggers this Appender. See `$set_threshold()`.
    threshold = function() private$.threshold,

    #' @field layout The [Layout] that this Appender uses to format
    #'   [LogEvents][LogEvent]. See `$set_layout()`.
    layout = function() private$.layout,

    #' @field destination
    #' The output destination of the `Appender` in human-readable form. This
    #' is mainly used when printing information about the Appender itself.
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
#' An Appender that outputs to the \R console. If you have the package
#' **crayon** installed log levels will be coloured by default
#' (but you can modify this behaviour by passing a custom [Layout]).
#'
#' @family Appenders
#' @seealso [LayoutFormat]
#' @export
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
AppenderConsole <- R6::R6Class(
  "AppenderConsole",
  inherit = Appender,
  cloneable = FALSE,
  public = list(
    #' @description Create a new AppenderConsole.
    #' @param filters A `list` of [EventFilters][EventFilter] or predicate
    #'   functions used to filter events for this Appender. See [Filterable].
    #' @param connection A connection or a `character` scalar. See the `file`
    #'   argument of [cat()] for more details. Defaults to [stdout()], except
    #'   inside \pkg{knitr} rendering processes where it defaults to
    #'   [stderr()].
    initialize = function(
      threshold = NA_integer_,
      layout = LayoutFormat$new(
        fmt = "%L [%t] %m %f",
        timestamp_fmt = "%H:%M:%OS3",
        colors = getOption("lgr.colors", list())
      ),
      filters = NULL,
      connection = NULL
    ){
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)
      if (is.null(connection)) {
        connection <- default_console_connection()
      }
      self$set_connection(connection)
    },

    #' @description Process a [LogEvent] `event` by writing it to
    #'   `$connection`. This method is usually not called by the user, but
    #'   invoked by a [Logger].
    #' @param event a [LogEvent]
    append = function(event){
      cat(private$.layout$format_event(event), sep = "\n", file = get(".connection", private))
    },

    #' @description Set the connection that this Appender writes to.
    #' @param connection A connection or a `character` scalar. See the `file`
    #'   argument of [cat()] for more details.
    set_connection = function(connection){
      assert(
        inherits(connection, "connection") || is_scalar_character(connection)
      )

      private[[".connection"]] <- connection
    }
  ),

  active = list(
    destination = function() "console",

    #' @field connection The connection that this Appender writes to. See
    #'   `$set_connection()`.
    connection = function() private[[".connection"]]
  ),

  private = list(
    .connection = NULL
  )
)

default_console_connection <- function() {
  # In knitr, default to using stderr where log messages can be better handled
  if (isTRUE(getOption("knitr.in.progress", FALSE))) {
    return(stderr())
  }

  # has to be this, not a call to `stdout()`
  ""
}


# AppenderFile ------------------------------------------------------------

#' Log to a file
#'
#' @description
#' A simple Appender that outputs to a file in the file system. If you plan
#' to log to text files, consider logging to JSON files and take a look at
#' [AppenderJson], which is a shortcut for `AppenderFile` preconfigured with
#' [`LayoutJson`].
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
AppenderFile <- R6::R6Class(
  "AppenderFile",
  inherit = Appender,
  cloneable = FALSE,
  public = list(
    #' @description Create a new AppenderFile.
    #' @param file `character` scalar. Path to the log file. If `file` does
    #'   not exist it will be created.
    #' @param threshold `character` or `integer` scalar. The minimum log level
    #'   that triggers this Appender. See [log_levels].
    #' @param layout A [Layout] that determines how [LogEvents][LogEvent] are
    #'   formatted before being output by this Appender.
    #' @param filters A `list` of [EventFilters][EventFilter] or predicate
    #'   functions used to filter events for this Appender. See [Filterable].
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

    #' @description Process a [LogEvent] `event` by appending it to `$file`.
    #'   This method is usually not called by the user, but invoked by a
    #'   [Logger].
    #' @param event a [LogEvent]
    append = function(event){
      cat(
        private$.layout$format_event(event),
        sep = "\n",
        file = get(".file", envir = private),
        append = TRUE
      )
    },

    #' @description Set a log file
    #' @param file `character` scalar. Path to the log file. If `file` does not
    #' exist it will be created.
    set_file = function(file){
      assert(
        is_scalar_character(file),
        "`file` must be character scalar, not: ", string_repr(file)
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


    #' @description Display the contents of the log file.
    #'
    #' @param threshold `character` or `integer` scalar. The minimum log level
    #'   that should be displayed.
    #' @param n `integer` scalar. Show only the last `n` log entries that match
    #'   `threshold`.
    show = function(
      threshold = NA_integer_,
      n = 20L
    ){
      assert(is_scalar_integerish(n))
      threshold <- standardize_threshold(threshold)
      reader <- self$layout$read %||% default_file_reader

      dd <- reader(self$file, threshold = threshold, n = n)

      cat(dd, sep = "\n")
      invisible(dd)
    }
  ),


  # +- active ---------------------------------------------------------------
  active = list(

    #' @field file `character` scalar. path to the log file
    file = function() private$.file,

    #' @field data `data.frame`. Contents of `file` parsed to a
    #' `data.frame` if used with a [Layout] that supports parsing of log
    #' file data (notably [LayoutJson]). Will throw an error if `Layout` does
    #' not support parsing.
    data = function(){
      parser <- self$layout$parse

      assert(
        !is.null(parser),
        CannotParseLogError(paste(
          "Neither", fmt_class(class(self)[[1]]), "nor", fmt_class(class(self$layout)[[1]]),
          "support parsing log files to data.frames. Consider using <LayoutJson>",
          "instead."
        ))
      )

      parser(self$file)
    },

    #' @field dt Like `$data`, but returns a `data.table` instead (requires
    #' the **data.table** package).
    dt = function(){
      data.table::as.data.table(self$data)
    },

    destination = function() self$file
  ),

  private = list(
    .file = NULL
  )
)




# AppenderJson ------------------------------------------------------------

#' Log to a JSON file
#'
#' @rdname AppenderFile
#'
#' @family Appenders
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
AppenderJson <- R6::R6Class(
  "AppenderJson",
  inherit = AppenderFile,
  cloneable = FALSE,
  public = list(
    #' @description Create a new AppenderJson.
    #' @param file `character` scalar. Path to the log file. If `file` does
    #'   not exist it will be created.
    #' @param threshold `character` or `integer` scalar. The minimum log level
    #'   that triggers this Appender. See [log_levels].
    #' @param layout A [Layout] that determines how [LogEvents][LogEvent] are
    #'   formatted before being output by this Appender. Defaults to
    #'   [LayoutJson].
    #' @param filters A `list` of [EventFilters][EventFilter] or predicate
    #'   functions used to filter events for this Appender. See [Filterable].
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
  )
)




# AppenderTable -----------------------------------------------------------
# nocov start

#' Abstract class for logging to tabular structures
#'
#' @template abstract_class
#'
#' @description AppenderTable is extended by Appenders that write to a data
#' source that can be interpreted as tables, (usually a `data.frame`). Examples
#' are `AppenderDbi`, `AppenderRjdbc` and `AppenderDt` from the
#' [lgrExtra](https://github.com/s-fleck/lgrExtra) package.
#'
#' @family Appenders
#' @export
AppenderTable <- R6::R6Class(
  "AppenderTable",
  inherit = Appender,
  cloneable = FALSE,

  public = list(
    #' @description `AppenderTable` is an abstract class and cannot be
    #'   instantiated directly; instantiate one of its subclasses (such as
    #'   `AppenderDbi` from [lgrExtra](https://github.com/s-fleck/lgrExtra))
    #'   instead.
    #' @param ... ignored
    initialize = function(...){
      stop(CannotInitializeAbstractClassError())
    },

    #' @description Show recent log entries
    #' @param n a positive `integer` scalar. Show at most that many entries
    #' @param threshold an `integer` or `character` [threshold][log_level].
    #'   Only show events with a log level at or below this threshold.
    show = function(threshold = NA_integer_, n = 20L) NULL,

    #' @description Format the Appender for printing (see also
    #'   [print.Appender()]).
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
    #' @field data `data.frame`. Contents of the table, parsed to a
    #' `data.frame`.
    data = function() as.data.frame(self$dt),

    #' @field dt Like `$data`, but returns a `data.table` instead (requires
    #' the **data.table** package).
    dt   = function() data.table::as.data.table(self$data)
  )
)

# nocov end




# AppenderMemory  --------------------------------------------------

#' Abstract class for logging to memory buffers
#'
#' @template abstract_class
#'
#' @description
#' AppenderMemory is extended by Appenders that retain an in-memory event
#' buffer, such as [AppenderBuffer] and `AppenderPushbullet` from the
#' [lgrExtra](https://github.com/s-fleck/lgrExtra) package.
#'
#' @export
#' @seealso [LayoutFormat]
AppenderMemory <- R6::R6Class(
  "AppenderMemory",
  inherit = Appender,
  cloneable = FALSE,
  public = list(
    #' @description `AppenderMemory` is an abstract class and cannot be
    #'   instantiated directly; instantiate one of its subclasses (such as
    #'   [AppenderBuffer]) instead.
    #' @param ... ignored
    initialize = function(...){
      stop(CannotInitializeAbstractClassError())
    },

    #' @description Process a [LogEvent] `event` by inserting it into the
    #'   in-memory buffer, flushing the buffer first if necessary (see
    #'   `$should_flush`, `$flush_threshold`, `$flush_on_rotate`). This
    #'   method is usually not called by the user, but invoked by a
    #'   [Logger].
    #' @param event a [LogEvent]
    append = function(event){

      i <- get("insert_pos", envir = private) + 1L
      assign("insert_pos", i, envir = private, inherits = TRUE)
      private[["last_event"]] <- private[["last_event"]] + 1L
      private[["event_order"]][[i]] <- private[["last_event"]]
      private[[".buffer_events"]][[i]] <- event

      # check if buffer should be flushed
      if ({
        # buffer is rotated
        i > get(".buffer_size", envir = private) &&
        get(".flush_on_rotate", envir = private)
      } || {
        # flush threshold is exceeded
        thr <- get(".flush_threshold", envir = private)
        !is.null(thr) && (is.na(thr) || all(event[["level"]] <= thr))
      } || {
        # should_flush() function triggers
        standardize_should_flush_output(
          get(".should_flush", envir = private)(event)
        )
      }){
        self$flush()
      } else if (i > get(".buffer_size", envir = private)){
        # check if flush should be triggered if buffer size is exceeded
        # sepparetly from other conditions. Must be below all other flush
        # conditions in case this causes the buffer to be cleared but not flushed.
        if (get(".flush_on_rotate", envir = private) ){
          self[["flush"]]()
        } else {
          # does not clear the buffer. this is intentional
          assign("insert_pos", 0L, envir = private)
        }
      }

    },


    #' @description  Sends the buffer's contents to all attached Appenders and
    #'   then clears the Buffer
    flush = function(){self},

    #' @description Clears the buffer, discarding all buffered Events
    clear = function(){self},

    #' @description Set the maximum size of the buffer
    #' @param x an `integer` scalar `>= 0`. Number of [LogEvents] to buffer.
    set_buffer_size = function(x){
      assert(is_n0(x))
      private$.buffer_size <- x
      invisible(self)
    },

    #' @description Set function that can trigger flushing the buffer
    #' @param x A `function` with the single argument `event`. Setting `x` to
    #'   `NULL` is a shortcut for `function(event) FALSE`. See active bindings.
    set_should_flush = function(x){
      if (is.null(x)) x <- function(event) FALSE
      assert_filter(x)
      private$.should_flush <- x
      invisible(self)
    },

    #' @description Should the buffer be flushed when the Appender is destroyed?
    #' @param x A `logical` scalar. See active bindings.
    set_flush_on_exit = function(x){
      assert(is_scalar_bool(x))
      private$.flush_on_exit <- x
      invisible(self)
    },

    #' @description Should the buffer be flushed if `buffer_size` is exceeded?
    #' @param x A `logical` scalar. See active bindings.
    set_flush_on_rotate = function(x){
      assert(is_scalar_bool(x))
      private$.flush_on_rotate <- x
      invisible(self)
    },

    #' @description Set threshold that triggers flushing
    #' @param level A `numeric` or `character` [threshold][log_level]. See
    #'  active bindings.
    set_flush_threshold = function(level){
      if (!is.null(level))
        level <- standardize_threshold(level)

      private$.flush_threshold <- level
      invisible(self)
    },


    #' @description Display the contents of the log table. Relies on the
    #' `$format_event` method of the [Layout] attached to this Appender.
    #'
    #' @param threshold `character` or `integer` scalar. The minimum log level
    #'   that should be displayed.
    #' @param n `integer` scalar. Show only the last `n` log entries that match
    #'   `threshold`.
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

    #' @description Format the Appender for printing (see also
    #'   [print.Appender()]).
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
    #' @field flush_on_exit A `logical` scalar. Should the buffer be flushed if
    #'   the Appender is destroyed (e.g. because the \R session is terminated)?
    flush_on_exit = function() {
      get(".flush_on_exit", private)
    },

    #' @field flush_on_rotate A `logical` scalar. Should the buffer be flushed when it is
    #'   rotated because `$buffer_size` is exceeded?
    flush_on_rotate = function() {
      get(".flush_on_rotate", private)
    },

    #' @field should_flush A `function` with exactly one arguments: `event`.
    #'   `$append()` calls this function internally on the current [LogEvent]
    #'   and flushes the buffer if it evaluates to `TRUE`.
    should_flush = function(){
      get(".should_flush", private)
    },

    #' @field buffer_size `integer` scalar `>= 0`. Maximum number of [LogEvents]
    #' to buffer.
    buffer_size = function() {
      get(".buffer_size", private)
    },

    #' @field flush_threshold A `numeric` or `character` threshold. [LogEvents]
    #'   with a [log_level] equal to or lower than this threshold trigger
    #'   flushing the buffer.
    flush_threshold = function(){
      get(".flush_threshold", private)
    },

    #' @field buffer_events A `list` of [LogEvents]. Contents of the buffer.
    buffer_events = function() {
      res <- get(".buffer_events", envir = private)

      if (length(res)){
        ord <- get("event_order", envir = private)
        ord <- ord - min(ord) + 1L
        ord <- order(ord)
        res <- res[ord]
      }

      as_event_list(
        res[!vapply(res, is.null, FALSE)]
      )
    },

    #' @field data `data.frame`. Alias for `$buffer_df`.
    data = function(){
      self$buffer_df
    },

    #' @field dt `data.table`. Alias for `$buffer_dt`.
    dt = function(){
      self$buffer_dt
    },

    #' @field buffer_df A `data.frame`. Contents of the buffer converted
    #'   to a `data.frame`.
    buffer_df = function() {
      as.data.frame(self[["buffer_dt"]])
    },

    #' @field buffer_dt A `data.table`. Contents of the buffer converted
    #'   to a `data.table` (requires the **data.table** package).
    buffer_dt = function(){
      assert_namespace("data.table")
      as.data.table.event_list(get("buffer_events", self))
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
#' @description
#' An Appender that Buffers LogEvents in-memory and and redirects them to other
#' Appenders once certain conditions are met.
#'
#' @export
#' @seealso [LayoutFormat]
#' @family Appenders
AppenderBuffer <- R6::R6Class(
  "AppenderBuffer",
  inherit = AppenderMemory,
  cloneable = FALSE,
  public = list(

    #' @description
    #' The [Layout] for this Appender is used only to format console output of
    #' its `$show()` method.
    #' @param appenders single [Appender] or a `list` thereof to dispatch the
    #'   buffered events to when the buffer is flushed. See `$set_appenders()`.
    #' @param buffer_size an `integer` scalar `>= 0`. The maximum number of
    #'   [LogEvents] to buffer. See `$set_buffer_size()`.
    #' @param flush_threshold A `numeric` or `character` [threshold][log_level]
    #'   that triggers flushing the buffer. See `$set_flush_threshold()`.
    #' @param flush_on_exit A `logical` scalar. Should the buffer be flushed
    #'   when the Appender is destroyed? See `$set_flush_on_exit()`.
    #' @param flush_on_rotate A `logical` scalar. Should the buffer be flushed
    #'   if `buffer_size` is exceeded? See `$set_flush_on_rotate()`.
    #' @param should_flush A `function` with the single argument `event` that
    #'   can trigger flushing the buffer, or `NULL`. See `$set_should_flush()`.
    #' @param filters A `list` of [EventFilters][EventFilter] or predicate
    #'   functions used to filter events for this Appender. See [Filterable].
    initialize = function(
      threshold = NA_integer_,
      layout = LayoutFormat$new(
        fmt = "%L [%t] %m",
        timestamp_fmt = "%H:%M:%S",
        colors = getOption("lgr.colors")
      ),
      appenders = NULL,
      buffer_size = 1e3,
      flush_threshold = NULL,
      flush_on_exit = TRUE,
      flush_on_rotate = TRUE,
      should_flush = NULL,
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


    #' @description  Sends the buffer's contents to all attached Appenders and
    #'   then clears the Buffer
    flush = function(){
      for (event in get("buffer_events", envir = self)){
        for (app in self$appenders) {
          if (app$filter(event))  app$append(event)
        }
      }

      self$clear()
    },

    #' @description Clears the buffer, discarding all buffered Events
    clear = function(){
      assign("insert_pos", 0L, envir = private)
      private$.buffer_events <- list()
      invisible(self)
    },


    #' @description Exactly like A [Logger], an [AppenderBuffer] can have an
    #' arbitrary amount of Appenders attached. When the buffer is flushed, the
    #' buffered events are dispatched to these Appenders.
    #'
    #' @param x single [Appender] or a `list` thereof. Appenders control the
    #'   output of a Logger. Be aware that a Logger also inherits the Appenders
    #'   of its ancestors (see `vignette("lgr", package = "lgr")` for more info
    #'   about Logger inheritance).
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


    #' @description Add an [Appender] to the AppenderBuffer. Supplying a
    #'   `name` is optional but recommended. After adding an Appender with
    #'   `appender$add_appender(AppenderConsole$new(), name = "console")` you
    #'   can refer to it via `appender$appenders$console`. `$remove_appender()`
    #'   can remove an Appender by position or name.
    #' @param appender a single [Appender]
    #' @param name a `character` scalar. Optional but recommended.
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


    #' @description remove an appender
    #' @param pos `integer` index or `character` name of the Appender(s) to
    #' remove
    #'
    remove_appender = function(
      pos
    ){
      if (is.numeric(pos)){
        assert(
          all(pos %in% seq_along(private$.appenders)),
          "'pos' is out of range of the length of appenders (1:",
          length(private$.appenders), ")"
        )

        pos <- sort(as.integer(pos), decreasing = TRUE)
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


    #' @description Format the Appender for printing (see also
    #'   [print.Appender()]).
    #' @param ... ignored
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
    #' @field appenders A `list` of [Appenders][Appender]. Like for a
    #'   [Logger], buffered events will be passed on to these Appenders once
    #'   a flush is triggered. See `$set_appenders()`.
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
#' @export
#' @seealso [AppenderFileRotatingDate], [AppenderFileRotatingTime], [rotor::rotate()]
#' @family Appenders
AppenderFileRotating <- R6::R6Class(
  "AppenderFileRotating",
  inherit = AppenderFile,
  cloneable = FALSE,
  public = list(

    #' @description Create a new AppenderFileRotating.
    #' @param size,max_backups,compression,backup_dir
    #'   see [rotor::rotate()] for the meaning of these arguments. Note that
    #'   `backup_dir` corresponds to `dir`.
    #' @param create_file Defunct, do not use.
    initialize = function(
      file,
      threshold = NA_integer_,
      layout = LayoutFormat$new(),
      filters = NULL,

      size = Inf,
      max_backups = Inf,
      compression = FALSE,
      backup_dir = dirname(file),
      create_file = NULL
    ){
      assert_namespace("rotor")

      if (!file.exists(file))  file.create(file)

      if (length(create_file)){
        .Deprecated(msg = "The create_file argument is defunct and will be removed from future versions of lgr.")
      }

      private$bq <- rotor::BackupQueueIndex$new(file)

      self$set_file(file)
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)

      self$set_size(size)
      self$set_max_backups(max_backups)
      self$set_compression(compression)
      self$set_backup_dir(backup_dir)
      self$set_create_file(TRUE)

      self
    },


    #' @description Process a [LogEvent] `event` by appending it to `$file`,
    #'   then rotate the file if necessary (see `$rotate()`). This method is
    #'   usually not called by the user, but invoked by a [Logger].
    #' @param event a [LogEvent]
    append = function(event){
      super$append(event)
      self$rotate()
    },


    #' @description Rotate the log file if it meets the rotation conditions
    #'   (see `$size`), or unconditionally if `force = TRUE`. See
    #'   [rotor::rotate()] for details.
    #' @param force `TRUE` or `FALSE`. Force rotation regardless of whether
    #'   the rotation conditions are met.
    rotate = function(
      force   = FALSE
    ){
      assert(is_scalar_bool(force))

      bq <- get("bq", private)

      if (force || bq$should_rotate(size = self$size)){
        bq$push()
        bq$prune()
        file.remove(self$file)
        file.create(self$file)
      }

      self
    },


    #' @description Remove old backups so that at most `max_backups` remain.
    #'   See [rotor::rotate()].
    #' @param max_backups `integer` scalar. Defaults to `$max_backups`.
    prune = function(max_backups = self$max_backups){
      get("bq", envir = private)$prune(max_backups)
      self
    },


    #' @description Set a log file
    #' @param file `character` scalar. Path to the log file. If `file` does
    #'   not exist it will be created.
    set_file = function(
      file
    ){
      super$set_file(file)
      private$bq$set_origin(self$file)
      self
    },


    #' @description Set the maximum size of the log file that triggers
    #'   rotation. See [rotor::rotate()].
    #' @param x A `numeric` file size in bytes, a `character` scalar such as
    #'   `"5mb"` (see [rotor::rotate()]), or `Inf` to disable size-based
    #'   rotation.
    set_size = function(
      x
    ){
      private[[".size"]] <- x
      self
    },


    #' @description Set the maximum number of backups to keep. See
    #'   [rotor::rotate()].
    #' @param x `integer` scalar `>= 0`, or `Inf` to keep all backups.
    set_max_backups = function(
      x
    ){
      private[["bq"]]$set_max_backups(x)
      self
    },


    #' @description Set whether backups should be compressed. See
    #'   [rotor::rotate()].
    #' @param x `TRUE` or `FALSE`.
    set_compression = function(
      x
    ){
      private[["bq"]]$set_compression(x)
      self
    },


    #' @description Defunct, do not use.
    #' @param x ignored
    set_create_file = function(
      x
    ){
      assert(is_scalar_bool(x))
      private[[".create_file"]] <- x
      self
    },


    #' @description Set the directory that backups are stored in. See
    #'   [rotor::rotate()].
    #' @param x `character` scalar. Path to the backup directory.
    set_backup_dir = function(
      x
    ){
      private$bq$set_dir(x, create = FALSE)
      self
    },


    #' @description Format the Appender for printing (see also
    #'   [print.Appender()]).
    format = function(
      color = FALSE,
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
    #' @field size The maximum size of the log file that triggers rotation.
    #'   See `$set_size()`.
    size = function() get(".size", private),

    #' @field create_file Defunct, do not use.
    create_file = function() get(".create_file", private),

    #' @field compression Whether backups are compressed. See
    #'   `$set_compression()`.
    compression = function() get("bq", private)$compression,

    #' @field max_backups The maximum number of backups to keep. See
    #'   `$set_max_backups()`.
    max_backups = function() get("bq", private)$max_backups,

    #' @field backups A `data.frame` containing information on path, file size,
    #'     etc... on the available backups of `file`.
    backups     = function() get("bq", private)$files,

    #' @field backup_dir The directory that backups are stored in. See
    #'   `$set_backup_dir()`.
    backup_dir  = function() get("bq", private)$dir
  ),


  private = list(
    .size = NULL,
    .create_file = NULL,
    bq = NULL
  )
)


# AppenderFileRotatingTime ------------------------------------------------

#' Log to a time-stamped rotating file
#'
#' An extension of [AppenderFile] that rotates logfiles based on certain
#' conditions. Please refer to the documentation of [rotor::rotate_time()] for
#' the meanings of the extra arguments
#'
#' @family Appenders
#' @seealso [AppenderFileRotatingDate], [AppenderFileRotating], [rotor::rotate()]
#' @export
AppenderFileRotatingTime <- R6::R6Class(
  "AppenderFileRotatingTime",
  cloneable = FALSE,
  inherit = AppenderFileRotating,
  public = list(

  #' @description Create a new AppenderFileRotatingTime.
  #' @param age,size,max_backups,compression,backup_dir,fmt,overwrite,cache_backups
  #'   see [rotor::rotate_time()] for the meaning of these arguments. Note
  #'   that `backup_dir` corresponds to `dir`.
  #' @param create_file Defunct, do not use.
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
      cache_backups = TRUE,
      create_file = NULL
    ){
      assert_namespace("rotor")

      if (length(create_file)){
        .Deprecated(msg = "The create_file argument is defunct and will be removed from future versions of lgr.")
      }

      if (!file.exists(file))  file.create(file)

      private$bq <- rotor::BackupQueueDateTime$new(file)

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
      self$set_create_file(TRUE)
      self$set_cache_backups(cache_backups)

      self
    },


    #' @description Rotate the log file if it meets the rotation conditions
    #'   (see `$size`, `$age`), or unconditionally if `force = TRUE`. See
    #'   [rotor::rotate_time()] for details.
    #' @param force `TRUE` or `FALSE`. Force rotation regardless of whether
    #'   the rotation conditions are met.
    #' @param now A `POSIXct` scalar. The timestamp to use for naming the new
    #'   backup and for evaluating `$age`.
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
        bq$push(
          overwrite = self$overwrite,
          now = now
        )
        bq$prune()
        file.remove(self$file)
        file.create(self$file)
      }

      self
    },


    #' @description Set the maximum age of the log file that triggers
    #'   rotation. See [rotor::rotate_time()].
    #' @param x A `difftime` or `numeric` scalar (interpreted as days), or
    #'   `Inf` to disable age-based rotation.
    set_age = function(
      x
    ){
      private[[".age"]] <- x
      self
    },


    #' @description Set the `strftime()`-style timestamp format used for
    #'   naming backups. See [rotor::rotate_time()].
    #' @param x `character` scalar.
    set_fmt = function(
      x
    ){
      get("bq", private)$set_fmt(x)
      self
    },


    #' @description Set whether an existing backup for the same timestamp
    #'   may be overwritten. See [rotor::rotate_time()].
    #' @param x `TRUE` or `FALSE`.
    set_overwrite = function(
      x
    ){
      assert(is_scalar_bool(x))
      private[[".overwrite"]] <- x
      self
    },


    #' @description set the `cache_backups` flag.
    #' @param x a `logical` scalar
    set_cache_backups = function(
      x
    ){
      private$bq$set_cache_backups(x)
      self
    },

    #' @description Format the Appender for printing (see also
    #'   [print.Appender()]).
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
    #' @field age The maximum age of the log file that triggers rotation.
    #'   See `$set_age()`.
    age = function() get(".age", private),

    #' @field overwrite Whether an existing backup for the same timestamp
    #'   may be overwritten. See `$set_overwrite()`.
    overwrite = function() get(".overwrite", private),

    #' @field fmt The `strftime()`-style timestamp format used for naming
    #'   backups. See `$set_fmt()`.
    fmt = function() get("bq", private)$fmt,

    #' @field cache_backups
    #'   `TRUE` or `FALSE`. If `TRUE` (the default) the list of backups is cached,
    #'   if `FALSE` it is read from disk every time this appender triggers.
    #'   Caching brings a significant speedup for checking whether to rotate or
    #'   not based on the `age` of the last backup, but is only safe if
    #'   there are no other programs/functions (except this appender) interacting
    #'   with the backups.
    cache_backups = function() get("bq", private)$cache_backups
  ),


  private = list(
    .age = NULL,
    .overwrite = NULL
  )
)



# AppenderFileRotatingDate ----------------------------------------------------

#' Log to a date-stamped rotating file
#'
#' This is a simpler version of AppenderFileRotatingTime when the timestamps
#' do not need to include sub-day accuracy.
#'
#' @family Appenders
#' @seealso [AppenderFileRotatingTime], [AppenderFileRotating], [rotor::rotate()]
#' @export
AppenderFileRotatingDate <- R6::R6Class(
  "AppenderFileRotatingDate",
  inherit = AppenderFileRotatingTime,
  cloneable = FALSE,
  public = list(


    #' @description Create a new AppenderFileRotatingDate.
    #' @param age,size,max_backups,compression,backup_dir,fmt,overwrite,cache_backups
    #'   see [rotor::rotate_date()] for the meaning of these arguments. Note
    #'   that `backup_dir` corresponds to `dir`.
    #' @param create_file Defunct, do not use.
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
      cache_backups = TRUE,
      create_file = NULL
    ){
      assert_namespace("rotor")

      if (!file.exists(file))  file.create(file)

      if (length(create_file)){
        .Deprecated(msg = "The create_file argument is defunct and will be removed from future versions of lgr.")
      }

      private$bq <- rotor::BackupQueueDate$new(file)

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
      self$set_create_file(TRUE)
      self$set_backup_dir(backup_dir)
      self$set_cache_backups(cache_backups)

      self
    }
  )
)




# print.Appender ----------------------------------------------------------

#' Print an Appender object
#'
#' The `print()` method for Loggers displays the most important aspects of
#' the Appender.
#'
#'
#' @inheritParams print.Logger
#'
#' @return
#'   `print()` returns `x` (invisibly), `format()` returns a `character` vector.
#' @export
#'
#' @examples
#' # print most important details of logger
#' print(lgr$console)
print.Appender <- function(
  x,
  color = requireNamespace("crayon", quietly = TRUE),
  ...
){
  assert(is_scalar_bool(color))
  cat(format(x, color = color), sep = "\n")
  invisible(x)
}




appender_summary <- function(x){
  dd <- lapply(x, srs_appender)

  if (!length(x)){
    return(NULL)
  }

  if (is.null(names(x))){
    names(x) <- ""
  }


  names(dd) <- ifelse(
    is.na(names(x)) | is_blank(names(x)),
    paste0("[[", seq_len(length(x)), "]]"),
    names(x)
  )
  dd <- do.call(rbind, dd)

  if (is.null(dd)) return(NULL)

  dd$name <- rownames(dd)
  dd$destination <- ifelse(
    !is_blank(dd$destination),
    paste("->", dd$destination),
    ""
  )

  with(
    dd,
    paste0(
      pad_right(name), ": ",
      pad_right(class), " [",
      pad_left(fmt_threshold(threshold, type = "character")), "] ",
      destination
    )
  )
}



# helpers -----------------------------------------------------------------

#' Default reader for files created with AppenderFile
#'
#' @param file a `character` scalar: path to a file
#' @param threshold only `NA` is supported for the default reader, other
#'   values cause a warning. See `LayoutFormat$read()` for an example where
#'   that supports thresholds.
#' @param n an `integer` scalar > 0
#'
#' @return a `character` vector of the last `n` matching log lines.
#' @noRd
default_file_reader <- function(file, threshold, n){
  assert(
    is_scalar_character(file),
    "`file` must be the path to a file, not ", string_repr(file)
  )

  if (isFALSE(is.na(threshold))){
    warning("A threshold was supplied to AppenderFile$show(), but the ",
    "Appenders' Layout does not support filtering by log level")
  }

  last_n(readLines(file), n)
}



standardize_should_flush_output <- function(
  x
){
  if (isTRUE(x)){
    return(TRUE)

  } else if (isFALSE(x)){
    return(FALSE)
  }

  warning(ValueIsNotBoolWarning(paste0(
    "`$should_flush()` did not return `TRUE` or `FALSE` but ",
    string_repr(x), ". ",
    "Please set a valid filter function with `$set_should_flush()`."
  )))

  FALSE
}



# conditions --------------------------------------------------------------

CannotParseLogError <- function(message){
  error(message = message, class = "CannotParseLogError")
}



ValueIsNotBoolWarning <- function(message){
  condition(message, class = c("ValueIsNotBoolWarning", "warning"))
}
