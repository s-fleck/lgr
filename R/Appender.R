#' Appenders
#'
#' Appenders are assigned to [Loggers] and manage the output of the [LogEvents]
#' to a destination, such as the console or a text file. An appender must have
#' a single [Layout] that tells it how to format the LogEvent. For details
#' please refer to the documentations of the specific Appenders.
#'
#' @eval r6_usage(Appender)
#'
#' @section Creating a New Appender:
#'
#' \describe{
#'   \item{`threshold`}{`character` or `integer` scalar. The minimum log level
#'     that triggers this logger. See [log_levels]}
#'   \item{`layout`}{a `Layout` that will be used for formatting the `LogEvents`
#'     passed to this Appender
#'    }
#'  }
#'
#' @section Fields and Methods:
#'
#' \describe{
#'   \item{`append(event)`}{Write the [LogEvent] `event` to a destination}
#'   \item{`threshold`, `set_threshold(level)`}{get/set the `Appender` threshold}
#'   \item{`layout`, `set_layout(layout)`}{get/set the `Layout`}
#'   \item{`destination`}{The output destination of the `Appender` in
#'     human-readable form (mainly for print output)}
#'  }
#'
#'
#' @name Appender
#' @aliases Appenders
#' @family Appenders
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
      self$set_layout(layout)
      self$set_threshold(threshold)
    },

    append = function(event){
      private$.layout$format_event(event)
    },

    set_threshold = function(level){
      assert_valid_threshold(level)

      if (is_scalar_character(level))
        level <- unlabel_levels(level)

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
    .filters = list(check_threshold),
    .threshold = NA,
    .layout = NULL
  )
)




# AppenderConsole ---------------------------------------------------------

#' AppenderConsole
#'
#' A simple Appender that outputs to the console. If you
#' have the package **crayon** installed log levels will be coloured by default
#' (but you can modify this behaviour by passing a custom [Layout])
#'
#' @eval r6_usage(AppenderConsole)
#'
#' @inheritSection Appender Creating a New Appender
#' @inheritSection Appender Fields and Methods
#'
#'
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
        colors = getOption("yog.colors", list())
      )
    ){
      self$set_threshold(threshold)
      self$set_layout(layout)
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
#' @eval r6_usage(AppenderFile)
#'
#' @inheritSection Appender Creating a New Appender
#' @inheritSection Appender Fields and Methods
#'
#' @section Creating a New Appender:
#'
#' \describe{
#'   \item{file}{`character` scalar. Path to the desired log file. If the file
#'     does not exist it will be created}
#'  }
#'
#' @section Fields and Methods:
#'
#' \describe{
#'   \item{`file, set_file(file)`}{get/set the log file}
#' }
#'
#' @export
#' @seealso [LayoutFormat], [LayoutJson]
#'
#' @examples
#' logger <- Logger$new("loggername")
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
      self$set_file(file)
      self$set_threshold(threshold)
      self$set_layout(layout)
    },

    append = function(event){
      cat(
        private$.layout$format_event(event),
        sep = "\n", file = private$.file, append = TRUE
      )
      return(invisible())
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




# AppenderTable -----------------------------------------------------------

#' AppenderTable
#'
#' `AppenderTable` is an internal class that is only exported for developers
#' that want to extend yog.
#'
#' @inheritSection Appender Creating a New Appender
#' @section Creating a New Appender:
#' @section Fields and Methods:
#'
#' \describe{
#'   \item{`show(n, threshold)`}{Show the last `n` log entries with a log level
#'   bellow `threshold`. The log entries will be formated for console output
#'   via the defined [Layout]}
#'   \item{`data`}{Get the log recorded by this `Appender` as a `data.table`
#'   with a maximum of `buffer_size` rows}
#' }
#'
#'
#' @seealso [LayoutFormat], [simple_logging], [data.table::data.table]
#' @family Appenders
#' @name AppenderTable
NULL


# exclude from coverage because AppenderTable is just a metaclass
# nocov start
#' @export
AppenderTable <- R6::R6Class(
  "AppenderTable",
  inherit = Appender,
  public = list(
    show = function(n = 20, threshold = NA_integer_) NULL
  ),

  active = list(
    destination = function() "",
    data = function() NULL
  ),

  private = list(
    .conn = NULL,
    .table = NULL
  )
)
# nocov end



# AppenderMemoryDt ----------------------------------------------------------

#' AppenderMemoryDt
#'
#' An Appender that outputs to an in-memory `data.table`. This requires that
#' you have the suggested package **data.table** installed. This kind of appender
#' is pretty useful for interactive use, and hast very little overhead.
#'
#' @eval r6_usage(AppenderMemoryDt)
#'
#' @inheritSection Appender Creating a New Appender
#' @section Creating a New Appender:
#'
#' \describe{
#'   \item{buffer_size}{`integer` scalar. Number of rows of the in-memory
#'   `data.table`}
#'   \item{prototype}{A prototype `data.table`. This is only necessary to set
#'   manually if you use custom [LogEvents] (which is not yet supported by yog,
#'   but planned for the future)}
#'  }
#'
#' @section Fields and Methods:
#'
#' \describe{
#'   \item{`show(n, threshold)`}{Show the last `n` log entries with a log level
#'   bellow `threshold`. The log entries will be formated for console output
#'   via the defined [Layout]}
#'   \item{`data`}{Get the log recorded by this `Appender` as a `data.table`
#'   with a maximum of `buffer_size` rows}
#' }
#'
#'
#' @export
#' @seealso [LayoutFormat], [simple_logging], [data.table::data.table]
#' @family Appenders
#' @aliases yog_data
#' @name AppenderMemoryDt
#'
#' @examples
#' lg <- Logger$new(
#'   "test",
#'   appenders = list(memory = AppenderMemoryDt$new()),
#'   threshold = NA,
#'   parent = NULL  # to prevent routing to root logger for this example
#' )
#' lg$debug("test")
#' lg$error("test")
#'
#' # Displaying the log
#' lg$appenders$memory$data
#' lg$appenders$memory$show()
#' show_log(target = lg$appenders$memory)
#'
#' # If you pass a Logger to show_log, it looks for the first MemoryAppender it
#' # can find.
#' show_log(target = lg)
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
      buffer_size = 1e5
    ){
      assert(is_scalar_integerish(buffer_size))
      assert(
        data.table::is.data.table(prototype) && is.integer(prototype$.id),
        "'prototype' must be a data.table with an integer column '.id'"
      )

      private$current_row <- 0L
      private$id <- 0L
      self$set_threshold(threshold)
      self$set_layout(layout)

      # initialize empty dt
      prototype <- data.table::copy(prototype)
      for (j in seq_along(prototype)){
        data.table::set(prototype, i = 1L, j = j, value = NA)
      }
      dd <- list(
        prototype,
        list(.id = rep(prototype[[1]], buffer_size - 1L))
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

      if (identical(nrow(dd),  0L)){
        cat("[empty log]")
        return(invisible(NULL))
      }

      res <- tail(dd[dd$level <= threshold, ], n)
      dd <- as.environment(res)
      assign("logger", self$logger, dd)
      cat(self$layout$format_event(dd), sep = "\n")
      invisible(res)
    }

  ),



  # +- active ---------------------------------------------------------------
  active = list(
    data = function(){
      tmp <- private$.data[!is.na(private$.data$.id), ]
      tmp[order(tmp$.id), ]
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




# AppenderDbi -------------------------------------------------------------


#' AppenderDbi
#'
#' Log to a database table with any **DBI** compatabile backend.
#'
#' @eval r6_usage(AppenderDbi)
#'
#' @inheritSection Appender Creating a New Appender
#' @section Creating a New Appender:
#'
#' \describe{
#'   \item{conn}{a DBI connection}
#'  }
#'
#' @inheritSection AppenderTable Fields and Methods
#' @section Fields and Methods:
#' \describe{
#'   \item{`close_on_exit`, `set_close_on_exit()`}{...}
#'   \item{`data`}{...}
#'   \item{`conn`}{get the DBI connection object}
#'   \item{`table`}{Name of the target database table}
#' }
#'
#' @export
#' @family Appenders
#' @name AppenderDbi
NULL




#' @export
AppenderDbi <- R6::R6Class(
  "AppenderDbi",
  inherit = AppenderTable,
  public = list(
    initialize = function(
      conn,
      table,
      threshold = NA_integer_,
      layout = select_dbi_layout(conn),
      close_on_exit = TRUE
    ){
      assert_namespace("DBI")
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_close_on_exit(close_on_exit)
      private$.conn  <- conn
      private$.table <- table

      table_exists <- DBI::dbExistsTable(self$conn, table)

      if (table_exists){
        message("Logging to existing table '", table, "'")
      } else if (is.null(self$layout$col_types)) {
        message("Creating '", table, "' on first log")
      } else {
        message("Creating '", table, "' with manual column types")
        DBI::dbExecute(conn, layout$sql_create_table(table))
      }
    },

    finalize = function() {
      if (isTRUE(self$close_on_exit))
        DBI::dbDisconnect(private$.conn)
    },

    set_close_on_exit = function(x){
      assert(is_scalar_bool(x))
      private$.close_on_exit <- x
      invisible(self)
    },

    show = function(n = 20, threshold = NA_integer_){
      dd <- data.table::copy(self$data)
      data.table::setattr(
        dd,
        "class",
        c("yog_data", "data.table", "data.frame")
      )

      if (is.na(threshold)) threshold <- Inf

      if (is.character(threshold))
        threshold <- unlabel_levels(threshold)

      if (identical(nrow(dd),  0L)){
        cat("[empty log]")
        return(invisible(NULL))
      }

      print(tail(dd[dd$level <= threshold, ], n))
    },


    append = function(event){
      dd <- private$.layout$format_event(event)
      DBI::dbWriteTable(
        private$.conn,
        private$.table,
        row.names = FALSE,
        dd,
        append = TRUE
      )
      return(invisible())
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

    data = function(){
      dd <- DBI::dbGetQuery(private$.conn, sprintf("SELECT * FROM %s", private$.table))
      names(dd) <- tolower(names(dd))
      dd[["timestamp"]] <- as.POSIXct(dd[["timestamp"]])
      dd[["level"]] <- as.integer(dd[["level"]])
      dd
    },

    table = function() private$.table
  ),

  private = list(
    .conn = NULL,
    .table = NULL,
    .close_on_exit = NULL
  )
)




# AppenderRjdbc -------------------------------------------------------------

#' AppenderRjdbc
#'
#' Log to a database table with the **RJDBC** package. **RJDBC** is only
#' somewhat  **DBI** compliant and does not work with [AppenderDbi]. I
#' personally do not recommend using **RJDBC** if it can be avoided.
#'
#' @eval r6_usage(AppenderRjdbc)
#'
#' @inheritSection Appender Creating a New Appender
#' @section Creating a New Appender:
#'
#' \describe{
#'   \item{conn}{an RJDBC connection}
#'  }
#'
#' @inheritSection AppenderDbi Fields and Methods
#' @section Fields and Methods:
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
  public = list(
    initialize = function(
      conn,
      table,
      threshold = NA_integer_,
      layout = select_dbi_layout(conn),
      close_on_exit = TRUE
    ){
      assert_namespace("RJDBC")
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_close_on_exit(close_on_exit)
      private$.conn  <- conn
      private$.table <- table

      table_exists <- tryCatch(
        is.data.frame(
          DBI::dbGetQuery(
            self$conn,
            sprintf("select 1 from %s where 1 = 2", table)
          )),
        error = function(e) FALSE
      )

      if (table_exists){
        message("Logging to existing table ", table)
      } else {
        message("Creating '", table, "' with manual column types")
        RJDBC::dbSendUpdate(conn, layout$sql_create_table(table))
      }
    },

    append = function(event){
      dd <- private$.layout$format_event(event)

      for (i in seq_len(nrow(dd))){
        data <- as.list(dd[i, ])

        q <-  sprintf(
          "INSERT INTO %s (%s) VALUES (%s)",
          private$.table,
          paste(self$layout$col_names, collapse = ", "),
          paste(rep("?", length(data)), collapse = ", ")
        )

        RJDBC::dbSendUpdate(get(".conn", private), q, list=data)
      }

      return(invisible())
    }
  ),

  private = list(
    .conn = NULL,
    .table = NULL
  )
)
# nocov end



# AppenderBuffer --------------------------------------------------

#' AppenderBuffer
#'
#' An Appender that Buffers LogEvents in-memory and and redirects them to other
#' appenders once certain conditions are met.
#'
#' @eval r6_usage(AppenderBuffer)
#'
#' @inheritSection Appender Creating a New Appender
#' @section Creating a New Appender:
#'
#' \describe{
#'   \item{`buffer_size`}{`integer` scalar. Number of [LogEvents] to buffer}
#'   \item{`should_flush`}{A `function` with a single argument `event`
#'     (a [LogEvent]) that must only return either `TRUE` or `FALSE`. If the
#'     function returns `TRUE`, flushing of the buffer is triggered. Defaults
#'     to flushing if a `FATAL` event is registered
#'    }
#'   \item{`flush_on_exit`}{`TRUE` or `FALSE`: Whether the buffer should be
#'     flushed when the Appender is garbage collected (f.e when you close R)}
#'   \item{`flush_on_rotate`}{`TRUE` or `FALSE`: Whether the buffer should be
#'     flushed when the Buffer is full (f.e when you close R). Setting this
#'     to off can have slighly negative perfomance impacts.}
#'   \item{`appenders`}{Like for a [Logger]. Buffered events will be passed on
#'     to these Appenders once a flush is triggered}
#'  }
#'
#' @inheritSection Appender Fields and Methods
#' @section Fields and Methods:
#'
#' \describe{
#'   \item{`flush()`}{Manually trigger flushing}
#'   \item{`add_appender(appender)`}{Add and Appender to this Appender.
#'     see [Logger]}
#'   \item{`remove_appender(pos)`}{Remove and Appender from this Appender.
#'     see [Logger]}
#'   \item{`appenders`, `set_appenders()`}{Get/set the `list` of `Appenders`
#'     attached to this `Appender`}
#'   \item{`buffer_size, set_buffer_size(x)`}{get/set the bufffer size}
#'   \item{`flush_on_exit, set_flush_on_exit(x)`}{get/set whether the buffer
#'    should be flushed when this `Appender` is garbage collected}
#'   \item{`flush_on_rotate, set_flush_on_rotate`}{get/set whether the buffer
#'     should be flushed once it is full}
#'   \item{`should_flush(event)`, `set_should_flush(x)`}{Call or set the
#'     function that determins whether an event should trigger flushing based
#'     on its properties (e.g. its log level)
#'   }
#' }
#'
#' @export
#' @seealso [LayoutFormat]
#' @family Appenders
#' @name AppenderBuffer
NULL




#' @export
AppenderBuffer <- R6::R6Class(
  "AppenderBuffer",
  inherit = Appender,
  public = list(
    initialize = function(
      threshold = NA_integer_,
      appenders = NULL,
      should_flush = function(event) isTRUE(event[["level"]] <= 100),
      flush_on_exit = TRUE,
      flush_on_rotate = TRUE,
      layout = LayoutFormat$new(
        fmt = "%L [%t] %m",
        timestamp_fmt = "%H:%M:%S",
        colors = getOption("yog.colors")
      ),
      buffer_size = 1e3
    ){
      stopifnot(
        is_scalar_integerish(buffer_size),
        is_scalar_bool(flush_on_exit),
        is_scalar_bool(flush_on_rotate)
      )

      self$set_threshold(threshold)
      self$set_should_flush(should_flush)
      self$set_appenders(appenders)
      self$set_buffer_size(buffer_size)
      self$set_flush_on_exit(flush_on_exit)
      self$set_flush_on_rotate(flush_on_rotate)

      # no speed advantage in pre allocating lists in R!
      private$.buffered_events <- list()

      invisible(self)
    },


    append = function(
      x
    ){
      len <- length(private$.buffered_events)
      private$.buffered_events[[len + 1L]] <- x$clone()
      if (private$.should_flush(x)){
        self$flush()
        return(invisible())
      } else {
        if (len >= self$buffer_size){
          if (self$flush_on_rotate){
            self$flush()
          } else {
            len <- length(private$.buffered_events)
            private$.buffered_events <-
              private$.buffered_events[seq.int(len + 1L - self$buffer_size, len)]
          }
        }
      }
      invisible(NULL)
    },

    flush = function(
      x = private$.buffered_events
    ){
      for (event in private$.buffered_events){
        for (app in self$appenders) {
          if (app$filter(event))  app$append(event)
        }
      }
      private$.buffered_events <- list()
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
      for (i in seq_along(x))  self$add_appender(x[[i]], name = names(x)[[i]])

      invisible(self)
    },


    add_appender = function(
      appender,
      name = NULL
    ){
      assert(inherits(appender, "Appender"))

      for(app in self$appenders){
        # appender is already attached to logger, nothing to do, this is
        # necessary because `logger$appender$blubb <- blagh` tries to readd the
        # whole appender list
        if (identical(app, appender)) return(invisible(self))
      }

      assert(
        is.null(appender$logger) || identical(appender$logger, self),
        "Cannot add appender to logger as it is already attached to another",
        "logger. Please create a new appender or clone the old one with",
        "`appender$clone()`, but be aware that this can cause weird bugs",
        "for some appender types."
      )

      private$.appenders[length(private$.appenders) + 1L] <- list(appender)

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

    finalize = function(){
      if (self$flush_on_exit) self$flush()
    },

    set_buffer_size = function(x){
      assert(is_scalar_integerish(x))
      private$.buffer_size <- x
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

    set_should_flush = function(x){
      assert(is.function(x))
      private$.should_flush <- x
      invisible(self)
    }
  ),



  # +- active ---------------------------------------------------------------
  active = list(
    flush_on_exit = function() private$.flush_on_exit,

    flush_on_rotate = function() private$.flush_on_rotate,

    buffer_size = function() private$.buffer_size,

    buffered_events = function() private$.buffered_events,

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

    destination = function() paste(length(self$appenders), "child Appenders")
  ),

  private = list(
    .appenders = list(),
    .flush_on_exit = NULL,
    .flush_on_rotate = NULL,
    .should_flush = NULL,
    .buffer_size = NULL,
    .buffered_events = NULL
  )
)




# utils -------------------------------------------------------------------

#TODO: function needs renaming and doc
trim_last_event <- function(x, max_len){
  if (length(x) == 1L)
    x
  else
    x[seq.int(length(x) - max_len + 1L, length(x))]
}
