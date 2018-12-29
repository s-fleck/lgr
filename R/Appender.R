#' Abstract Class for Appenders
#'
#' Abstract classes are exported for developers that want to extend them, they
#' are not useful to casual users. Appenders are assigned to [Loggers] and
#' manage the output of the [LogEvents] to a destination, such as the console or
#' a text file. An Appender must have a single [Layout] that tells it how to
#' format the LogEvent. For details please refer to the documentations of the
#' specific Appenders.
#'
#' @eval r6_usage(Appender)
#'
#' @section Creating a New Appender:
#'
#' New Appenders are instantiated with `Appender$new()`. For the arguments to
#' `new()` please refer to the section *Fields*. You can also modify those
#' fields after the Appender has been created with setters in the form of
#' `appender$set_<fieldname>(value)`
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
#'     \item{`append(event)`}{Write the [LogEvent] `event` to a destination}
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
    .filters = NULL,
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
#'   layout = LayoutFormat$new("[%t] %c(): [%n] %m from user %u", colors = getOption("lgr.colors"))))
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
      )
    ){
      self$set_threshold(threshold)
      self$set_layout(layout)
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
#' @inheritSection AppenderFile Methods
#'
#' @section Fields:
#' \describe{
#'   \item{`data`}{The log recorded by this `Appender` as a `data.frame`. This
#'   might become slow if the log files grow to large.}
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{`show(n, threshold)`}{Show the last `n` log entries with a log level
#'   bellow `threshold`. The log entries will be formated as in the source
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
      layout = LayoutJson$new()
    ){
      self$set_file(file)
      self$set_threshold(threshold)
      self$set_layout(layout)
    },

    show = function(n = 20, threshold = NA){
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
    }
  )

)




# AppenderTable -----------------------------------------------------------

#' Abstract Class for Logging to Tabular Structures
#'
#' Abstract classes are exported for developers that want to extend them, they
#' are not useful to casual users. [AppenderDbi], [AppenderRjdbc] and
#' [AppenderDt] are derived from AppenderTabel.
#'
#' @inheritSection Appender Creating a New Appender
#' @section Creating a New Appender:
#'
#' @inheritSection Appender Fields
#' @inheritSection Appender Methods
#'
#' @section Fields:
#'
#' \describe{
#'   \item{`show(n, threshold)`}{Show the last `n` log entries with a
#'   log level bellow `threshold`.}
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{`data`}{Get the log recorded by this `Appender` as a
#'   `data.table`}
#' }
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
  cloneable = FALSE,
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



# AppenderDt ----------------------------------------------------------

#' Log to an In-Memory Data.Table
#'
#' An Appender that outputs to an in-memory `data.table`. This requires that
#' you have the suggested package **data.table** installed. This kind of
#' Appender is useful for interactive use, and has very little overhead.
#'
#' @section Custom Fields:
#'
#' `AppenderDt` supports [custom fields][LogEvent], but they have to be
#' pre-allocated in the `prototype` argument. Custom fields that are not
#' part of the prototype are discarded.
#'
#' With the default settings, the custom field `value` is included in the
#' `data.table` as a list column to store arbitrary \R objects (see example).
#' It is recommended to use this feature only `TRACE` level.
#'
#' @eval r6_usage(AppenderDt)
#'
#' @inheritSection Appender Fields
#' @inheritSection Appender Methods
#'
#'
#' @section Creating a Data Table Appender:
#'
#' \describe{
#'   \item{buffer_size}{`integer` scalar. Number of rows of the in-memory
#'   `data.table`}
#'   \item{prototype}{A prototype `data.table`. This is only necessary to set
#'   manually if you use custom [LogEvents]. The default prototype already
#'   contains a list column called `value` that can hold arbitrary R values
#'   (see examples)}
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
#' @aliases lgr_data
#' @name AppenderDt
#'
#' @examples
#' lg <- Logger$new(
#'   "test",
#'   appenders = list(memory = AppenderDt$new()),
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
#'
#' # Data tables can store arbitrary R values in list columns. The default
#' # AppenderDt comes with a single list column called "value" that you can use
#' lg$info("the iris data frame", value = iris)
#' lg$appenders$memory$data
#' head(lg$appenders$memory$data$value[[3]])
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
        fmt = "%L [%t] %m",
        timestamp_fmt = "%H:%M:%S",
        colors = getOption("lgr.colors")
      ),
      prototype = data.table::data.table(
        .id  = NA_integer_,
        level = NA_integer_,
        timestamp = Sys.time(),
        caller = NA_character_,
        msg = NA_character_,
        value = list(list())
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
      # Select and prepare event values to be inserted into data
        vals <- event[["values"]]
        valnames <- setdiff(names(get(".data", private)), ".id")
        vals <- vals[valnames]
        names(vals) <-  valnames

        # handle list-columns
        vals[vapply(vals, is.null, FALSE)] <- NA
        list_cols <- get("list_cols", private)
        vals[list_cols] <- lapply(vals[list_cols], list)

      # Prepare values for vectorized insert (if necessary)
        lengths <- vapply(vals, length, 1L, USE.NAMES = FALSE)
        lenmax  <- max(lengths)
        assert(all(lengths %in% c(1, lenmax)))

        # take special care if vectorized insert is bigger than buffer size
        if (lenmax > nrow(private$.data)){
          vals <- lapply(vals, trim_last_event, nrow(private$.data))
          private[["id"]] <- private[["id"]] + lenmax - nrow(private$.data)
          lenmax <- nrow(private$.data)
        }
        i   <- seq_len(lenmax)

      # generate new ids
        ids <- i + private[["id"]]

      # check if rotation is necessary
        if (private[["current_row"]] + lenmax <= nrow(private$.data)){
          i   <- i + private[["current_row"]]
          private[["current_row"]] <- private[["current_row"]] + lenmax
        } else {
          # rotate buffer
          private[["current_row"]] <- lenmax
        }

      # Perform the insert
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
    .data = NULL,
    list_cols = NULL
  )
)




# AppenderDbi -------------------------------------------------------------


#' Log to Databases via DBI
#'
#' Log to a database table with any **DBI** compatabile backend. AppenderDbi
#' does *not* support case sensitive / quoted column names.
#'
#' @eval r6_usage(AppenderDbi)
#'
#' @inheritSection Appender Creating a New Appender
#' @section Creating a New AppenderDbi:
#'
#' \describe{
#'   \item{`conn`}{a [DBI connection][DBI::dbConnect]}
#'  }
#'
#' @inheritSection AppenderTable Fields
#' @inheritSection AppenderTable Methods
#' @section Fields:
#' \describe{
#'   \item{`close_on_exit`, `set_close_on_exit()`}{`TRUE` or `FALSE`. Close the
#'   Database connection when the Logger is removed?}
#'   \item{`data`}{Querry the whole log from the Database and return it as a
#'   `data.frame`}
#'   \item{`conn`}{get the DBI connection object}
#'   \item{`table`}{Name of the target database table}
#' }
#'
#' @section DBI Layouts:
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
#' with the `col_types` from `layout`; however, a more flexible approach is
#' to create the table manually first using an `SQL CREATE TABLE` statement.
#'
#' @export
#' @family Appenders
#' @name AppenderDbi
NULL




#' @export
AppenderDbi <- R6::R6Class(
  "AppenderDbi",
  inherit = AppenderTable,
  cloneable = FALSE,
  public = list(
    initialize = function(
      conn,
      table,
      threshold = NA_integer_,
      layout = select_dbi_layout(conn, table),
      close_on_exit = TRUE
    ){
      assert_namespace("DBI")
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_close_on_exit(close_on_exit)

      private$.conn  <- conn
      private$.table <- table

      if (DBI::dbExistsTable(self$conn, table)){
        # do nothing
      } else if (is.null(self$layout$col_types)) {
        msg <- paste0("Creating '", table, "' on first log. ")
        if (!setequal(layout$event_values, DEFAULT_FIELDS)){
          message(
            msg,
            "The Layout contains custom fields, but no `col_types`. ",
            "The column types will be determined automaticaly",
            "when the first event is beeing logged to the database table."
          )
        } else {
          message(msg)
        }
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
        c("lgr_data", "data.table", "data.frame")
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
      NULL
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

#' Log to Databases via RJDBC
#'
#' Log to a database table with the **RJDBC** package. **RJDBC** is only
#' somewhat  **DBI** compliant and does not work with [AppenderDbi]. I
#' personally do not recommend using **RJDBC** if it can be avoided. As
#' opposed to `AppenderDbi` you always need to specify the column types if you
#' are logging to a non-existant table.
#'
#' @inheritSection Appender Creating a New Appender
#' @inheritSection AppenderDbi DBI Layouts
#' @inheritSection AppenderDbi Fields
#' @inheritSection AppenderDbi Methods
#'
#' @eval r6_usage(AppenderRjdbc)
#'
#' @section Creating a New Appender:
#'
#' \describe{
#'   \item{conn}{an RJDBC connection}
#'  }
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
        # do nothing
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

      NULL
    }
  ),

  private = list(
    .conn = NULL,
    .table = NULL
  )
)

# nocov end



# AppenderBuffer --------------------------------------------------

#' Log to a Memory Buffer
#'
#' An Appender that Buffers LogEvents in-memory and and redirects them to other
#' appenders once certain conditions are met.
#'
#' @eval r6_usage(AppenderBuffer)
#'
#' @inheritSection Appender Creating a New Appender
#' @inheritSection Appender Fields
#' @inheritSection Appender Methods
#'
#' @section Fields:
#'
#' \describe{
#'   \item{`flush()`}{Manually trigger flushing}
#'   \item{`add_appender(appender)`}{Add and Appender to this Appender.
#'     see [Logger]}
#'   \item{`remove_appender(pos)`}{Remove and Appender from this Appender.
#'     see [Logger]}
#'   \item{`appenders`, `set_appenders()`}{Like for a [Logger]. Buffered events will be passed on
#'     to these Appenders once a flush is triggered}
#'   \item{`buffer_size, set_buffer_size(x)`}{`integer` scalar. Number of [LogEvents] to buffer}
#'   \item{`flush_on_exit, set_flush_on_exit(x)`}{`TRUE` or `FALSE`: Whether the
#'     buffer should be flushed when the Appender is garbage collected (f.e when
#'     you close \R)}
#'   \item{`flush_on_rotate, set_flush_on_rotate`}{`TRUE` or `FALSE`: Whether
#'     the buffer should be flushed when the Buffer is full (f.e when you close
#'     \R). Setting this to off can have slighly negative perfomance impacts.}
#'   \item{`should_flush(event)`, `set_should_flush(x)`}{ A `function` with a
#'     single argument `event` (a [LogEvent]) that must only return either `TRUE`
#'     or `FALSE`. If the function returns `TRUE`, flushing of the buffer is
#'     triggered. Defaults to flushing if a `FATAL` event is registered }
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
  cloneable = FALSE,
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
        colors = getOption("lgr.colors")
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
      NULL
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
