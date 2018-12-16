#' Layouts
#'
#' Layouts get a [LogEvent] passed down from an [Appender], and format them
#' for output. How this formatting works exactly varries widely. For example
#' for file or console output the log event is usually formatted into a single
#' character line.
#'
#'
#' @section Fields and Methods:
#'
#' \describe{
#'   \item{`format_event(event)`}{format a [LogEvent]}
#' }
#'
#'
#'
#' @name Layout
#' @aliases Layouts
#' @family Layouts
#' @include print_LogEvent.R
#' @include utils.R
#' @include utils-sfmisc.R
#' @include Filterable.R
NULL




# Layout ------------------------------------------------------------------

#' @export
Layout <- R6::R6Class(
  "Layout",

  public = list(
    format_event = function(event) paste(capture.output(print(event$values)), collapse = " ")
  )
)




# LayoutFormat ------------------------------------------------------------

#' LayoutFormat
#'
#' Format a [LogEvent] as human readable text using [format.LogEvent()]
#'
#' @eval r6_usage(LayoutFormat)
#'
#' @section Creating a New LayoutFormat:
#'
#' LayoutFormat passes it fields as arguments to [format.LogEvent()].
#'
#' \describe{
#'   \item{`fmt`}{see [format.LogEvent()]}
#'   \item{`timestamp_fmt`}{see [base::format.POSIXct()]}
#'   \item{`colors`}{see [format.LogEvent()]}
#'   \item{`pad_levels`}{see [format.LogEvent()]}
#'  }
#'
#'
#' @inheritSection Layout Fields and Methods
#'
#'
#' @name LayoutFormat
#' @family Layouts
#' @include Filterable.R
#' @include log_levels.R
#' @examples
#'
#' # setup a dummy LogEvent
#' event <- LogEvent$new(
#'   logger = Logger$new("dummy logger", user = "testuser"),
#'   level = 200,
#'   timestamp = Sys.time(),
#'   caller = NA_character_,
#'   msg = "a test message"
#' )
#' lo <- LayoutFormat$new()
#' lo$format_event(event)
#'
NULL




#' @export
LayoutFormat <- R6::R6Class(
  "LayoutFormat",
  inherit = Layout,
  public = list(
    initialize = function(
      fmt = "%L [%t] %m",
      timestamp_fmt = "%Y-%m-%d %H:%M:%OS3",
      colors = NULL,
      pad_levels = "right"
    ){
      self$set_fmt(fmt)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_colors(colors)
      self$set_pad_levels(pad_levels)
    },

    format_event = function(
      event
    ){
      format.LogEvent(
        event,
        fmt = private$.fmt,
        timestamp_fmt = private$.timestamp_fmt,
        colors = private$.colors,
        pad_levels = private$.pad_levels
      )
    },

    set_fmt = function(x){
      assert(is_scalar_character(x))
      private$.fmt <- x
      invisible(self)
    },

    set_timestamp_fmt = function(x){
      assert(is_scalar_character(x))
      private$.timestamp_fmt <- x
      invisible(self)
    },

    set_colors = function(x){
      assert(
        is.null(x) || is.list(x),
        "'colors' must either be NULL or a list of functions, not ",
        class_fmt(x)
      )
      private$.colors <- x
      invisible(self)
    },

    set_pad_levels = function(x){
      assert(is_scalar_character(x))
      private$.pad_levels <- x
      invisible(self)
    }
  ),


  active = list(
    fmt = function()  private$.fmt,

    timestamp_fmt = function() private$.timestamp_fmt,

    colors = function() private$.colors,

    pad_levels = function() private$.pad_levels
  ),

  private = list(
    .fmt = NULL,
    .timestamp_fmt = NULL,
    .colors = NULL,
    .pad_levels = NULL
  )
)




# LayoutTable -------------------------------------------------------------

#' LayoutTable
#'
#' `LayoutTable` is an internal class that is only exported for developers that
#' want to extend yog. You are probably looking for either [LayoutDbi] or
#' [LayoutJson].
#'
#' @section Creating a New Layout:
#'
#' \describe{
#'   \item{`event_vals`}{Names of the fields of the [LogEvent]
#'     to include in the output object.
#'   }
#'
#'   \item{`logger_vals`}{Names of the fields of the [Logger] that produced the
#'     [LogEvent] to include in the output object.
#'   }
#'
#'   \item{`other_vals`}{A named `list` of any kind of \R values. Functions in
#'     this `list`` will be executed with no arguments and their results will
#'     be included in the results object (see examples)
#'   }
#' }
#'
#'
#' @inheritSection Layout Fields and Methods
#' @family Layouts
#'
#' @name LayoutTable
NULL




#' @export
LayoutTable <- R6::R6Class(
  "LayoutTable",
  inherit = Layout,
  public = list(
    format_event = function(event) {NULL},

    set_other_vals = function(x){
      assert(is.list(x) || is.null(x))
      assert(identical(length(names(x)), length(x)))
      private$.other_vals <- x
      invisible(self)
    },

    set_event_vals = function(x){
      private$.event_vals <- x
      invisible(self)
    },

    set_logger_vals = function(x){
      private$.logger_vals <- x
      invisible(self)
    }
  ),

  active = list(
    other_vals  = function() private$.other_vals,
    event_vals  = function() private$.event_vals,
    logger_vals = function() private$.logger_vals
  ),

  private = list(
    .other_vals = NULL,
    .event_vals = NULL,
    .logger_vals = NULL
  )
)




# LayoutDbi ---------------------------------------------------------------


#' LayoutDbi
#'
#' Format a [LogEvent] as data.frame for inserting into a Database.
#'
#' @eval r6_usage(LayoutDbi)
#'
#' @inheritSection LayoutTable Creating a New Layout
#'
#' @section Creating a New Layout:
#' \describe{
#'   \item{`col_types`}{a named `character` vector of column types supported by
#'     the target database. Must include all columns described in `event_vals`,
#'     `logger_vals` and `other_vals`. If a new database table is created by
#'     LayoutDbi its column order will correspond to the order of `col_types`.
#'   }
#' }
#'
#' @inheritSection LayoutTable Fields and Methods
#' @section Fields and Methods:
#'
#' \describe{
#'   \item{`col_types`, `set_col_types()`}{Get/set the col_types of this `Layout`}
#'   \item{`col_names`}{Convenience method to get the names of the `col_types`
#'     vector}
#' }
#'
#'
#' @section Database Specific Layouts:
#'
#' Different databases have different data types and features. Currently the
#' following `LayoutDBI` subclasses exist that deal with specific databases,
#' but this list is expected to grow as yog matures:
#'
#'   * `LayoutSQLite`: Needs its own Layout because SQLite does not support
#'     `timestamps`
#'   * `LayoutDBI`: For all other datbases
#'
#' The utility function `select_dbi_layout()` returns the appropriate
#' Layout for a DBI connection.
#'
#'
#' @name LayoutDbi
#' @aliases LayoutSqlite select_dbi_layout
#' @family Layouts
#' @family database layouts
#' @include Filterable.R
#' @include log_levels.R
#' @seealso [DBI::DBI]
#' @examples
#' # setup a dummy LogEvent
#' event <- LogEvent$new(
#'   logger = Logger$new("dummy logger", user = "testuser"),
#'   level = 200,
#'   timestamp = Sys.time(),
#'   caller = NA_character_,
#'   msg = "a test message"
#' )
#'
#' # defaults
#' lo <- LayoutDbi$new()
#' lo$format_event(event)
#'
#' # SQLite does not support timestamps so LayoutSqlite converts them to text
#' lo <- LayoutSqlite$new()
#' str(lo$format_event(event))
#'
#' # advanced example:
#' lo <- LayoutDbi$new(
#'   event_vals = c("level", "timestamp", "msg"),
#'   logger_vals = "user",
#'   other_vals = list(pid = Sys.getpid, teststring = "blah"),
#'   col_types =  c(
#'     timestamp = "timestamp",
#'     level = "smallint",
#'     msg = "varchar(1024)",
#'     user = "varchar(256)",
#'     pid = "integer",
#'     teststring = "varchar(256)"
#'   )
#' )
#' lo$format_event(event)
#'
NULL




#' @export
LayoutDbi <- R6::R6Class(
  "LayoutDbi",
  inherit = LayoutTable,
  public = list(
    initialize = function(
      event_vals  = c("level", "timestamp", "caller", "msg"),
      logger_vals = NULL,
      other_vals = NULL,
      col_types = NULL
    ){
      if (!is.null(col_types)){
        assert(all_are_distinct(names(col_types)))
        assert(setequal(
          names(col_types),
          c(event_vals, logger_vals, names(other_vals))
        ),
          "col_type missing for columns: ",
           paste(
             setdiff(c(event_vals, logger_vals, names(other_vals)), names(col_types)),
             collapse = ", "
           )
         )
      }
      self$set_event_vals(event_vals)
      self$set_logger_vals(logger_vals)
      self$set_other_vals(other_vals)
      self$set_col_types(col_types)
    },

    format_event = function(event) {
      vals <- mget(self$event_vals, event)

      if (!is.null(self$logger_vals)){
        vals <- c(vals, mget(self$logger_vals, event[["logger"]]))
      }

      if (!is.null(private$.other_vals)){
        ov <- private$.other_vals
        for (i in seq_along(ov)){
          nm <- names(ov)[[i]]
          if (is.function(ov[[i]]))
            vals[[nm]] <- ov[[i]]()
          else
            vals[[nm]] <- ov[[i]]
        }
      }

      if (!is.null(private$.col_types)){
        vals <- vals[names(private$.col_types)]
      }
      vals <- c(vals, list(stringsAsFactors = FALSE))
      do.call(data.frame, args = vals)
    },

    set_col_types = function(x){
      if (!is.null(x)){
        assert(is.character(x))
        assert(identical(length(names(x)), length(x)))
      }
      private$.col_types <- x
      invisible(self)
    }
  ),

  active = list(
    col_types = function() private$.col_types,
    col_names = function() names(private$.col_types)
  ),

  private = list(
    .col_types = NULL
  )
)




# +- LayoutSqlite ---------------------------------------------------------

#' @export
LayoutSqlite <- R6::R6Class(
  "LayoutSqlite",
  inherit = LayoutDbi,
  public = list(
    format_event = function(event) {
      vals <- mget(self$event_vals, event)

      if (!is.null(self$logger_vals)){
        vals <- c(vals, mget(self$logger_vals, event[["logger"]]))
      }

      if (!is.null(private$.other_vals)){
        ov <- private$.other_vals
        for (i in seq_along(ov)){
          nm <- names(ov)[[i]]
          if (is.function(ov[[i]]))
            vals[[nm]] <- ov[[i]]()
          else
            vals[[nm]] <- ov[[i]]
        }
      }

      vals <- lapply(
        vals,
        function(.x) if (inherits(.x, "POSIXt")) format(.x) else .x
      )

      if (!is.null(private$.col_types)){
        vals <- vals[names(private$.col_types)]
      }
      vals <- c(vals, list(stringsAsFactors = FALSE))
      do.call(data.frame, args = vals)
    }
  )
)



# +- LayoutDBI utils ------------------------------------------------------

#' @export
select_dbi_layout <- function(conn){
  cls <- c(class(conn))
  switch(
    cls,
    "SQLiteConnection" = LayoutSqlite$new(),
    LayoutDbi$new()
  )
}

# LayoutJson --------------------------------------------------------------

#' LayoutJson
#'
#' Format a LogEvent as JSON
#'
#' @eval r6_usage(LayoutJson)
#'
#' @inheritSection LayoutTable Creating a New Layout
#'
#' @section Creating a New Layout:
#' \describe{
#'   \item{`toJSON_args`}{a list of values passed on to [jsonlite::toJSON()]}
#' }
#'
#' @inheritSection LayoutTable Fields and Methods
#' @section Fields and Methods:
#' \describe{
#'   \item{`toJSON_args`, `set_toJSON_args()`}{Get/set the `toJSON_args`}
#' }
#'
#' @name LayoutJson
#' @family Layouts
#' @include Filterable.R
#' @include log_levels.R
#' @seealso [read_json_lines()], [http://jsonlines.org/](http://jsonlines.org/)
#' @examples
#'
#' # setup a dummy LogEvent
#' event <- LogEvent$new(
#'   logger = Logger$new("dummy logger", user = "testuser"),
#'   level = 200,
#'   timestamp = Sys.time(),
#'   caller = NA_character_,
#'   msg = "a test message"
#' )
#' lo <- LayoutJson$new(
#'   event_vals = c("level", "timestamp", "msg"),
#'   logger_vals = "user",
#'   other_vals = list(pid = Sys.getpid, random_number = function() runif(3), teststring = "blah")
#' )
#' lo$format_event(event)
#' lo$format_event(event)
#'
NULL




#' @export
LayoutJson <- R6::R6Class(
  "LayoutJson",
  inherit = LayoutTable,
  public = list(
    initialize = function(
      event_vals  = c("level", "timestamp", "caller", "msg"),
      logger_vals = NULL,
      other_vals = NULL,
      toJSON_args = list(auto_unbox = TRUE)
    ){
      self$set_toJSON_args(toJSON_args)
      self$set_event_vals(event_vals)
      self$set_logger_vals(logger_vals)
      self$set_other_vals(other_vals)
    },

    format_event = function(event) {
      vals <- mget(self$event_vals, event)

      if (!is.null(self$logger_vals)){
        vals <- c(vals, mget(self$logger_vals, event[["logger"]]))
      }

      if (!is.null(private$.other_vals)){
        ov <- private$.other_vals
        for (i in seq_along(ov)){
         nm <- names(ov)[[i]]
         if (is.function(ov[[i]]))
           vals[[nm]] <- ov[[i]]()
         else
           vals[[nm]] <- ov[[i]]
        }
      }

      do.call(jsonlite::toJSON, args = c(list(vals), self$toJSON_args))
    },

    set_toJSON_args = function(x){
      assert(is.list(x))
      assert(identical(length(names(x)), length(x)))
      private$.toJSON_args <- x
      invisible(self)
    }
  ),

  active = list(
    toJSON_args = function() private$.toJSON_args
  ),

  private = list(
    .toJSON_args = NULL
  )
)
