#' Abstract Class for Layouts
#'
#' Abstract classes are exported for developers that want to extend them, they
#' are not useful to casual users.  Layouts get a [LogEvent] passed down from an
#' [Appender], and format them for output. How this formatting works exactly
#' varries widely. For example for file or console output the log event is
#' usually formatted into a single character line.
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
#' @keywords internal
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

#' Format Log Events as Text
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




# LayoutGlue ------------------------------------------------------------

#' Format Log Events as Text with Glue
#'
#' Format a [LogEvent] as human readable text using [glue::glue]. The function
#' is evaluated in an environment in which it has access to all elements of
#' the [LogEvent] (see examples)
#'
#' @eval r6_usage(LayoutGlue)
#'
#' @section Creating a New Layout:
#'
#'
#' \describe{
#'   \item{`fmt`}{see [glue::glue()]}
#'  }
#'
#'
#' @inheritSection Layout Fields and Methods
#'
#'
#' @name LayoutGlue
#' @family Layouts
#' @include Filterable.R
#' @include log_levels.R
#' @examples
#' lg <- Logger$new("testlogger", appenders = AppenderConsole$new(), propagate = FALSE)
#' lg$appenders[[1]]$set_layout(LayoutGlue$new())
#' lg$fatal("test")
#'
#'
#' # All fields of the LogEvent are available, even custom ones
#' lg$appenders[[1]]$layout$set_fmt("{logger$name} {level_name}({level}) {caller}: {toupper(msg)} {{custom: {custom}}}")
#' lg$fatal("test", custom = "foobar")
#'
#'
NULL



#' @export
LayoutGlue <- R6::R6Class(
  "LayoutGlue",
  inherit = Layout,
  public = list(
    initialize = function(
      fmt = "{pad_right(colorize_levels(toupper(level_name)), 5)} [{timestamp}] msg",
      colors = NULL
    ){
      assert_namespace("glue")
      self$set_fmt(fmt)
    },

    format_event = function(
      event
    ){
      op <- parent.env(event)
      on.exit(parent.env(event) <- op)
      parent.env(event) <- environment()
      unclass(glue::glue(get(".fmt", private), .envir = event))
    },

    set_fmt = function(x){
      assert(is_scalar_character(x))
      private$.fmt <- x
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
    }
  ),


  active = list(
    fmt = function()  private$.fmt
  ),

  private = list(
    .fmt = NULL
  )
)




# LayoutTable -------------------------------------------------------------

#' Abstract Class for Formatting Data as Tabular Structures
#'
#' Abstract classes are exported for developers that want to extend them, they
#' are not useful to casual users. [LayoutDbi] and [LayoutJson] are derived
#' from LayoutTabel.
#'
#' @section Creating a New Layout:
#'
#' \describe{
#'   \item{`event_values`}{Names of the fields of the [LogEvent]
#'     to include in the output object.
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
    format_event = function(event) { as.data.frame(event) },

    set_event_values = function(x){
      assert(is.character(x) || is.null(x))
      private$.event_values <- x
      invisible(self)
    },

    set_val_names = function(x){
      assert(is.null(x) || is.character(x))
      private$.val_names <- x
      invisible(self)
    }

  ),

  active = list(
    event_values  = function() get(".event_values", private),
    val_names   = function() get(".val_names", private)
  ),

  private = list(
    .event_values  = NULL,
    .val_names   = NULL
  )
)




# LayoutDbi ---------------------------------------------------------------


#' Format Log Events for Output to Databases
#'
#' Format a [LogEvent] as `data.frame` for inserting into a relational database.
#'
#' @eval r6_usage(LayoutDbi)
#'
#' @inheritSection LayoutTable Creating a New Layout
#'
#' @section Creating a New Layout:
#' \describe{
#'   \item{`col_types`}{A named `character` vector of column types supported by
#'     the target database. If this is used instead of `event_values`, and
#'     the target logging `table` does not yet exist, the column type
#'     information is used by [AppenderDbi] or similar Appenders to create a
#'     new database table, either on instantion of the Appender or on writing
#'     of the first LogEvent. If the target database table already exists,
#'     the column type information is not used. You can only supply one of
#'     `event_values` and `col_types`.
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
#' but this list is expected to grow as lgr matures:
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
#' # advanced example that supports a custom_field:
#' lo <- LayoutDbi$new(
#'   event_values = c("level", "timestamp", "msg", "custom_field"),
#'   col_types =  c(
#'     timestamp = "timestamp",
#'     level = "smallint",
#'     msg = "varchar(2048)",
#'     custom_field = "integer"
#'   )
#' )
#'
#' event <- LogEvent$new(
#'   logger = Logger$new("dummy logger", user = "testuser"),
#'   level = 200,
#'   timestamp = Sys.time(),
#'   caller = NA_character_,
#'   msg = "a test message",
#'   custom_field = "blubb"
#' )
#'
#' lo$format_event(event)
#' #'
NULL




#' @export
LayoutDbi <- R6::R6Class(
  "LayoutDbi",
  inherit = LayoutTable,
  public = list(
    initialize = function(
      event_values  = NULL,
      col_types = NULL
    ){
      assert(
        is.null(event_values) + is.null(col_types) >= 1,
        "You can either supply `event_values` or `col_types`, not both. ",
        "If you dont supply anything, `event_values` will default to: ",
        paste(DEFAULT_FIELDS, collapse = ", ")
      )


      if (is.null(event_values))  event_values <- names(col_types)
      if (is.null(event_values))  event_values <- DEFAULT_FIELDS


      if (!is.null(col_types)){
        if (is.null(event_values)){

        }
        assert_colnames_match_valnames(names(col_types), event_values)

      }

      self$set_event_values(event_values)
      self$set_col_types(col_types)
    },

    format_event = function(event){
      ev <- get(".event_values", private)

      if (length(ev)){
        vals <- mget(ev, event, ifnotfound = NA)
      } else {
        if (is.null(ev)){
          vals <- get("values", event)
        } else {
          vals <- c()
        }
      }

      ct <- get(".col_types", private)
      if (length(ct)){
        assert(setequal(names(vals), names(ct)))
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
    },

    sql_create_table = function(table){
      generate_sql_create_table(
        tname = table,
        col_types = private$.col_types,
        col_names = names(private$.col_types)
      )
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
      ev <- get(".event_values", private)

      if (length(ev)){
        vals <- mget(ev, event, ifnotfound = NA)
      } else {
        if (is.null(ev)){
          vals <- get("values", event)
        } else {
          vals <- c()
        }
      }


      ct <- get(".col_types", private)
      if (length(ct)){
        if (!setequal(names(vals), names(ct))){
          browser()
        }
        assert(setequal(names(vals), names(ct)))
        vals <- vals[names(private$.col_types)]
      }

      vals[] <- lapply(
        vals,
        function(.x) if (inherits(.x, "POSIXt")) format(.x) else .x
      )

      vals <- c(vals, list(stringsAsFactors = FALSE))
      do.call(data.frame, args = vals)
    }
  )
)




# +- LayoutRjdbc ----------------------------------------------------------

#' @export
LayoutRjdbc <- LayoutSqlite




# +- LayoutDBI utils ------------------------------------------------------

#' @export
select_dbi_layout <- function(
  conn,
  table
){
  cls <- c(class(conn))

  res <- switch(
    cls,
    "SQLiteConnection" = LayoutSqlite$new(
      col_types = c(
        level = "integer",
        timestamp = "character",
        caller = "character",
        msg = "character"
      )),
    "JDBCConnection" = LayoutRjdbc$new(
      col_types = c(
        level = "smallint",
        timestamp = "timestamp",
        caller = "varchar(1024)",
        msg = "varchar(2048)"
      )),
    LayoutDbi$new(event_values = c("level", "timestamp",  "caller", "msg"))
  )

  db_names <- tryCatch({
    DBI::dbListFields(conn, table)
  },
    error = function(e) NULL
  )

  if (!is.null(db_names)){
    res$set_col_types(NULL)
    res$set_event_values(db_names)
  }


  res
}




# LayoutJson --------------------------------------------------------------

#' Format LogEvents as JSON
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
#'
#' event <- LogEvent$new(
#'   logger = Logger$new("dummy logger", user = "testuser"),
#'   level = 200,
#'   timestamp = Sys.time(),
#'   caller = NA_character_,
#'   msg = "a test message",
#'   custom_field = "LayoutJson can handle arbitrary fields"
#' )
#'
#' # Default settings show all event fals
#' lo <- LayoutJson$new()
#' lo$format_event(event)
#'
#'
#' # Values from the LogEvent can be suppressed
#' lo <- LayoutJson$new(
#'   event_values = c("level", "timestamp", "msg")
#' )
#' lo$format_event(event)
#'
NULL




#' @export
LayoutJson <- R6::R6Class(
  "LayoutJson",
  inherit = LayoutTable,
  public = list(
    initialize = function(
      event_values  = NULL,
      toJSON_args = list(auto_unbox = TRUE)
    ){
      # init
      event_values  <- name_vals(event_values)
      self$set_event_values(event_values)
      self$set_toJSON_args(toJSON_args)
    },

    format_event = function(event) {
      ev <- get(".event_values", private)

      if (length(ev)){
        vals <- mget(ev, event, ifnotfound = NA)
        names(vals) <- names(ev)
      } else {
        if (is.null(ev)){
          vals <- get("values", event)
        } else {
          vals <- c()
        }
      }

      do.call(jsonlite::toJSON, args = c(list(vals), get(".toJSON_args", private)))
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




# utils -------------------------------------------------------------------

assert_colnames_match_valnames <- function(
  colnames,
  valnames
){
  assert(all_are_distinct(colnames))
  assert(all_are_distinct(valnames))

  if (!setequal(colnames, valnames)){
    msg <- ""
    mis_ct <- setdiff(valnames, names(colnames))
    ext_ct <- setdiff(colnames, valnames)

    if (length(mis_ct)){
      msg <- paste0(
        "col_type missing: ", paste(mis_ct, collapse = ", "), "."
      )
    }

    if (length(ext_ct)){
      msg <- paste0(
        msg, "col_type defined but not part of layout: ",
        paste(ext_ct, collapse = ", "),
        "."
      )
    }

    msg <- paste(
      msg,
      "If you set `event_values` to NULL, they will automatically be overriden",
      "by `col_types`."
    )

    stop(msg, call. = FALSE)
  }

  TRUE
}




name_vals <- function(x){
  if (is.null(names(x)))
    names(x) <- x
  else
    names(x)[names(x) == ""] <- x[names(x) == ""]

  x
}
