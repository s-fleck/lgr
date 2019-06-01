#' Abstract Class for Layouts
#'
#' Abstract classes are exported for developers that want to extend them, they
#' are not useful to casual users.  Layouts get a [LogEvent] passed down from an
#' [Appender], and format them for output. How this formatting works exactly
#' varies widely. For example for file or console output the log event is
#' usually formatted into a single character line.
#'
#' @section Creating a New Layout:
#'
#' Layouts are instantiated with `<LayoutSubclass>$new()`. For a description of
#' the arguments to this function please refer to the Fields section.
#'
#' @section Methods:
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
    format_event = function(event) paste(capture.output(print(event$values)), collapse = " "),
    toString = function() "<empty>"
  )
)




# LayoutFormat ------------------------------------------------------------

#' Format Log Events as Text
#'
#' Format a [LogEvent] as human readable text using [format.LogEvent()], which
#' provides a quick and easy way to customize log messages. If you need
#' more control and flexibility, consider using [LayoutGlue] instead.
#'
#' @inheritParams format.LogEvent
#' @inheritSection Layout Methods
#' @inheritSection print.LogEvent Format Tokens
#'
#' @eval r6_usage(LayoutFormat)
#'
#' @section Creating a New LayoutFormat:
#'
#' A new LayoutFormat is instantiated with `LayoutFormat$new()`. For a
#' description of the arguments to this function please refer to the Fields,
#' and the documentation of [format.LogEvent()].
#'
#' @section Fields:
#' \describe{
#'   \item{`fmt`}{
#'     a `character` scalar containing format tokens. See [format.LogEvent()].}
#'   \item{`timestamp_fmt`}{
#'     a `character` scalar. See [base::format.POSIXct()]}
#'   \item{`colors`}{a named `list` of functions passed on on [format.LogEvent()]}
#'   \item{`pad_levels`}{`right`, `left` or `NULL`. See [format.LogEvent()]}
#'  }
#'
#' @section Format Tokens:
#' This is the same list of format tokens as for [format.LogEvent()]
#'
#'
#' @name LayoutFormat
#' @family Layouts
#' @include Filterable.R
#' @include log_levels.R
#' @examples
#' # setup a dummy LogEvent
#' event <- LogEvent$new(
#'   logger = Logger$new("dummy logger"),
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
    },

    toString = function(){
      paste(fmt_class(class(self)[[1]]), self$fmt)
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

#' Format Log Events as Text via glue
#'
#' Format a [LogEvent] as human readable text using [glue::glue]. The function
#' is evaluated in an environment in which it has access to all elements of
#' the [LogEvent] (see examples). This is more flexible than [LayoutFormat],
#' but also more complex and slightly less performant.
#'
#' @eval r6_usage(LayoutGlue)
#'
#' @section Creating a New LayoutGlue:
#'
#' A new `LayoutGlue` is instantiated with `LayoutGlue$new()`. It takes a single
#' argument `fmt` that is passed on to `glue::glue()` for each LogEvent.
#'
#' @inheritSection Layout Methods
#' @section Fields:
#'
#' \describe{
#'   \item{`fmt`}{see [glue::glue()]}
#'  }
#'
#' @name LayoutGlue
#' @family Layouts
#' @include Filterable.R
#' @include log_levels.R
#' @seealso lgr exports a number of formatting utility functions that are
#'   useful for layout glue: [colorize_levels()], [pad_left()], [pad_right()].
#' @examples
#' lg <- get_logger("test")$
#'   set_appenders(AppenderConsole$new())$
#'   set_propagate(FALSE)
#'
#' lg$appenders[[1]]$set_layout(LayoutGlue$new())
#' lg$fatal("test")
#'
#'
#' # All fields of the LogEvent are available, even custom ones
#' lg$appenders[[1]]$layout$set_fmt(
#'   "{logger$name} {level_name}({level}) {caller}: {toupper(msg)} {{custom: {custom}}}"
#' )
#' lg$fatal("test", custom = "foobar")
#' lg$config(NULL)  # reset logger config
#'
NULL



#' @export
LayoutGlue <- R6::R6Class(
  "LayoutGlue",
  inherit = Layout,
  public = list(
    initialize = function(
      fmt = "{pad_right(colorize_levels(toupper(level_name)), 5)} [{timestamp}] msg"
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
    },


    toString = function() {
      paste(fmt_class(class(self)[[1]]), self$fmt)
    }
  ),


  active = list(
    fmt = function()  private$.fmt
  ),

  private = list(
    .fmt = NULL
  )
)




# LayoutDbi ---------------------------------------------------------------


#' Format Log Events for Output to Databases
#'
#' LayoutDbi can contain `col_types` that [AppenderDbi] can use to create new
#' database tables; however, it is safer and more flexible to set up the log
#' table up manually with an `SQL CREATE TABLE` statement instead.
#'
#' The LayoutDbi parameters `fmt`, `timestamp_fmt`, `colors` and `pad_levels`
#' are only applied for for console output via the `$show()` method and do not
#' influence database inserts in any way. The inserts are pre-processed by
#' the methods `$format_data()`, `$format_colnames` and `$format_tablenames`.
#'
#' It does not format
#' LogEvents directly, but their `data.table` representations (see
#' [as.data.table.LogEvent]), as well as column- and table names.
#'
#'
#' @eval r6_usage(LayoutDbi)
#' @inheritSection LayoutFormat Methods
#' @inheritSection Layout Creating a New Layout
#'
#' @section Creating a New Layout:
#'
#'
#' @section Fields:
#'
#' \describe{
#'   \item{`col_types`}{A named `character` vector of column types supported by
#'   the target database. If not `NULL` this is used by [AppenderDbi] or similar
#'   Appenders to create a new database table on instantiation of the Appender. If
#'   the target database table already exists, `col_types` is not used.
#'   }
#'   \item{`col_names`}{Convenience method to get the names of the `col_types`
#'     vector}
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{`format_table_name(x)`}{Format table names before inserting into
#'     the database. For example some databases prefer all lowercase names,
#'     some uppercase. SQL updates should be case-agnostic, but sadly in
#'     practice not all DBI backends behave consistently in this regard}
#'   \item{`format_colnames`}{Format column names before inserting into the
#'     database. See `$format_table_name` for more info}
#'   \item{`format_data`}{Format the input `data.table` before inserting into
#'     the database. Usually this function does nothing, but for example for
#'     SQLite it has to apply formatting to the timestamp.
#'   }
#'   \item{`col_names`}{Convenience method to get the names of the `col_types`
#'     vector}
#' }
#'
#' @section Database Specific Layouts:
#'
#' Different databases have different data types and features. Currently the
#' following `LayoutDbi` subclasses exist that deal with specific databases,
#' but this list is expected to grow as lgr matures:
#'
#'   * `LayoutSqlite`: For SQLite databases
#'   * `LayoutPostgres`: for Postgres databases
#'   * `LayoutMySql`: for MySQL databases
#'   * `LayoutDb2`: for DB2 databases
#'
#' The utility function [select_dbi_layout()] tries returns the appropriate
#' Layout for a DBI connection, but this does not work for odbc and JDBC
#' connections where you have to specify the layout manually.
#'
#'
#' @name LayoutDbi
#' @aliases LayoutSqlite LayoutRjdbc LayoutRjdbcDb2 LayoutDb2 LayoutMySql LayoutPostgres
#' @family Layouts
#' @family database layouts
#' @include Filterable.R
#' @include log_levels.R
#' @seealso [select_dbi_layout()], [DBI::DBI],
#'
NULL




#' @export
LayoutDbi <- R6::R6Class(
  "LayoutDbi",
  inherit = LayoutFormat,
  public = list(
    initialize = function(
      col_types = NULL,
      fmt = "%L [%t] %m  %f",
      timestamp_fmt = "%Y-%m-%d %H:%M:%S",
      colors = getOption("lgr.colors", list()),
      pad_levels = "right",

      format_table_name = identity,
      format_colnames = identity,
      format_data = identity
    ){
      self$set_col_types(col_types)
      self$set_fmt(fmt)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_colors(colors)
      self$set_pad_levels(pad_levels)

      self$format_table_name <- format_table_name
      self$format_colnames   <- format_colnames
      self$format_data       <- format_data

      self
    },

    format_table_name = NULL,
    format_colnames   = NULL,
    format_data       = NULL,


    set_col_types = function(x){
      if (!is.null(x)){
        assert(is.character(x))
        assert(identical(length(names(x)), length(x)))
      }
      private$.col_types <- x
      invisible(self)
    },


    sql_create_table = function(table){
      assert(
        !is.null(private$.col_types),
        "To create new database tables the Layout must contain `col_types`"
      )
      sql_create_table(
        tname = table,
        col_types = private$.col_types,
        col_names = names(private$.col_types)
      )
    },


    toString = function(){
      paste(
        fmt_class(class(self)[[1]]),
        paste(self$col_names, collapse = ", ")
      )
    }
  ),


  active = list(
    col_types = function() {
      r <- get(".col_types", envir = private)
      names(r) <- self$format_colnames(names(r))
      r
    },


    col_names = function(){
      names(self$col_types)
    }
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

    initialize = function(
      col_types = NULL,
      fmt = "%L [%t] %m  %f",
      timestamp_fmt = "%Y-%m-%d %H:%M:%S",
      colors = getOption("lgr.colors", list()),
      pad_levels = "right",

      format_table_name = tolower,
      format_colnames = tolower,
      format_data = function(x){
        for (nm in names(x)){
          if (inherits(x[[nm]], "POSIXt"))
            data.table::set(x, i = NULL, j = nm, value = format(x[[nm]]))
        }
        x
      }
    ){
      self$set_col_types(col_types)
      self$set_fmt(fmt)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_colors(colors)
      self$set_pad_levels(pad_levels)

      self$format_table_name <- format_table_name
      self$format_colnames   <- format_colnames
      self$format_data       <- format_data

      self
    }
  )
)




# +- LayoutPostgres ----------------------------------------------------------

#' @export
LayoutPostgres <- R6::R6Class(
  "LayoutPostgres",
  inherit = LayoutDbi,
  public = list(
    initialize = function(
      col_types = NULL,
      fmt = "%L [%t] %m  %f",
      timestamp_fmt = "%Y-%m-%d %H:%M:%S",
      colors = getOption("lgr.colors", list()),
      pad_levels = "right",

      format_table_name = function(x){
        if (is_Id(x)) x else tolower(x)
      },
      format_colnames = tolower,
      format_data = identity
    ){
      self$set_col_types(col_types)
      self$set_fmt(fmt)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_colors(colors)
      self$set_pad_levels(pad_levels)

      self$format_table_name <- format_table_name
      self$format_colnames   <- format_colnames
      self$format_data       <- format_data

      self
    }
  )
)


# +- LayoutMySql ----------------------------------------------------------

#' @export
LayoutMySql <- R6::R6Class(
  "LayoutMySql",
  inherit = LayoutDbi,
  public = list(
    initialize = function(
      col_types = NULL,
      fmt = "%L [%t] %m  %f",
      timestamp_fmt = "%Y-%m-%d %H:%M:%S",
      colors = getOption("lgr.colors", list()),
      pad_levels = "right",

      format_table_name = as_tname,
      format_colnames = tolower,
      format_data       = function(x){
        data.table::setnames(x, tolower(names(x)))
        x
      }
    ){
      self$set_col_types(col_types)
      self$set_fmt(fmt)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_colors(colors)
      self$set_pad_levels(pad_levels)

      self$format_table_name <- format_table_name
      self$format_colnames   <- format_colnames
      self$format_data       <- format_data

      self
    }
  )
)





# +- LayoutDb2 ----------------------------------------------------------

#' @export
LayoutDb2 <- R6::R6Class(
  "LayoutDb2",
  inherit = LayoutDbi,
  public = list(
    initialize = function(
      col_types = NULL,
      fmt = "%L [%t] %m  %f",
      timestamp_fmt = "%Y-%m-%d %H:%M:%S",
      colors = getOption("lgr.colors", list()),
      pad_levels = "right",

      format_table_name = function(x){
        if (inherits(x, "Id")){
          x
        } else {
          toupper(x)
        }
      },
      format_colnames = toupper,
      format_data = function(x){
        names(x) <- toupper(names(x))
        x
      }
    ){
      self$set_col_types(col_types)
      self$set_fmt(fmt)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_colors(colors)
      self$set_pad_levels(pad_levels)

      self$format_table_name <- format_table_name
      self$format_colnames   <- format_colnames
      self$format_data       <- format_data

      self
    }
  )
)


# +- LayoutRjdbc ----------------------------------------------------------


#' @export
LayoutRjdbc <- R6::R6Class(
  "LayoutDbi",
  inherit = LayoutDbi,
  public = list(
    initialize = function(
      col_types = NULL,
      fmt = "%L [%t] %m  %f",
      timestamp_fmt = "%Y-%m-%d %H:%M:%S",
      colors = getOption("lgr.colors", list()),
      pad_levels = "right",

      format_table_name =  as_tname,
      format_colnames = toupper,
      format_data = function(x){
        names(x) <- toupper(names(x))
        x
      }
    ){
      self$set_col_types(col_types)
      self$set_fmt(fmt)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_colors(colors)
      self$set_pad_levels(pad_levels)

      self$format_table_name <- format_table_name
      self$format_colnames   <- format_colnames
      self$format_data       <- format_data

      self
    }
  )
)




# +- LayoutRjdbcDb2 ----------------------------------------------------------

#' @export
LayoutRjdbcDb2 <- R6::R6Class(
  "LayoutDbi",
  inherit = LayoutDbi,
  public = list(
    initialize = function(
      col_types = NULL,
      fmt = "%L [%t] %m  %f",
      timestamp_fmt = "%Y-%m-%d %H:%M:%S",
      colors = getOption("lgr.colors", list()),
      pad_levels = "right",

      format_table_name =  function(x) toupper(as_tname(x)),
      format_colnames = toupper,
      format_data = function(x){
        names(x) <- toupper(names(x))
        x
      }
    ){
      self$set_col_types(col_types)
      self$set_fmt(fmt)
      self$set_timestamp_fmt(timestamp_fmt)
      self$set_colors(colors)
      self$set_pad_levels(pad_levels)

      self$format_table_name <- format_table_name
      self$format_colnames   <- format_colnames
      self$format_data       <- format_data

      self
    }
  )
)






# LayoutJson --------------------------------------------------------------

#' Format LogEvents as JSON
#'
#' Format a LogEvent as JSON
#'
#' @eval r6_usage(LayoutJson)
#'
#' @inheritSection Layout Creating a New Layout
#' @inheritSection Layout Methods
#'
#' @section Creating a New Layout:
#'
#' @section Fields:
#' \describe{
#'   \item{`toJSON_args`, `set_toJSON_args()`}{a list of values passed on to
#'     [jsonlite::toJSON()]
#'    }
#' }
#'
#' @section Methods:
#'
#' @name LayoutJson
#' @family Layouts
#' @include Filterable.R
#' @include log_levels.R
#' @seealso [read_json_lines()], [http://jsonlines.org/](http://jsonlines.org/)
#' @examples
#' # setup a dummy LogEvent
#'
#' event <- LogEvent$new(
#'   logger = Logger$new("dummy logger"),
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
NULL




#' @export
LayoutJson <- R6::R6Class(
  "LayoutJson",
  inherit = Layout,
  public = list(
    initialize = function(
      toJSON_args = list(auto_unbox = TRUE)
    ){
      # init
      self$set_toJSON_args(toJSON_args)
    },

    format_event = function(event) {
      do.call(
        jsonlite::toJSON,
        args = c(list(x = event$values), get(".toJSON_args", private))
      )
    },

    set_toJSON_args = function(x){
      assert(is.list(x))
      assert(identical(length(names(x)), length(x)))
      private$.toJSON_args <- x
      invisible(self)
    },

    toString = function() {
      fmt_class(class(self)[[1]])
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




#' Select Appropriate Database Table Layout
#'
#' Selects an appropriate Layout for a database table based on
#' a DBI connection and - if it already exists in the database -
#' the table itself.
#'
#' @param conn  a [DBI connection][DBI::dbConnect()]
#' @param table a `character` scalar. The name of the table to log to.
#'
#' @export
select_dbi_layout <- function(
  conn,
  table
){
  cls <- c(class(conn))

  if (identical(class, "MySQLConnection")){
    stop(
      "'RMySQL' is not supported by lgr. Please use the newer 'RMariaDB'",
      "package to connect to MySQL and MariaDB databases"
    )
  }

  res <- switch(
    cls,
    "PostgreSQLConnection" = LayoutPostgres$new(),

    "PqConnection" = LayoutPostgres$new(),

    "MariaDBConnection" = LayoutMySql$new(),

    "MySQLConnection" = LayoutMySql$new(),

    "SQLiteConnection" = LayoutSqlite$new(
      col_types = c(
        level = "integer",
        timestamp = "TEXT",
        logger = "TEXT",
        caller = "TEXT",
        msg = "TEXT"
      )),

    "JDBCConnection" = LayoutRjdbc$new(
      col_types = c(
        level = "smallint",
        timestamp = "timestamp",
        logger = "varchar(256)",
        caller = "varchar(256)",
        msg = "varchar(2048)"
      )),
    LayoutDbi$new()
  )

  ct <- get_col_types(conn, table)

  if (!is.null(ct))  res$set_col_types(ct)

  res
}



get_col_types <- function(conn, table){
  res <- tryCatch({
    dd  <- DBI::dbSendQuery(conn, paste("SELECT * FROM", table))
    res <- DBI::dbColumnInfo(dd)
    DBI::dbClearResult(dd)
    if ("type" %in% names(res))
      setNames(as.character(res$type), tolower(res$name))
    else if ("field.type" %in% names(res))
      setNames(as.character(res$field.type), tolower(res$field.name))
  },
    error = function(e) NULL
  )

  res
}
