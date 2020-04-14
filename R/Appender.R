# Appender ----------------------------------------------------------------

#' Appenders
#'
#' @description
#' Appenders are assigned to [Loggers] and manage the output of the [LogEvents]
#' to a destination (such as the console or a text file). An Appender has a
#' single [Layout] that tells it how to format the LogEvent. For details
#' please refer to the documentations of the specific Appenders.
#'
#' More appenders can be found in the package [lgrExtra](https://github.com/s-fleck/lgrExtra)
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

    initialize = function(
      layout = Layout$new(),
      threshold = NA_integer_
    ){
      self$set_layout(layout)
      self$set_threshold(threshold)
    },

    #' @description Process a [LogEvent] `event`. This method is usually not
    #' called by the user, but invoked by a [Logger]
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
#' A simple Appender that outputs to the console. If you have the package
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
    file = function() private$.file,
    data = function(){
      parser <- self$layout$parse

      assert(
        !is.null(parser),
        "Neither ", fmt_class(class(self)[[1]]), " nor ", fmt_class(class(self$layout)[[1]]),
        " support parsing log files to data.frames. Consider using <LayoutJson> ",
        "instead."
      )

      parser(self$file)
    },

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
#' `AppenderJson` is just a shortcut for `AppenderFile` preconfigured with
#' [`LayoutJson`].
#'
#' @family Appenders
#' @rdname AppenderFile
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
#' @description
#' AppenderTable is extended by Appenders that write to a data source that
#' can be interpreted as tables, (usually a `data.frame`). Examples are
#' [lgrExtra::AppenderDbi], [lgrExtra::AppenderRjdbc] and [lgrExtra::AppenderDt].
#'
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
#' @export
AppenderTable <- R6::R6Class(
  "AppenderTable",
  inherit = Appender,
  cloneable = FALSE,

  public = list(
    initialize = function(...){
      stop(CannotInitializeAbstractClassError())
    },


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




# AppenderMemory  --------------------------------------------------

#' Abstract class for logging to memory buffers
#'
#' @template abstract_class
#'
#' @description
#' AppenderMemory is extended by Appenders that retain an in-memory event
#' buffer, such as [AppenderBuffer] and [lgrExtra::AppenderPushbullet].
#'
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
#'  [log level][log_level]. Minimum event level that will trigger flushing of
#'  the buffer (set `NULL` for no flushing). This behaviour is implemented
#'  through `should_flush()`, and you can modify that function for different
#'  behaviour.
#'   }
#'   \item{`should_flush(event)`, `set_should_flush(x)`}{
#'     A function with exactly one arguments: `event`.
#'     If the function returns `TRUE`, flushing of the buffer
#'     is triggered. Defaults to flushing if an event of level `error`
#'     or higher is registered.}
#' }
#'
#'
#' @export
#' @seealso [LayoutFormat]
#'
#' @export
AppenderMemory <- R6::R6Class(
  "AppenderMemory",
  inherit = Appender,
  cloneable = FALSE,
  public = list(
    initialize = function(...){
      stop(CannotInitializeAbstractClassError())
    },

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


    #' @description  Sends the buffer's contents to all attached Appenders and
    #'   then clears the Buffer
    flush = function(){self},


    #' @description Clears the buffer, discarding all buffered Events
    clear = function(self){},


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
      if (!is.null(level))
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
      as_event_list(
        res[!vapply(res, is.null, FALSE)]
      )
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
#' An Appender that Buffers LogEvents in-memory and and redirects them to other
#' Appenders once certain conditions are met.
#'
#'
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
#' @export
#' @seealso [LayoutFormat]
#' @family Appenders
#' @export
AppenderBuffer <- R6::R6Class(
  "AppenderBuffer",
  inherit = AppenderMemory,
  cloneable = FALSE,
  public = list(

    #' @description
    #' The [Layout] for this Appender is used only to format console output of
    #' its `$show()` method.
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
#'
#' @section Fields:
#'
#' \describe{
#'
#'
#'   \item{`backups`}{A `data.frame` containing information on path, file size,
#'     etc... on the available backups of `file`.}
#'  }
#'
#'
#' @export
#' @seealso [AppenderFileRotatingDate], [AppenderFileRotatingTime], [rotor::rotate()]
#' @family Appenders, Rotating Appenders
#' @name AppenderFileRotating
#' @export
AppenderFileRotating <- R6::R6Class(
  "AppenderFileRotating",
  inherit = AppenderFile,
  public = list(

    #' @description
    #' @param age,size,max_backups,fmt,overwrite,compression,backup_dir
    #' Please see [rotor::rotate()] for the meaning of these arguments
    #  (`fmt` is passed on as `format`).
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

#' Log to a time-stamped rotating file
#'
#' @seealso [AppenderFileRotatingDate], [AppenderFileRotating], [rotor::rotate()]
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


    #' @description set the `cache_backups` flag.
    #' @param x a `logical` scalar
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
#' do not need to inclue sub-day accuracy.
#'
#' @seealso [AppenderFileRotatingTime], [AppenderFileRotating], [rotor::rotate()]
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




# helpers -----------------------------------------------------------------

#' Default reader for files created with AppenderFile
#'
#' @param file a `character` scalar: path to a file
#' @param threshold only `NA` is supported for the default reader, other
#'   values cause a warning. See `LayoutFormat$read()` for an example where
#'   that supports thresholds.
#' @param n an `integer` scalar > 0
#'
#' @return
#' @noRd
default_file_reader <- function(file, threshold, n){
  assert(
    is_scalar_character(file),
    "`file` must be the path to a file, not ", preview_object(file)
  )

  if (isFALSE(is.na(threshold))){
    warning("A threshold was supplied to AppenderFile$show(), but the ",
    "Appenders' Layout does not support filtering by log level")
  }

  last_n(readLines(file), n)
}





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
