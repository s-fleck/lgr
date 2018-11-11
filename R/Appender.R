#' @include print.R
#' @include utils.R
#' @include utils-sfmisc.R
#' @include Filterable.R

# Appender ----------------------------------------------------------------


#' @aliases Appenders
#' @export
Appender <- R6::R6Class(
  "Appender",
  inherit = Filterable,

  public = list(
    initialize = function(
      layout = Layout$new(),
      threshold = 400
    ){
      self$layout    <- layout
      self$threshold <- threshold
    },

    append = function(x){
      private$.layout$format_event(x)
    },

    format = function(
      x,
      ...,
      colors = TRUE
    ){
      ind <- "  "

      header <- paste(
        class(self)[[1]],
        style_subtle(class_fmt(self, c("R6", class(self)[[1]])))
      )

      # Object summaries
      obs    <- object_summaries(self)
      methods <- obs[grep("function", obs)]
      methods <- methods[!names(methods) %in% c("clone", "initialize", "print")]


      active <- obs[obs == "active binding"]
      active_bindings <- lapply(names(active), function(.x){
        sv <- private[[paste0(".", .x)]]
        iv <- self[[.x]]
        if (!is.null(sv))
          return(sls(sv))
        else if (!is.null(iv))
          return(paste(style_accent(sls(iv)), style_subtle("(inherited)")))
        else
          return(style_subtle("NULL"))
      })
      names(active_bindings) <- names(active)

      # print

      paste0(
        paste0(header, "\n"),
        paste0(ind, "Active Bindings:\n"),
        paste0(ind, ind, names(active_bindings), ": ", active_bindings, collapse = "\n"), "\n",
        paste0(ind, "Methods:\n"),
        paste0(ind, ind, names(methods), ": ", methods, collapse = "\n")
      )
    },

    print = function(..., colors = TRUE){
      cat(format(x = self, ..., colors = colors), "\n")
      invisible(self)
    }
  ),

  active = list(
    threshold = function(value){
      if (missing(value)) return(private$.threshold)
      if (is_scalar_character(value)){
        value <- unlabel_levels(value)
      }
      is.na(value) || assert_valid_threshold(value, log_levels = getOption("yog.log_levels"))
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
    }
  ),

  private = list(
    .filters = list(check_threshold),
    .threshold = 300,
    .layout = NULL,
    .logger = NULL
  )
)




# AppenderFormat ----------------------------------------------------------

AppenderFormat <- R6::R6Class(
  "AppenderFormat",
  inherit = Appender,
  public = list(
    initialize = function(
      threshold = 400L,
      layout = LayoutFormat$new()
    ){
      self$threshold <- threshold
      self$layout    <- layout
    }
  )
)


# AppenderConsole ---------------------------------------------------------

#' @export
AppenderConsole <- R6::R6Class(
  "AppenderConsole",
  inherit = AppenderFormat,
  public = list(
    initialize = function(
      threshold = 400L,
      layout = LayoutFormat$new(
        fmt = "%L [%t] %m",
        timestamp_fmt = "%H:%M:%OS3",
        colors = getOption("yog.colors")
      )
    ){
      self$threshold <- threshold
      self$layout <- layout
    },

    append = function(x){
      cat(private$.layout$format_event(x), sep = "\n")
      return(invisible())
    }
  )
)






# AppenderFile ------------------------------------------------------------

#' @inheritParams cat
#'
#' @export
AppenderFile <- R6::R6Class(
  "AppenderFile",
  inherit = AppenderConsole,
  public = list(
    initialize = function(
      file,
      threshold = NA,
      layout = LayoutFormat$new()
    ){
      self$file <- file
      self$threshold <- threshold
      self$layout <- layout
    },

    append = function(x){
      cat(
        private$.layout$format_event(x),
        sep = "\n", file = private$.file, append = TRUE
      )
      return(invisible())
    }
  ),

  active = list(
    file = function(value){
      if (missing(value)) return(private$.file)
      private$.file <- value
    }
  ),

  private = list(
    .file = NULL
  )
)




# AppenderRotating ----------------------------------------------------

#' @inheritParams cat
#'
#' @export
AppenderRotating <- R6::R6Class(
  "AppenderRotating",
  inherit = AppenderFile,
  public = list(
    initialize = function(
      file,
      threshold = NA,
      layout = LayoutFormat$new(),
      compress = FALSE,
      compress_args = list(flags = paste0(formals(utils::zip)$flags, "q"))
    ){
      self$file <- file
      self$threshold <- threshold
      self$layout <- layout
      self$compress <- compress
      self$compress_args <- compress_args
    },

    append = function(x){
      cat(
        private$.layout$format_event(x),
        sep = "\n", file = private$.file, append = TRUE
      )
      return(invisible())
    },

    do_rollover = function(){
      assert(file.exists(self$file))
      backups <- private$list_backups()
      idx_pad <- 1L

      if (length(backups) > 0){
        idx <- names(backups)
        ext <- ifelse(tools::file_ext(backups) == "zip", ".zip", "")
        sorder <- order(sort(as.integer(idx)), decreasing = TRUE)
        idx <- idx[sorder]
        ext <- ext[sorder]

        assert(!anyNA(idx))
        idx_pad <- max(nchar(as.character(as.integer(idx) + 1L)))

        # walk instead of vectorized file.rename to prevent accidental overwrites
        walk(
          seq_along(idx),
          function(i){
            file.rename(
              paste0(self$file, ".", idx[[i]], ext[[i]]),
              paste0(self$file, ".", as.integer(idx[[i]]) + 1L, ext[[i]])
            )
          }
        )
      }

      if (self$compress) {
        utils::capture.output(do.call(
          utils::zip,
          c(list(zipfile = paste0(self$file, ".1.zip"), files  = self$file), self$compress_args)
        ))
        file.remove(self$file)

      } else {
        file.rename(self$file, paste0(self$file, ".1"))
      }

      backups <- private$list_backups()
      file.rename(backups, autopad_backup_index(backups))
      invisible(self)
    },


    prune_backups = function(max_backups){
      backups <- private$list_backups()
      if (length(backups) >= max_backups){
        backups <- backups[order(as.integer(names(backups)))]
        to_remove <- backups[-c(seq_len(max_backups))]
        file.remove(to_remove)
        backups <- private$list_backups()
        file.rename(backups, autopad_backup_index(backups))
      }
      invisible(self)
    }
  ),


  active = list(
    file = function(value){
      if (missing(value)) return(private$.file)
      private$.file <- value
    },


    compress = function(value){
      if (missing(value)) return(private$.compress)
      assert(is_scalar_bool(value))
      private$.compress <- value
    },


    compress_args = function(value){
      if (missing(value)) return(private$.compress_args)
      assert(
        identical(length(names(value)), length(value)) &&
        all(names(value) %in% names(formals(utils::zip))),
        "'compress_args' must be a named list, all names be valid arguments",
        "for tools::zip()."
      )
      private$.compress_args <- value
    }
  ),


  private = list(
    .file = NULL,
    .compress = FALSE,
    .compress_args = NULL,
    list_backups = function(){
      backups <- list.files(
        dirname(self$file),
        pattern = paste0(basename(self$file), "\\.\\d*(\\.zip){0,1}$"),
        full.names = TRUE
      )
      idx <- get_backup_index(backups)
      setNames(backups, idx)
    }
  )
)





# AppenderMemory ----------------------------------------------------------


#' @include utils.R utils-sfmisc.R
#' @export
AppenderMemoryDt <- R6::R6Class(
  "AppenderMemoryDt",
  inherit = AppenderFormat,
  public = list(
    initialize = function(
      threshold = NA,
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


    append = function(x){
      # could just use as.list[,c(...)] but mget has a bit less overhead
      #vals <- mget(c("level", "timestamp", "caller", "msg"), x)
      vals <- x[["values"]]
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
      threshold = NA
    ){
      if (is.na(threshold)) threshold <- Inf
      dd <- self$data
      dd <- tail(dd[dd$level <= threshold], n)
      dd <- as.environment(dd)
      assign("logger", self$logger, dd)
      cat(self$layout$format_event(dd), sep = "\n")
    }
  ),


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
    }
  ),


  private = list(
    id = 0L,
    current_row = 0L,
    .data = NULL
  )
)




# appender crash ----------------------------------------------------------


# appender email ----------------------------------------------------------




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

# AppenderMemoryDtBuffer --------------------------------------------------

#' @include utils.R utils-sfmisc.R
#' @export
AppenderMemoryBufferDt <- R6::R6Class(
  "AppenderMemoryBufferDt",
  inherit = AppenderFormat,
  public = list(
    initialize = function(
      threshold = NA,
      appenders = NULL,
      flush_level = 200,
      should_flush = function(
        x
      ){
        FALSE

      },
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
      x
    ){
      # could just use as.list[,c(...)] but mget has a bit less overhead
      #vals <- mget(c("level", "timestamp", "caller", "msg"), x)
      vals <- x[["values"]]
      lengths <- vapply(vals, length, integer(1), USE.NAMES = FALSE)
      lenmax  <- max(lengths)
      assert(all(lengths %in% c(1, lenmax)))


      # Special care is necessary if the log event is bigger than the buffer
      if (
        lenmax > nrow(private$.data)
      ){
        self$flush()  # flush current cache
        self$flush(x) # flush x before trimming it so that if fits the buffer
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
        self$flush()
        private[["current_row"]] <- lenmax
      }

      data.table::set(
        private$.data,
        i,
        j = c(".id", names(vals)),
        value = c(list(ids), vals)
      )

      if (self$should_flush(x)){
        self$flush()
      }

      private[["id"]] <- private[["id"]] + lenmax
      NULL
    },

    show = function(
      n = 20,
      threshold = NA
    ){
      if (is.na(threshold)) threshold <- Inf
      dd <- self$data
      dd <- tail(dd[dd$level <= threshold], n)
      dd <- as.environment(dd)
      assign("logger", self$logger, dd)
      cat(self$layout$format_event(dd), sep = "\n")
    },

    flush = function(
      x = get(".data", envir = private)
    ){
      for (app in c(self$appenders)) {
        if (app$filter(x)){
          app$append(x)
        }
      }
    },

    finalize = function(){
      self$flush()
    }
  ),

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
    }
  ),


  private = list(
    id = 0L,
    current_row = 0L,
    flushed_row = 0L,
    .data = NULL
  )
)


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

