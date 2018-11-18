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
  cloneable = FALSE,


  # +- public --------------------------------------------------------------
  public = list(
    initialize = function(
      layout = Layout$new(),
      threshold = NA_integer_
    ){
      self$layout    <- layout
      self$threshold <- threshold
    },

    append = function(event){
      private$.layout$format_event(event)
    },


    print = function(..., colors = TRUE){
      cat(format(x = self, ..., colors = colors), "\n")
      invisible(self)
    }
  ),


  # +- active ---------------------------------------------------------------
  active = list(
    threshold = function(value){
      if (missing(value)) return(private$.threshold)
      assert_valid_threshold(value)

      if (is_scalar_character(value))
        value <- unlabel_levels(value)

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
    },

    destination = function() NULL
  ),

  private = list(
    .filters = list(check_threshold),
    .threshold = NA,
    .layout = NULL,
    .logger = NULL
  )
)



# AppenderConsole ---------------------------------------------------------

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
        colors = getOption("yog.colors")
      )
    ){
      self$threshold <- threshold
      self$layout <- layout
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

#' @inheritParams cat
#'
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
      self$file <- file
      self$threshold <- threshold
      self$layout <- layout
    },

    append = function(event){
      cat(
        private$.layout$format_event(event),
        sep = "\n", file = private$.file, append = TRUE
      )
      return(invisible())
    }
  ),


  # +- active ---------------------------------------------------------------
  active = list(
    file = function(value){
      if (missing(value)) return(private$.file)
      private$.file <- value
    },

    destination = function() self$file
  ),

  private = list(
    .file = NULL
  )
)






# AppenderMemory ----------------------------------------------------------


#' @include utils.R utils-sfmisc.R
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
      dd <- tail(dd[dd$level <= threshold], n)
      dd <- as.environment(dd)
      assign("logger", self$logger, dd)
      cat(self$layout$format_event(dd), sep = "\n")
    }
  ),



  # +- active ---------------------------------------------------------------
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


get_backup_timestamp <- function(
  x
){
  vapply(
    strsplit(x, ".", fixed = TRUE),
    function(.x) {
      .r <- .x[[length(.x)]]
      if (identical(.r, "zip"))  .r <- .x[[length(.x) - 1L]]
      .r
    },
    character(1)
  )
}



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

