#' @include print.R
#' @include utils.R
#' @include utils-sfmisc.R

# Appender ----------------------------------------------------------------



#' @export
Appender <- R6::R6Class(
  "Appender",

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
    }
  ),

  private = list(
    .threshold = NULL,
    .layout = NULL
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
        timestamp_fmt = "%H:%M:%S",
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
        "\n", sep = "", file = private$.file, append = TRUE
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
      lengths <- unlist(eapply(x, length, USE.NAMES = FALSE))
      lenmax  <- max(lengths)
      assert(all(lengths %in% c(1, lenmax)))

      if (lenmax > nrow(private$.data)){
        x <- eapply(x, trim_last_value, nrow(private$.data))
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
        private[["current_row"]] <- lenmax  # reset cache
      }

      data.table::set(
        private$.data,
        i,
        j = c(".id", names(x)),
        value = c(list(ids), as.list(x))
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

trim_last_value <- function(x, max_len){
  if (length(x) == 1L)
    x
  else
    x[seq.int(length(x) - max_len + 1L, length(x))]
}


