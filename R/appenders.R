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
      threshold = 4
    ){
      self$layout    <- layout
      self$threshold <- threshold
    },

    append = function(x){
      if (x[["level"]] <= private$.threshold || is.na(private$.threshold)) {
        private$.layout$format_event(x)
      }
    },

    format = function(
      x,
      ...,
      colors = TRUE,
      single_line_summary = FALSE
    ){
      if (single_line_summary){
        return(private$single_line_summary(colors = colors))
      }

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

    print = function(..., single_line_summary = FALSE, colors = TRUE){
      cat(format(x = self, ..., colors = colors, single_line_summary = single_line_summary))
      invisible(self)
    }
  ),

  active = list(
    threshold = function(value){
      if (missing(value)) return(private$.threshold)
      if (is_scalar_character(value)){
        value <- unlabel_levels(value)
      }
      is.na(value) || assert_valid_threshold(value, log_levels = self$log_levels)
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
      .layout = NULL,
      single_line_summary = function(
        colors = TRUE
      ){
        threshold <- private$.threshold
        cls <- class(self)[[1]]

        return(ptrunc(
          paste0(cls, ": "),
          threshold,
          width = 80,
          sep = " "
        ))
      }
  )
)





# AppenderFormat ----------------------------------------------------------

AppenderFormat <- R6::R6Class(
  "AppenderFormat",
  inherit = Appender,
  public = list(
    initialize = function(
      threshold = 4L,
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
      threshold = 4L,
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
      if (is.na(private$.threshold) || x$level <= private$.threshold){
        cat(
          private$.layout$format_event(x), "\n",
          sep = ""
        )
      }

      return(invisible())
    }

  ),

  private = list(
    single_line_summary = function(
      colors = TRUE
    ){
      threshold <- private$.threshold
      cls <- class(self)[[1]]

      return(ptrunc(
        paste0(cls, ": "),
        threshold,
        width = 80,
        sep = " "
      ))
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
    }
  ),

  active = list(
    file = function(value){
      if (missing(value)) return(private$.file)
      private$.file <- value
    }
  ),

  private = list(
    .file = NULL,
    single_line_summary = function(
      colors = TRUE
    ){
      threshold <- private$.threshold
      cls <- class(self)[[1]]

      return(ptrunc(
        paste0(cls, ": "),
        threshold,
        "->",
        private$.file,
        width = 80,
        sep = " "
      ))
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
      private[["current_row"]] <- private[["current_row"]] + 1L
      private[["id"]] <- private[["id"]] + 1L

      data.table::set(
        private$.data,
        private[["current_row"]],
        j = c(".id", names(x)),
        value = c(private$id, as.list(x))
      )

      if (private[["current_row"]] == nrow(private$.data)){
        private[["current_row"]] <- 0L
      }
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



