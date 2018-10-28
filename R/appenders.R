
#' @include format.R
#' @include utils.R
#' @include utils-sfmisc.R
#'
#' @export
Appender <- R6::R6Class(
  "Appender",

  public = list(
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
    parent_memlog = function(value){
      if (missing(value)) return(private$.parent_memlog)
      private$.parent_memlog <- value
    },
    threshold = function(value){
      if (missing(value)){
        res <- private$.threshold %||% private$.parent_memlog$threshold
        if (is.character(res))
          res <- private$.parent_memlog$unlabel_levels(res)
        return(res)
      }

      if (is.character(value))
        res <- private$.parent_memlog$unlabel_levels(res)

      is.null(value) || assert_valid_threshold(
        value,
        private$.parent_memlog$log_levels,
        sprintf("Illegal threshold set for appender <%s>: ", class(self)[[1]])
      )

      private$.threshold <- value
    }
  ),
    private = list(
      .threshold = NULL,
      .parent_memlog = NULL,
      single_line_summary = function(colors = TRUE){
        sv <- private$.threshold
        iv <- self$threshold
        ml <- self$parent_memlog

        if (!is.null(sv)){
          threshold <- sv
          if (!is.null(ml)){
            threshold <- paste0(ml$label_levels(sv), " (", sv, ")")
          }

        } else if (!is.null(iv)){
          iv <- paste0(ml$label_levels(iv), " (", iv, ")")
          threshold <- paste(style_accent(iv), style_subtle("(inherited)"))
        } else {
          threshold <- style_subtle("no threshold")
        }
        cls <- class(self)[[1]]

        return(paste(fmt_class(cls), threshold))
      }
  )
)




# appender format ---------------------------------------------------------

AppenderFormat <- R6::R6Class(
  "AppenderFormat",
  inherit = Appender,
  public = list(
    initialize = function(
      threshold = NULL,
      formatter = format.memlog_data,
      fmt = "%L [%t] %m",
      timestamp_fmt = "%Y-%m-%d %H:%M:%S",
      colors = NULL
    ){
      self$threshold <- threshold
      self$formatter <- formatter
      self$fmt <- fmt
      self$timestamp_fmt <- "%Y-%m-%d %H:%M:%S"
      self$colors <- colors
    }
  ),

  active = list(
    formatter = function(value){
      if (missing(value)) return(private$.formatter)
      private$.formatter <- value
    },

    fmt = function(value){
      if (missing(value)) return(private$.fmt)
      private$.fmt <- value
    },

    timestamp_fmt = function(value){
      if (missing(value)) return(private$.timestamp_fmt)
      private$.timestamp_fmt <- value
    },

    colors = function(value){
      if (missing(value)) return(private$.colors)
      private$.colors <- value
    }
  ),

  private = list(
    format_entry = function(x){
      private$.formatter(
        x,
        fmt = private$.fmt,
        timestamp_fmt = private$.timestamp_fmt,
        colors = private$.colors,
        ml = private$.parent_memlog
      )
    },
    .formatter = NULL,
    .fmt = NULL,
    .timestamp_fmt = NULL,
    .colors = NULL
  )
)




#' @export
AppenderConsole <- R6::R6Class(
  "AppenderConsole",
  inherit = AppenderFormat,
  public = list(
    append = function(){
      x <- private$.parent_memlog$collector$last_value

      if (x$level <= self$threshold)
        cat(private$format_entry(x), "\n")

      return(invisible(x$msg))
    }
  )
)




#' @inheritParams cat
#'
#' @export
AppenderFile <- R6::R6Class(
  "AppenderFile",
  inherit = AppenderFormat,
  public = list(
    initialize = function(
      file,
      threshold = NULL,
      formatter = format.memlog_data,
      fmt = "%L [%t] %m",
      timestamp_fmt = "%Y-%m-%d %H:%M:%S"
    ){
      self$file <- file
      self$threshold <- threshold
      self$formatter <- formatter
      self$fmt <- fmt
      self$timestamp_fmt <- fmt
    },


    append = function(){
      threshold <- self$threshold
      x <- private$.parent_memlog$collector$last_value

      if (x$level <= threshold){
        cat(
          private$format_entry(x), "\n",
          file = private$.file,
          append = TRUE
        )
      }

      return(invisible(x$msg))
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
    single_line_summary = function(colors = TRUE){
      sv <- private$.threshold
      iv <- self$threshold
      ml <- self$parent_memlog

      if (!is.null(sv)){
        threshold <- sv
        if (!is.null(ml)){
          threshold <- paste0(ml$label_levels(sv), " (", sv, ")")
        }

      } else if (!is.null(iv)){
        iv <- paste0(ml$label_levels(iv), " (", iv, ")")
        threshold <- paste(style_accent(iv), style_subtle("(inherited)"))
      } else {
        threshold <- style_subtle("no threshold")
      }
      cls <- class(self)[[1]]

      return(paste(fmt_class(cls), threshold, ptrunc(private$.file)))
    }
  )
)




# appender glue -----------------------------------------------------------


AppenderGlue <- R6::R6Class(
  "AppenderGlue",
  inherit = AppenderFormat,
  public = list(
    initialize = function(
      threshold = NULL,
      fmt = "{toupper(.ml$label_levels(level))} [{format(timestamp, format = '%Y-%m-%d %H:%M:%S')}] {msg}",
      colors = NULL
    ){
      assert_namespace("glue")

      self$threshold <- threshold
      self$formatter <- glue::glue
      self$fmt <- fmt
      self$colors <- colors
    },

    append = function(){
      x <- private$.parent_memlog$collector$last_value
      if (x$level <= self$threshold){
        private$format_entry(x)
      } else {
        invisible(NULL)
      }
    }
  ),

  private = list(
    format_entry = function(x){
      .ml <- private$.parent_memlog  # make available for glue format
      do.call(glue::glue, c(list(private$.fmt), as.list(x)))
    }
  )
)




#' @export
AppenderConsoleGlue <- R6::R6Class(
  "AppenderConsoleGlue",
  inherit = AppenderGlue,
  public = list(
    append = function(){
      x <- private$.parent_memlog$collector$last_value

      if (x$level <= self$threshold)
        cat(private$format_entry(x), "\n")

      return(invisible(x$msg))
    }
  )
)




# appender minimal --------------------------------------------------------


#' @export
AppenderConsoleMinimal <- R6::R6Class(
  "AppenderConsoleMinimal",
  inherit = Appender,
  public = list(
    initialize = function(
      threshold = NULL,
      timestamp_fmt ="%Y-%m-%d %H:%M:%S",
      colors = NULL
    ){
      self$threshold <- threshold
      self$timestamp_fmt <- timestamp_fmt
      self$colors <- colors
    },

    append = function(){
      x <- private$.parent_memlog$collector$last_value

      if (x$level > self$threshold){
        return(invisible(x$msg))
      } else {
        pad <- max(nchar(names(private$.parent_memlog$log_levels)))
        lvl <- pad_right(toupper(private$.parent_memlog$label_levels(x$level)), pad)

        if (!identical(length(private$.colors), 0L)){
          coln <- private$.parent_memlog$unlabel_levels(names(private$.colors))
          lvl <- colorize_levels(lvl, x$level, private$.colors, coln )
        }

        cat(
          lvl,
          " [", format(x$timestamp, private$.timestamp_fmt), "] ",
          x$msg,
          "\n",
          sep = ""
        )
        return(invisible(x$msg))
      }
    }
  ),

  active = list(
    timestamp_fmt = function(value){
      if (missing(value)) return(private$.timestamp_fmt)

      assert(is_scalar_character(value))
      private$.timestamp_fmt <- value
    },

    colors = function(value){
      if (missing(value)) return(private$.colors)
      private$.colors <- value
    }
  ),


  private = list(
    .timestamp_fmt = NULL,
    .colors = NULL
  )
)







# appender crash ----------------------------------------------------------


# appender email ----------------------------------------------------------



pad_left <- function(
  x,
  nchar = max(nchar(x)),
  pad = " "
){
  diff <- nchar - nchar(x)
  padding <-
    vapply(diff, function(i) paste(rep.int(" ", i), collapse = ""), character(1))
  lvls <- paste0(padding, x)
}


pad_right <- function(
  x,
  nchar = max(nchar(x)),
  pad = " "
){
  diff <- nchar - nchar(x)
  padding <-
    vapply(diff, function(i) paste(rep.int(" ", i), collapse = ""), character(1))
  paste0(x, padding)
}
