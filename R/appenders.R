
# Appender ----------------------------------------------------------------



#' @include format.R
#' @include utils.R
#' @include utils-sfmisc.R
#'
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
        value <- self$unlabel_levels(value)
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
          threshold <- style_subtle(paste(iv, "<inherited>"))
        } else {
          threshold <- style_subtle("no threshold")
        }
        cls <- class(self)[[1]]

        paste(paste0(cls, ":"), threshold)
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






# AppenderFile ------------------------------------------------------------

#' @inheritParams cat
#'
#' @export
AppenderFile <- R6::R6Class(
  "AppenderFile",
  inherit = AppenderFormat,
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
      if (is.na(private$.threshold || x$level <= private$.threshold)){
        cat(
          private$.layout$format_event(x), "\n",
          file = private$.file,
          append = TRUE,
          sep = ""
        )
      }

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
        threshold <- style_subtle(paste(iv, "<inherited>"))
      } else {
        threshold <- style_subtle("no threshold")
      }
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



# AppenderConsole ---------------------------------------------------------

#' @export
AppenderConsole <- R6::R6Class(
  "AppenderConsole",
  inherit = AppenderFile,
  public = list(
    initialize = function(
      threshold = NA,
      layout = LayoutFormat$new()
    ){
      self$file <- ""
      self$threshold <- threshold
      self$layout <- layout
    }
  )
)




# appender crash ----------------------------------------------------------


# appender email ----------------------------------------------------------



