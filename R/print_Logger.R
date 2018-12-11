print.Logger <- function(x){
  cat(format(x), sep = "\n")
}


format.Logger = function(
  x,
  ...
){
  header <- paste(
    paste0("<", class(x)[[1]], "> [", fmt_threshold(x$threshold, type = "character"), "]"),
    style_subtle(paste(format(x$ancestry), collapse = " -> "))
  )

  inherited_appenders <- do.call(rbind, lapply(x$inherited_appenders, srs_appender))
  appenders <- do.call(rbind, lapply(x$appenders, srs_appender))

  fmt_appenders <- function(.x){
    if (is.null(x)) NULL


    .x$name <- rownames(.x)
    .x$destination <- ifelse(
      !is_blank(.x$destination),
      paste("->", .x$destination),
      ""
    )


    with(
      .x,
      paste0(
        pad_right(name), ": ",
        pad_right(class), " [",
        pad_left(fmt_threshold(threshold, type = "character")), "] ",
        destination
      )
    )
  }

  ind <- "  "

  c(
    header,
    "",
    "appenders:",
    paste0(ind, fmt_appenders(appenders)),
    "",
    "inherited appenders:",
    paste0(ind, fmt_appenders(inherited_appenders))
  )

}



summary.Appender = function(
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
}




# format appenders --------------------------------------------------------



fmt_threshold <- function(
  x,
  type = "both",
  log_levels = getOption("yog.log_levels")
){
  assert(all(is.na(x)) || is_integerish(stats::na.omit(x)) || is.character(x))

  log_levels = c("off" = 0L, log_levels)
  if (is.character(x)){
    assert(all(x %in% names(log_levels)))
    x <- unlabel_levels(x, log_levels = log_levels)
  }

  impl <- function(.x){
    assert((length(.x) == 1L) && (is.na(.x)) || is_integerish(.x))

    if (.x %in% log_levels){
      .r <- log_levels[which(log_levels == .x)]

    } else if (is.na(.x)){
      .r <- c("all" = NA)
    } else {
      return(format(.x))
    }

    if (identical(type, "character")){
      return(names(.r))
    }

    paste0(names(.r), " (", .r, ")")
  }

  vapply(x, impl, character(1))
}



srs_appender <- function(x){
  data.frame(
    class = class_fmt(x, ignore = c("R6", "Filterable", "Appender")),
    threshold = x$threshold,
    destination = destination(x)
  )
}



#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
destination <- function(x){
  UseMethod("destination")
}



#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
destination.default <- function(x){
  "(unknown)"
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
destination.AppenderConsole <- function(x){
  "console"
}



#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
destination.AppenderFile <- function(x){
  x$file
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
destination.AppenderMemoryDt <- function(x){
  "memory"
}


