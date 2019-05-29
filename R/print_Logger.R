#' Print a Logger Object
#'
#' The `print()` method for Loggers displays the most important aspects of
#' the Logger.
#'
#'
#' @param x any \R Object
#' @param color `TRUE` or `FALSE`: Output with color? Requires the Package
#'   **crayon**
#' @param ... ignored
#'
#' @return
#'   `print()` returns `x` (invisibly), `format()` returns a `character` vector.
#' @export
#'
#' @examples
#' # print most important details of logger
#' print(lgr)

print.Logger <- function(
  x,
  color = requireNamespace("crayon", quietly = TRUE),
  ...
){
  assert(is_scalar_bool(color))
  cat(format(x, color = color), sep = "\n")
  invisible(x)
}




#' @export
#' @rdname print.Logger
format.Logger = function(
  x,
  color = FALSE,
  ...
){
  assert(is_scalar_bool(color))
  assert(is_scalar_bool(color))
  if (!color) style_subtle <- identity

  thr <- fmt_threshold(x$threshold, type = "character")

  if (is.null(x[[".__enclos_env__"]][["private"]][[".threshold"]])){
    thr <- style_subtle(thr)
  }


  header <- paste(
    paste0("<", class(x)[[1]], "> [", thr, "]"),
    format(x$ancestry, color = color)
  )

  appenders <- appender_summary(x$appenders)
  inherited_appenders  <- appender_summary(x$inherited_appenders)


  ind <- "  "
  res <- header

  if (!is.null(appenders)){
    res <-c(
      res, "",
      "appenders:", paste0(ind, appenders)
    )
  }

  if (!is.null(inherited_appenders)){
    res <- c(
      res, "",
      "inherited appenders:", paste0(ind, inherited_appenders)
    )
  }

  res
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



# single-row-summary
srs_appender <- function(x){
  data.frame(
    class = fmt_class(class(x)[[1]]),
    threshold = x$threshold,
    destination = x$destination
  )
}




#' @description
#' You can also print just the `ancestry` of a Logger which can be accessed with
#' with `logger$ancestry()`. This returns a named `character` vector whose
#' names correspond to the names of the Loggers `logger` inherits from. The
#' `TRUE`/`FALSE` status of its elements correspond to the `propagate` values of
#' these Loggers.
#'
#' @rdname print.Logger
#' @export
#'
#' @examples
#' # print only the ancestry of a logger
#' lg <- get_logger("AegonV/Aerys/Rheagar/Aegon")
#' get_logger("AegonV/Aerys/Rheagar")$set_propagate(FALSE)
#'
#' print(lg$ancestry)
#' unclass(lg$ancestry)

print.ancestry <- function(
  x,
  color = requireNamespace("crayon", quietly = TRUE),
  ...
){
  assert(is_scalar_bool(color))
  cat(format(x), "\n")
  invisible(x)
}




#' @export
#' @rdname print.Logger
format.ancestry <- function(
  x,
  color = FALSE,
  ...
){
  assert(is_scalar_bool(color))
  sps <- rep("/", length(x))
  nms <- names(x)

  style <- identity

  if (color){
    for (i in rev(seq_along(x))){
      nms[[i]] <- style(nms[[i]])
      sps[[i]] <- style(sps[[i]])
      if (!x[[i]]){
        style <- style_subtle
      }
    }
  }

  sps[length(sps)] <- ""

  paste0(nms, sps, collapse = "")
}
