#' Print a Logger Object
#'
#' Display a short summary of the most important aspects of the logger
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
#' print(yog)
print.Logger <- function(
  x,
  color = requireNamespace("crayon", quietly = TRUE),
  ...
){
  assert(is_scalar_bool(color))
  cat(format(x, color = color), sep = "\n")
}




#' @export
#' @rdname print.Logger
format.Logger = function(
  x,
  color = requireNamespace("crayon", quietly = TRUE),
  ...
){
  assert(is_scalar_bool(color))
  assert(is_scalar_bool(color))
  if (!color) style_subtle <- identity

  header <- paste(
    paste0("<", class(x)[[1]], "> [", fmt_threshold(x$threshold, type = "character"), "]"),
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
    class = class_fmt(x, ignore = c("R6", "Filterable", "Appender")),
    threshold = x$threshold,
    destination = x$destination
  )
}




#' Print an Ancenstry Object
#'
#' @inheritParams print.Logger
#' @return `x` (invisibly)
#' @export
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
#' @rdname print.ancestry
format.ancestry <- function(
  x,
  color = requireNamespace("crayon", quietly = TRUE),
  ...
){
  assert(is_scalar_bool(color))
  seps <- rep("||", length(x))
  seps <- ifelse(x, " -> ", " | ")
  seps[length(seps)] <- ""

  if (color){
    names <- style_subtle(names(x))
    seps  <- style_subtle(seps)
    for (i in seq_along(x)){
      seps[[i]]  <- crayon::strip_style(seps[[i]])
      names[[i]] <- crayon::strip_style(names[[i]])
      if (!x[[i]]) break
    }
  }

  paste0(names, seps, collapse = "")
}
