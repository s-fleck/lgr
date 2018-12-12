#' Print a Logger Object
#'
#' Display a short summary of the most important aspects of the logger
#'
#' @param x any \R Object
#' @param colors `TRUE` or `FALSE`: Output with colors? Requires the Package
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
  colors = requireNamespace("crayon", quietly = TRUE),
  ...
){
  cat(format(x, colors = colors), sep = "\n")
}




#' @export
#' @rdname print.Logger
format.Logger = function(
  x,
  colors = requireNamespace("crayon", quietly = TRUE),
  ...
){
  assert(is_scalar_bool(colors))
  if (!colors) style_subtle <- identity

  header <- paste(
    paste0("<", class(x)[[1]], "> [", fmt_threshold(x$threshold, type = "character"), "]"),
    style_subtle(paste(format(x$ancestry), collapse = " -> "))
  )

  appenders <-
    do.call(rbind, lapply(x$appenders, srs_appender))
  inherited_appenders <-
    do.call(rbind, lapply(x$inherited_appenders, srs_appender))

  fmt_appenders <- function(.x){
    if (is.null(.x)) return(NULL)

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

  res <- header

  if (!is.null(appenders)){
    res <-c(
      res, "",
      "appenders:", paste0(ind, fmt_appenders(appenders))
    )
  }

  if (!is.null(inherited_appenders)){
    res <-c(
      res, "",
      "inherited appenders:", paste0(ind, fmt_appenders(inherited_appenders))
    )
  }

  res
}




srs_appender <- function(x){
  data.frame(
    class = class_fmt(x, ignore = c("R6", "Filterable", "Appender")),
    threshold = x$threshold,
    destination = x$destination
  )
}
