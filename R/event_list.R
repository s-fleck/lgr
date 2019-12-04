#' #' A List of LogEvents
#'
#' An event_list is a class for `list()`s whose only elements are [LogEvents].
#' This structure is occasionally used internally in lgr (for example by
#' [AppenderBuffer]) and can be useful for developers that want to write
#' their own Appenders.
#'
#' For convenience, `as.data.frame()` and `as.data.table()` methods
#' exist for event lists.
#'
#' @param x any `R` object
#' @param ... for `event` elements to be added to the list, for the `as_*()`
#'   functions paramters passed on to methods.
#'
#' @rdname event_list
#' @export
event_list <- function(...){
  as_event_list(list(...))
}




#' A List of LogEvents
#'
#' An event_list is a class for `list()`s whose only elements are [LogEvents].
#' This structure is occasionally used internally in lgr (for example by
#' [AppenderBuffer]) and can be useful for developers that want to write
#' their own Appenders.
#'
#' @rdname event_list
#' @return
#'   an `event_list()` and `as_event_list()` return a `list` of subclass
#'   `event_list`
#'
#'   `as.data.frame` and `as.data.table` return a `data.frame` or `data.table`
#'   respectively
#' @export
#'
as_event_list <- function(x, ...){
  UseMethod("as_event_list")
}




#' @rdname event_list
#' @export
as_event_list.list <- function(
  x,
  ...
){
  assert(all(vapply(x, inherits, logical(1), "LogEvent")))
  structure(x, class = c("event_list", "list"))
}




#' @rdname event_list
#' @param na.rm remove `NA` values before coercing a data.frame to an `event_list()`.
#' @export
as_event_list.data.frame <- function(
  x,
  na.rm = TRUE,
  ...
){
  structure(lapply(
    seq_len(nrow(x)),

    function(i){
      dd <- as.list(x[i, !names(x) %in% c(".id", ".custom")])

      if (na.rm){
        for (j in rev(seq_along(dd))) if (is.na(dd[[j]])) dd[[j]] <- NULL
      }

      r <- as.environment(c(dd, x[i, ][[".custom"]][[1]]))
      r[["values"]] <- rev(as.list(r))
      r
    }
  ),
    class = c("event_list", "list")
  )
}




#' @rdname event_list
#' @export
as.data.table.event_list <- function(x, na.rm = TRUE){
  data.table::rbindlist(
    lapply(
      x,
      data.table::as.data.table,
      needs_boxing = Negate(is_scalar_atomic)
  ),
  fill = TRUE,
  use.names = TRUE)
}



#' @inheritParams as.data.frame.LogEvent
#' @rdname event_list
#' @export
as.data.frame.event_list <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  stringsAsFactors = FALSE,
  na.rm = TRUE,
  ...
){
  as.data.frame(as.data.table.event_list(x), stringsAsFactors = stringsAsFactors)
}
