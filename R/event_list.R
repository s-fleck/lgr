#' A List of LogEvents
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
#'   functions parameters passed on to methods.
#'
#' @family docs relevant for extending lgr
#' @export
event_list <- function(...){
  as_event_list(list(...))
}




#' @rdname event_list
#' @return
#'   an `event_list()` and `as_event_list()` return a flat `list`
#'   of [LogEvents]. Nested lists get automatically flattened.
#'
#'   `as.data.frame` and `as.data.table` return a `data.frame` or `data.table`
#'   respectively
#' @export
#' @examples
#' e <- LogEvent$new(level = 300, msg = "a", logger = lgr)
#' as_event_list(e)
#' as_event_list(c(e, e))
#' # nested lists get automatically unnested
#' as_event_list(c(e, list(nested_event = e)))
#'
as_event_list <- function(x, ...){
  UseMethod("as_event_list")
}




#' @rdname event_list
#' @export
as_event_list.list <- function(
  x,
  ...,
  scalarize = FALSE
){
  if (length(x)){
    res <- unlist(x)
    assert(all(vapply(res, inherits, logical(1), "LogEvent")))
    if (scalarize){
      res <- unlist(lapply(res, as_event_list, scalarize = TRUE))
    }
  } else {
    res <- list()
  }

  structure(res, class = c("event_list", "list"))
}




#' @param scalarize `logical` scalar. Turn [LogEvents] with non-scalar `msg`
#'   field into separate log events
#' @export
#' @rdname event_list
#' @examples
#' # scalarize = TRUE "unpacks" events with vector log messages
#' e <- LogEvent$new(level = 300, msg = c("A", "B"), logger = lgr)
#' as_event_list(e, scalarize = FALSE)
#' as_event_list(e, scalarize = TRUE)
#'
as_event_list.LogEvent <- function(
  x,
  ...,
  scalarize = FALSE
){
  if (scalarize && length(msgs <- get("msg", envir = x)) > 1){
    vals <- x$values
    vals <- vals[!names(vals) %in% c("msg", "logger")]
    as_event_list.list(lapply(
      msgs,
      function(m) do.call(LogEvent$new, c(list(msg = m, logger = get(".logger", envir = x)), vals))
    ))
  } else {
    event_list(x)
  }
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

    # the hardcoded .id and .fields columns are used by lgrExtra::AppenderDt
    function(i){
      dd <- as.list(x[i, !names(x) %in% c(".id", ".fields")])

      if (na.rm){
        for (j in rev(seq_along(dd))) if (is.na(dd[[j]])) dd[[j]] <- NULL
      }

      r <- as.environment(c(dd, x[i, ][[".fields"]][[1]]))
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
      box_if = Negate(is_scalar_atomic)
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
