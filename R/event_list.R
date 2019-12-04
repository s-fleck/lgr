event_list <- function(...){
  as_event_list(list(...))
}




#' Convert a data.frame to a list of LogEvents
#'
#' convert a data.frame as returned for example by AppenderDt$dt to a list of
#  LogEvent like environments. Useful for printing.
#'
#' @param x
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
as_event_list <- function(x, na.rm = TRUE){
  UseMethod("as_event_list")
}




#' Title
#'
#' @param x
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
as_event_list.list <- function(
  x,
  na.rm = TRUE
){
  assert(all(vapply(x, inherits, logical(1), "LogEvent")))
  structure(x, class = c("event_list", "list"))
}




#' @export
as_event_list.data.frame <- function(x, na.rm = TRUE){
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




#' @export
as.data.frame.event_list <- function(x, na.rm = TRUE){
  as.data.frame(as.data.table.event_list(x))
}
