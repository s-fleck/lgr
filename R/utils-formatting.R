#' Pad Character Vectors
#'
#' @param x a `character` vector
#' @param width `integer` scalar. target string width
#' @param pad `character` scalar. the symbol to pad with
#'
#' @export pad_right
#' @name pad_right
#'
#' @examples
#' pad_left("foo", 5)
#' pad_right("foo", 5, ".")
#' pad_left(c("foo", "foooooo"), pad = ".")
NULL




#' @export pad_left
#' @rdname pad_right
#' @name pad_left
NULL




# internal ----------------------------------------------------------------

fmt_function_signature <- function(x){
  paste0("function(", paste(names(formals(x)), collapse = ", "), ")")
}




fmt_threshold <- function(
  x,
  type = "both",
  log_levels = getOption("lgr.log_levels")
){
  assert(all(is.na(x)) ||  is_integerish(x[!is.na(x)]) || is.character(x))

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
