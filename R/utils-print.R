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
