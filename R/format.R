#' Title
#'
#' @param x
#' @param format
#' @param timestamp_format
#'
#' @return
#' @export
#'
#' @examples
format.memlog_data <- function(
  x,
  format = "%L [%t] %m",
  timestamp_format = "%Y-%m-%d %H:%M:%S",
  ml = NULL,
  colors = NULL,
  ...
){
  stopifnot(
    is_scalar_character(format),
    is_scalar_character(timestamp_format)
  )

  # degenerate cases
    if (identical(nrow(x), 0L))  return("[empty log]")


  # init
    if (!is.null(ml)){
      lvls <- ml$label_levels(x$level)
      user <- ml$get_user() %||% NA_character_
    } else {
      lvls <- x$level
      user <- {
        if (requireNamespace("whoami", silent = TRUE))
          whoami::email_address(whoami::username())
        else
          "(unknown_user)"
      }
    }


  # tokenize
    tokens <- tokenize_format(
      format,
      valid_tokens = c("%t", "%u", "%p", "%c", "%m", "%l", "%L", "%n")
    )

  # format
    len  <- length(tokens)
    res  <- vector("list", length(tokens))
    for(i in seq_len(len)){
      res[[i]] <- switch(
        tokens[[i]],
        "%n" = x$level,
        "%l" = lvls,
        "%L" = toupper(lvls),
        "%t" = format(x$timestamp, format = timestamp_format),
        "%m" = x$msg,
        "%c" = x$caller %||% "(unknown function)",
        "%u" = user,
        "%p" = Sys.getpid(),
        tokens[[i]]
      )
    }
    res <- do.call(paste0, res)

  # colorize
    if (!is.null(colors)){
      color_levels <- ml$unlabel_levels(names(colors))
      for (i in seq_along(colors)){
        sel <- x$level == color_levels[[i]]
        res[sel] <- colors[[i]](res[sel])
      }
    }

  res
}




tokenize_format <- function(
  x,
  valid_tokens = NULL
){
  pos <- unlist(gregexpr("%.", x))

  if (identical(pos, -1L))
    return(x)
  pos <- sort(unique(c(1L, pos, pos + 2L, nchar(x) + 1L)))
  res <- vector("character", length(x))
  begin <- 1L
  for(i in seq_len(length(pos) -1L)) {
    res[[i]] <- substr(x, pos[[i]], pos[[i + 1]] - 1L)
  }

  if (!is.null(valid_tokens)){
    placeholders <- grep("%", res, value = TRUE)
    assert(
      all(placeholders %in% valid_tokens),
      "'format' contains unrecognised format specifications: ",
      paste(sort(setdiff(placeholders, valid_tokens)), collapse = ", ")
    )
  }

  res
}
