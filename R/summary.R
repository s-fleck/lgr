summary.Logger = function(
  x,
  ...
){

  x <- yog

  res <- list(
    ancestry = x$ancestry,
    appenders = self$appenders,
    ancestral_appenders = x$ancestral_appenders,
  )

  x$ancestry

  header <- paste(
    paste0("<", class(self)[[1]], ">"),
    style_subtle(format(self$ancestry))
  )

  ind <- "  "

  appenders <- do.call(rbind, lapply(self$appenders, srs))



  obsums <- object_summaries(self)

  act <- obsums[obsums == "active binding"]
  act <- paste0(names(act), ": ", act)

  funs <- obsums[grep("function", obsums)]
  funs <- paste0(names(funs), ": ", funs)

  sel <- grepl(logger_pat, funs)
  methods <- funs[!sel]
  loggers <- c(
    paste0(names(private$.log_levels), ": function (msg, ...) "),
    grep("^log:", funs, value = TRUE)
  )

  c(
    header,
    paste0(ind, "Fields / Active Bindings:"),
    paste0(ind, ind, "threshold: ",  fmt_threshold(self$threshold)),
    paste0(ind, ind, "string_formatter: ", sls(self$string_formatter)),
    paste0(ind, ind, "user: ", self$user),
    paste0(ind, ind, "propagate:", self$propagate),
    paste0(ind, ind, "appenders:"),
    paste0(ind, ind, ind, appenders),
    paste0(ind, ind, ind, anc_appenders)[!is.null(anc_appenders)],
    paste0(ind, "Methods:"),
    paste0(ind, ind, "Loggers:"),
    paste0(ind, ind, ind, loggers),
    paste0(ind, ind, methods)
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



format_appenders <- function(
  x
){
  res <- matrix(
    dimnames = list(NULL, c("names", "threshold", "destination")),
    nrow = length(x),
    ncol = 3
  )
  if (length(x) > 0){
    res[, "names"] <- pad_right(names(x))
    res[, "threshold"] <- vapply(x, function(.x) format_threshold(.x$threshold), character(1))
    res[, "destination"] <- vapply(x, function(.x) .x$destination, character(1))


    appenders <- paste0(
      pad_right(appenders$name), ": ",
      pad_right(
        paste0(label_levels(appenders$threshold), style_subtle(paste0(" (", appenders$threshold, ")")))
      ),
      vapply(appenders$comment, ptrunc_col, character(1), width = 128)
    )
  } else {
    appenders <- style_subtle("none")
  }

  anc_appenders <- do.call(
    rbind,
    lapply(self$ancestral_appenders, function(.x){
      cbind(data.frame(logger = .x$logger$name, srs(.x)))
    }
    ))


  if (length(anc_appenders) > 0){
    anc_appenders$logger <- pad_right(anc_appenders$logger)
    anc_appenders$name   <- pad_right(anc_appenders$name)

    anc_appenders <- paste0(
      style_subtle(paste0(anc_appenders$logger, " -> ")),
      anc_appenders$name, ": ",
      pad_right(
        paste0(label_levels(anc_appenders$threshold), style_subtle(paste0(" (", anc_appenders$threshold, ")")))
      ),
      vapply(anc_appenders$comment, ptrunc_col, character(1), width = 128)
    )
  } else {
    anc_appenders <- NULL
  }
}




format_threshold <- function(
  x,
  log_levels = getOption("yog.log_levels")
){
  assert(all(is.na(x)) || is_integerish(na.omit(x)) || is.character(x))

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

    paste0(names(.r), " (", .r, ")")
  }

  vapply(x, impl, character(1))



}

