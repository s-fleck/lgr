#'
#' @keywords internal
#' @importFrom data.table data.table set
#' @importFrom stats setNames
"_PACKAGE"




#' @export yog
.onLoad <- function(...){
  if (requireNamespace("colt", quietly = TRUE)){
    options(
      yog.colors = list(
        "fatal" = function(x) colt::clt_emph2(colt::clt_error(x)),
        "error" = colt::clt_error,
        "warn"  = colt::clt_warning,
        "debug" = colt::clt_chr_subtle,
        "trace" = colt::clt_chr_subtle
      )
    )
  }

   options(
      yog.log_levels = c(
        "fatal" = 100L,
        "error" = 200L,
        "warn"  = 300L,
        "info"  = 400L,
        "debug" = 500L,
        "trace" = 600L
      )
    )


  appenders <- list(
    console = AppenderConsole$new()
  )

  if (requireNamespace("data.table", quietly = TRUE)){
    appenders[["memory"]] <- AppenderMemoryDt$new()
  }

  assign(
    "yog",  # usally a logger should have the same name as the package it belongs to
    Logger$new(
      name = "root",
      appenders = appenders,
      parent = NULL  # _never_ set the parent to null yoursel, root should be the only root logger
    ),
    envir =  asNamespace("yog")
  )
}
