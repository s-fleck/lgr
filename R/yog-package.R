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

  appenders <- list(
    console = AppenderConsole$new()
  )

  if (requireNamespace("data.table", quietly = TRUE)){
    appenders[["memory"]] <- AppenderMemoryDt$new()
  }

  assign(
    "yog",
    Logger$new(appenders = appenders),
    envir =  asNamespace("yog")
  )
}
