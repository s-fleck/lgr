#' @keywords internal
#' @importFrom data.table data.table set
"_PACKAGE"




#' @export ml
.onLoad <- function(...){
  if (requireNamespace("colt", quietly = TRUE)){
    options(
      memlog.colors = list(
        "fatal" = function(x) colt::clt_emph2(colt::clt_error(x)),
        "error" = colt::clt_error,
        "warn"  = colt::clt_warning,
        "debug" = colt::clt_chr,
        "trace" = colt::clt_chr
      )
    )
  }

  assign("ml", Memlog$new(), envir =  asNamespace("memlog"))
}
