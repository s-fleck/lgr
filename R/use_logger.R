#' Setup a Simple Logger for a Package
#'
#' This gives you a minimal logger with no appenders that you can use inside
#' your package under the name `lg` (e.g. lg$fatal("test")). `use_logger()`
#' does not modify any files but only prints code for you to copy and paste.
#'
#'
#' @param pkg `character` scalar. Name of the package. The default is to try to
#'   get the Package name automatically using the packages **rprojroot** and
#'   **desc**
#'
#' @return a `character` scalar containing \R code.
#' @export
#'
#' @examples
#' use_logger("testpkg")
#'
use_logger <- function(
  pkg = desc::desc_get("Package", rprojroot::find_package_root_file("DESCRIPTION"))[[1]]
){

code <- sprintf(
'.onLoad <- function(...){
  assign(
    "lg",
    lgr::get_logger("%s"),
    envir = parent.env(environment())
  )
}', pkg
)


msg <- sprintf(
  "Add the following to any R file in your package (usually '%s-package.R' or 'zzz.R'):",
  pkg
)

message("\n", msg, "\n")
cat(code)
invisible(code)

}


