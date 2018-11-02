#' @include format.R
#' @include utils.R
#' @include utils-sfmisc.R
#'
#' @export
Layout <- R6::R6Class(
  "Layout",

  public = list(
    format_event = function() {do.call(private$formatter, opts)}
  ),

  active = list(
    opts = function(value){
      if (missing(value)) return(opts)
      private$.opts <- value
    }
  ),

  private = list(
    formatter,
    .opts
  )
)
