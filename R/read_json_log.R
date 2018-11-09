#' Read a json logfile
#'
#' @param file `character` scalar. path to a json logfile (one JSON object per line)
#'
#' @return a `data.table`
#' @export
#'
read_json_log <- function(x){
  assert_namespace("data.table")
  assert_namespace("jsonlite")
  res <- data.table::rbindlist(
    lapply(readr::read_lines(x), function(.x){
      r <- jsonlite::fromJSON(.x)
      r[vapply(r, is.null, logical(1), USE.NAMES = FALSE)] <- NA
      r
    })
  )

  if ("timestamp" %in% names(res)){
    data.table::set(res, NULL, "timestamp", as.POSIXct(res$timestamp))
  }

}
