#' Read a json logfile
#'
#' @param file `character` scalar. path to a json logfile (one JSON object per line)
#'
#' @return a `data.table`
#' @seealso [LayoutJson]
#' @export
#'
read_json_log <- function(file){
  assert_namespace("data.table", "jsonlite")
  con <- file(file, open = "r")
  on.exit(close(con))
  res <- jsonlite::stream_in(con, verbose = FALSE)

  if ("timestamp" %in% names(res)){
    data.table::set(res, NULL, "timestamp", as.POSIXct(res$timestamp))
  }

  res
}
