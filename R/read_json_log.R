#' Read a JSON logfile
#'
#' @param file `character` scalar. path to a JSON logfile (one JSON object per line)
#' @param ... passed on to [jsonlite::stream_in()]
#' @return a `data.frame`
#' @seealso [LayoutJson]
#' @export
#'
read_json_lines <- function(file, ...){
  assert_namespace("data.table", "jsonlite")
  con <- file(file, open = "r")
  on.exit(close(con))
  res <- jsonlite::stream_in(con, verbose = FALSE, ...)

  if ("timestamp" %in% names(res)){
    res$timestamp <- as.POSIXct(res$timestamp)
  }

  res
}
