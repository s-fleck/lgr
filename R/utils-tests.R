file_is_readable <- function(x){
  suppressWarnings(
  tryCatch({
    readBin(x, "raw", n = 1)
    TRUE
  },
  error = function(e) FALSE
  ))
}
