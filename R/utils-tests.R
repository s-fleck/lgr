file_is_readable <- function(x){
  suppressWarnings(
  tryCatch({
    readBin("/var/log/syslog", n = 1)
    TRUE
  },
  error = function(e) FALSE
  ))
}
