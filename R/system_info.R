# internal helpers used by FilterInjectSystemInfo to compute standard runtime
# metadata.


is_in_docker_container <- function() {
  file.exists("/.dockerenv")
}




get_current_scriptfile <- function() {
  res <- get_current_scriptfile_rstudio()
  if (!is.null(res)) return(res)

  res <- get_current_scriptfile_r()
  if (!is.null(res)) return(res)

  res <- get_current_scriptfile_rscript()
  if (!is.null(res)) return(res)

  get_current_scriptfile_source()
}




get_current_scriptfile_rstudio <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    return(NULL)
  }
  if (!isTRUE(rstudioapi::isAvailable())) {
    return(NULL)
  }

  path <- tryCatch(rstudioapi::getActiveDocumentContext()[["path"]], error = function(e) NULL)

  if (is.null(path) || is_blank(path)) {
    path <- tryCatch(rstudioapi::getSourceEditorContext()[["path"]], error = function(e) NULL)
  }

  if (is.null(path) || is_blank(path)) {
    return(NULL)
  }

  path
}




get_current_scriptfile_r <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  if (!grepl("^R(?:|term)(?:|[.]exe)$", basename(cmd_args[[1L]]), ignore.case = TRUE)) {
    return(NULL)
  }

  cmd_args_trailing <- commandArgs(trailingOnly = TRUE)
  leading_idx <- seq.int(from = 1, length.out = length(cmd_args) - length(cmd_args_trailing))
  cmd_args <- cmd_args[leading_idx]
  file_idx <- c(which(cmd_args == "-f") + 1, which(grepl("^--file=", cmd_args)))
  res <- gsub("^(?:|--file=)(.*)$", "\\1", cmd_args[file_idx])

  # If multiple --file arguments are given, R uses the last one
  res <- utils::tail(res[res != ""], 1)
  if (length(res) > 0) {
    return(res)
  }

  NULL
}




get_current_scriptfile_rscript <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  if (!grepl("^R(?:term|script)(?:|[.]exe)$", basename(cmd_args[[1L]]), ignore.case = TRUE)) {
    return(NULL)
  }

  cmd_args_trailing <- commandArgs(trailingOnly = TRUE)
  leading_idx <- seq.int(from = 1, length.out = length(cmd_args) - length(cmd_args_trailing))
  cmd_args <- cmd_args[leading_idx]
  res <- gsub("^(?:--file=(.*)|.*)$", "\\1", cmd_args)

  res <- utils::tail(res[res != ""], 1)
  if (length(res) > 0) {
    return(res)
  }

  NULL
}




get_current_scriptfile_source <- function() {
  calls <- sys.calls()

  for (call in calls) {
    if (identical(as.character(call[[1]]), "source")) {
      return(normalizePath(call[[2]]))
    }
  }

  NULL
}
