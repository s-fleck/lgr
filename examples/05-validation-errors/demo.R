# Validation: strict schema checking, path-addressed errors, did-you-mean
# suggestions, cycle/dangling-reference detection, and atomicity (a failed
# config never touches the logger hierarchy).
#
# Run interactively. For each file, read the printed error and check it
# against the # EXPECT comment.

library(lgr)

dir <- file.path("examples", "05-validation-errors")

show_error <- function(file, allow_code = FALSE) {
  cat("\n---", file, "---\n")
  threshold_before <- lgr$threshold
  appenders_before <- length(lgr$appenders)
  res <- try(lgr$config(file.path(dir, file), allow_code = allow_code), silent = TRUE)
  if (inherits(res, "try-error")) {
    cat(conditionMessage(attr(res, "condition")), "\n")
  } else {
    cat("(no error - unexpected!)\n")
  }
  cat("hierarchy still untouched:",
      identical(lgr$threshold, threshold_before) && identical(length(lgr$appenders), appenders_before),
      "\n")  # EXPECT: TRUE
}

show_error("bad-unknown-key.yaml")
# EXPECT: error names the unknown key `propogate` and its path (loggers.root)

show_error("bad-wrong-type.yaml")
# EXPECT: error says `disable_existing_loggers` should be a boolean

show_error("bad-dangling-ref.yaml")
# EXPECT: error names the missing appender reference `consle`, its path
# (loggers.root.appenders[1]), and suggests `console`

show_error("bad-cycle.yaml")
# EXPECT: error names the cyclic filters `a` and `b`

show_error("bad-unsupported-version.yaml")
# EXPECT: error says only version 1 is supported

# --- use validate_config()/read_config() to check without applying --------
cat("\n--- validate_config() without touching the hierarchy at all ---\n")
res <- try(validate_config(file.path(dir, "bad-unknown-key.yaml")), silent = TRUE)
cat(conditionMessage(attr(res, "condition")), "\n")
cat("this never even calls lgr$config(), so nothing to reset\n")
