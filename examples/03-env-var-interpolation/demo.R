# Environment variable interpolation: ${VAR}, ${VAR:-default}, type
# coercion, missing-variable errors, and $${...} escaping.
#
# Run interactively, one section at a time.

library(lgr)

cfg_path <- file.path("examples", "03-env-var-interpolation", "lgr.yaml")
log_dir <- tempfile("lgr-demo-")
dir.create(log_dir)

# --- 1. Missing variable (no default) errors, hierarchy untouched ---------
cat("\n--- missing variable without a default ---\n")
Sys.unsetenv(c("LGR_DEMO_LOG_DIR", "LGR_DEMO_BUFFER_SIZE"))
Sys.setenv(LGR_DEMO_LOG_DIR = log_dir)
threshold_before <- lgr$threshold
appenders_before <- length(lgr$appenders)
res <- try(lgr$config(cfg_path), silent = TRUE)
cat("error was raised:", inherits(res, "try-error"), "\n")            # EXPECT: TRUE
cat("error mentions LGR_DEMO_BUFFER_SIZE:",
    grepl("LGR_DEMO_BUFFER_SIZE", res, fixed = TRUE), "\n")            # EXPECT: TRUE
cat("root threshold unchanged:", identical(lgr$threshold, threshold_before), "\n")      # EXPECT: TRUE
cat("root appenders count unchanged:", identical(length(lgr$appenders), appenders_before), "\n")  # EXPECT: TRUE

# --- 2. Default used when var is unset, value used when set ---------------
cat("\n--- ${VAR:-default} ---\n")
Sys.setenv(LGR_DEMO_BUFFER_SIZE = "42")
lgr$config(cfg_path)  # LGR_DEMO_LEVEL still unset -> default "info" applies
cat("root threshold (from default 'info'):", lgr$threshold, "(400)\n")  # EXPECT: 400
lgr$config(NULL)

Sys.setenv(LGR_DEMO_LEVEL = "debug")
lgr$config(cfg_path)
cat("root threshold (from set var 'debug'):", lgr$threshold, "(500)\n")  # EXPECT: 500

# --- 3. Type coercion ------------------------------------------------------
cat("\n--- type coercion ---\n")
buffer_appender <- lgr$appenders[[3]]
cat("buffer_size class:", class(buffer_appender$buffer_size), "\n")     # EXPECT: numeric/integer
cat("buffer_size value:", buffer_appender$buffer_size, "\n")            # EXPECT: 42

# --- 4. Partial interpolation stays a string, file was written ------------
cat("\n--- partial interpolation (file path) ---\n")
file_appender <- lgr$appenders[[2]]
cat("file path:", file_appender$file, "\n")
lgr$info("hello, interpolated config")
cat("log file contents:\n")
cat(readLines(file_appender$file), sep = "\n")

lgr$config(NULL)

# --- 5. Escaping a literal ${...} ------------------------------------------
cat("\n--- $${...} escaping ---\n")
lgr$config(list(loggers = list(root = list(
  threshold = "info",
  appenders = list(console = list(
    class = "AppenderConsole",
    layout = list(class = "LayoutFormat", fmt = "literal: $${NOT_A_VAR} %m")
  ))
))))
lgr$info("check the prefix above")  # EXPECT: line starts with "literal: ${NOT_A_VAR}"

# --- cleanup ----------------------------------------------------------------
lgr$config(NULL)
Sys.unsetenv(c("LGR_DEMO_LOG_DIR", "LGR_DEMO_BUFFER_SIZE", "LGR_DEMO_LEVEL"))
unlink(log_dir, recursive = TRUE)
