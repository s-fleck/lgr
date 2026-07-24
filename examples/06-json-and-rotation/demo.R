# JSON config format, AppenderFileRotating, and a LoggerGlue logger.
#
# Run interactively, one section at a time.

library(lgr)

old_wd <- setwd(file.path("examples", "06-json-and-rotation"))
dir.create("logs", showWarnings = FALSE)

lgr$config("lgr.json")

# --- 1. JSON was parsed and applied ----------------------------------------
cat("\n--- JSON config applied ---\n")
cat("number of root appenders:", length(lgr$appenders), "(2)\n")  # EXPECT: 2

# --- 2. Rotating file appender ---------------------------------------------
cat("\n--- rotating file appender ---\n")
rotating <- lgr$appenders[[2]]
cat("class:", class(rotating)[1], "\n")           # EXPECT: AppenderFileRotating
cat("max_backups:", rotating$max_backups, "\n")   # EXPECT: 3

# write enough to trigger at least one rotation (size limit is 1K); silence
# the console appender for this part so the padding lines don't spam the
# terminal
console <- lgr$appenders[[1]]
console$set_threshold("off")
for (i in 1:200) {
  lgr$info(sprintf("padding line %03d - %s", i, strrep("x", 40)))
}
console$set_threshold("info")

log_files <- list.files("logs", pattern = "^rotating-demo")
cat("log files on disk:\n")
print(log_files)  # EXPECT: rotating-demo.log plus rotated backups (rotating-demo.1.log, .2.log, ...)

# --- 3. LoggerGlue ----------------------------------------------------------
cat("\n--- LoggerGlue ---\n")
glue_logger <- get_logger("glue_logger")
cat("is a LoggerGlue:", inherits(glue_logger, "LoggerGlue"), "\n")  # EXPECT: TRUE
name <- "world"
glue_logger$info("hello, {name}!")  # EXPECT: "hello, world!" printed via glue interpolation

# --- cleanup -----------------------------------------------------------------
lgr$config(NULL)
glue_logger$config(NULL)
unlink("logs", recursive = TRUE)
setwd(old_wd)
