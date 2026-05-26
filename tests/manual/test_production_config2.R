#!/usr/bin/env Rscript
devtools::load_all(".")

cat("\n=== Testing production config with logs directory ===\n")

# Create the logs directory
dir.create("logs", showWarnings = FALSE)

config_file <- system.file("configs/production.yaml", package = "lgr")
cat("Config file: ", config_file, "\n")

lgr$config(file = config_file)

cat("\nRoot logger after config:\n")
cat("- Threshold: ", lgr$threshold, " (info=400)\n")
cat("- Appenders: ", length(lgr$appenders), "\n")
cat("- Appender names:", paste(names(lgr$appenders), collapse = ", "), "\n")
cat("- Propagate: ", lgr$propagate, "\n")

mypkg <- get_logger("mypkg")
cat("\nmypkg logger after config:\n")
cat("- Threshold: ", mypkg$threshold, "\n")
cat("- Appenders: ", length(mypkg$appenders), "\n")
cat("- Propagate: ", mypkg$propagate, "\n")
cat("- Filters: ", length(mypkg$filters), "\n")

# Test logging
cat("\n\n=== Testing log output ===\n")
lgr$info("This is an info message")
lgr$warn("This is a warning message")
lgr$error("This is an error message")

cat("\nmypkg logger output:\n")
mypkg$info("Package info message")

# Check if files were created
cat("\n\n=== Checking output files ===\n")
if (file.exists("logs/application.jsonl")) {
    cat("logs/application.jsonl created successfully\n")
    cat("Content (first 3 lines):\n")
    lines <- readLines("logs/application.jsonl", n = 3)
    for (line in lines) {
        cat(line, "\n")
    }
}

# Cleanup
lgr$config(NULL)
cat("\nConfig reset to NULL\n")

cat("\n=== Test complete ===\n")
