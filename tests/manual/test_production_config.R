#!/usr/bin/env Rscript
# Test with actual production config files

devtools::load_all(".")

cat("\n=== Testing with production.yaml config ===\n")

config_file <- system.file("configs/production.yaml", package = "lgr")
cat("Config file: ", config_file, "\n")

if (file.exists(config_file)) {
    lgr$config(file = config_file)

    cat("\nRoot logger after config:\n")
    cat("- Threshold: ", lgr$threshold, "\n")
    cat("- Appenders: ", length(lgr$appenders), "\n")
    cat("- Propagate: ", lgr$propagate, "\n")

    mypkg <- get_logger("mypkg")
    cat("\nmypkg logger after config:\n")
    cat("- Threshold: ", mypkg$threshold, "\n")
    cat("- Appenders: ", length(mypkg$appenders), "\n")
    cat("- Propagate: ", mypkg$propagate, "\n")
    cat("- Filters: ", length(mypkg$filters), "\n")

    # Test logging
    cat("\n\nTesting log output with configured appenders:\n")
    lgr$info("This is an info message")
    lgr$warn("This is a warning message")
    lgr$error("This is an error message")

    cat("\nmypkg logger output:\n")
    mypkg$info("Package info message")
    mypkg$warn("Package warning")

    lgr$config(NULL)
    cat("\nConfig reset to NULL\n")
} else {
    cat("Config file not found!\n")
}

cat("\n=== Test complete ===\n")
