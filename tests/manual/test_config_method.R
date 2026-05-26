#!/usr/bin/env Rscript
devtools::load_all(".")

cat("\n=== Testing config method with file parameter ===\n")

# Test 1: Check if lgr is a Logger object
cat("Class of lgr: ", class(lgr), "\n")
cat("Methods available:\n")
print(ls(lgr))

# Test 2: Try calling config with file
cat("\n\nTrying lgr$config(file = ...)\n")

config_file <- system.file("configs/production.yaml", package = "lgr")
cat("Config file: ", config_file, "\n")

tryCatch(
    {
        # Try different ways to call
        cat("\nMethod 1: lgr$config(file = config_file)\n")
        lgr$config(file = config_file)
        cat("Success!\n")
    },
    error = function(e) {
        cat("Error: ", conditionMessage(e), "\n")
    }
)

tryCatch(
    {
        cat("\nMethod 2: lgr$config(cfg = NULL)\n")
        lgr$config(cfg = NULL)
        cat("Success!\n")
    },
    error = function(e) {
        cat("Error: ", conditionMessage(e), "\n")
    }
)

cat("\n=== Test complete ===\n")
