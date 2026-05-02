#!/usr/bin/env Rscript
# Test with debug output

devtools::load_all(".")

cat("\n=== Testing config with file ===\n")

config_file <- system.file("configs/production.yaml", package = "lgr")
cat("Config file:", config_file, "\n")

# Add a trace to see which config method is being called
tryCatch(
    {
        lgr$config(file = config_file)
        cat("Success!\n")
    },
    error = function(e) {
        cat("Error:", conditionMessage(e), "\n")
        cat("Trace:\n")
        print(traceback())
    }
)
