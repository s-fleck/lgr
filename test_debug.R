#!/usr/bin/env Rscript
# Debug script to test logging config

# Load the lgr package
devtools::load_all(".")

# Test 1: root logger can apply multi-logger configs
cat("\n=== Test 1: root logger with multi-logger configs ===\n")

cfg <- list(
    version = 1,
    disable_existing_loggers = TRUE,
    layouts = list(
        plain = list(class = "LayoutFormat", fmt = "%L %m")
    ),
    appenders = list(
        console_info = list(
            class = "AppenderConsole",
            threshold = "info",
            layout = "plain"
        )
    ),
    filters = list(
        force_warn = list(class = "FilterForceLevel", level = "warn")
    ),
    loggers = list(
        root = list(threshold = "info", appenders = "console_info"),
        "pkg/sub" = list(propagate = FALSE, filters = "force_warn")
    )
)

cat("Config created. Applying to lgr...\n")
lgr$config(cfg)

cat("lgr$threshold = ", lgr$threshold, " (expected 300)\n")
cat("lgr$appenders length = ", length(lgr$appenders), "\n")

sub <- get_logger("pkg/sub")
cat("pkg/sub$propagate = ", sub$propagate, " (expected FALSE)\n")
cat("pkg/sub$filters length = ", length(sub$filters), "\n")

# Test 2: legacy logger config
cat("\n=== Test 2: legacy logger config ===\n")

cfg2 <- list(
    threshold = "error",
    appenders = list(
        AppenderConsole = list(threshold = "info")
    ),
    propagate = FALSE
)

cat("Config created. Applying to legacy logger...\n")
lg <- get_logger("legacy")
lg$config(cfg2)

cat("legacy$threshold = ", lg$threshold, " (expected 400)\n")
cat(
    "legacy$appenders[[1]]$threshold = ",
    lg$appenders[[1]]$threshold,
    " (expected 300)\n"
)
cat("legacy$propagate = ", lg$propagate, " (expected FALSE)\n")

cat("\n=== Tests complete ===\n")
