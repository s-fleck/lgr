#!/usr/bin/env Rscript
# Debug script with more detailed output

devtools::load_all(".")

cat("\n=== Test 1 Debug: root logger threshold ===\n")

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

cat("Initial lgr$threshold = ", lgr$threshold, "\n")
cat("Calling lgr$config(cfg)...\n")

# Before calling config, let's check what as_logging_config returns
lc <- lgr:::as_logging_config(cfg)
cat("as_logging_config returned:\n")
print(class(lc))
cat("Root logger config:", "\n")
print(lc$loggers$root)

lgr$config(cfg)

cat("After lgr$config(cfg):\n")
cat("lgr$threshold = ", lgr$threshold, "\n")

# Check the root logger config directly
cat("\nDebug: Get the root logger and check its state\n")
root <- get_logger("root")
cat("get_logger('root')$threshold = ", root$threshold, "\n")

cat("\n=== Test 2 Debug: Legacy appenders ===\n")

cfg2 <- list(
    threshold = "error",
    appenders = list(
        AppenderConsole = list(threshold = "info")
    ),
    propagate = FALSE
)

cat("Config for legacy logger:\n")
print(cfg2)

cat("\nCalling as_logger_config on the config...\n")
lc2 <- lgr:::as_logger_config(cfg2)
cat("as_logger_config returned:\n")
print(class(lc2))
print(names(lc2))

cat("\nCalling resolve_r6_ctors on appenders...\n")
resolved <- lgr:::resolve_r6_ctors(lc2$appenders)
cat("Resolved appenders:\n")
print(resolved)
print(class(resolved))
print(lapply(resolved, class))

cat("\nTrying to create the logger config...\n")
lg <- get_logger("legacy")
tryCatch(
    {
        lg$config(cfg2)
        cat("Success!\n")
        cat("legacy$threshold = ", lg$threshold, "\n")
        cat(
            "legacy$appenders[[1]]$threshold = ",
            lg$appenders[[1]]$threshold,
            "\n"
        )
    },
    error = function(e) {
        cat("Error: ", conditionMessage(e), "\n")
    }
)
