#!/usr/bin/env Rscript
# Debug apply_logging_config

devtools::load_all(".")

cat("\n=== Debug apply_logging_config ===\n")

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

lc <- lgr:::as_logging_config(cfg)
cat("After as_logging_config, root logger config:\n")
print(lc$loggers$root)

cat("\nNow manually trace through apply_logging_config...\n")

# Extract root logger config
lcfg <- lc$loggers$root

cat("Raw lcfg:\n")
print(lcfg)

# Convert to logger_config
lcfg_as_lc <- lgr:::as_logger_config(lcfg)
cat("\nAfter as_logger_config, lcfg_as_lc:\n")
print(class(lcfg_as_lc))
print(lcfg_as_lc)

# Parse it
lcfg_parsed <- lgr:::parse_logger_config(lcfg_as_lc)
cat("\nAfter parse_logger_config, lcfg_parsed:\n")
print(class(lcfg_parsed))
cat("Threshold: ", lcfg_parsed$threshold, "\n")
cat("Appenders:\n")
print(lcfg_parsed$appenders)

# Now apply to root logger
cat("\n\nApplying to root logger...\n")
root_before <- get_logger("root")
cat("Root logger threshold BEFORE: ", root_before$threshold, "\n")

root_before$config(lcfg_parsed)

cat("Root logger threshold AFTER: ", root_before$threshold, "\n")
cat("Root logger appenders:\n")
print(root_before$appenders)
