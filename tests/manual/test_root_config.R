#!/usr/bin/env Rscript
devtools::load_all(".")

cat("\n=== Test: root logger can apply multi-logger configs ===\n")

old <- get_logger("old/logger")
old$set_threshold("fatal")

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

cat("Before lgr$config:\n")
cat("lgr$threshold = ", lgr$threshold, "\n")

lgr$config(cfg)

cat("\nAfter lgr$config:\n")
cat("lgr$threshold = ", lgr$threshold, " (expected: 400 for 'info')\n")
cat("lgr$appenders length = ", length(lgr$appenders), "\n")

if (length(lgr$appenders) > 0) {
    cat(
        "lgr$appenders[[1]]$threshold = ",
        lgr$appenders[[1]]$threshold,
        " (expected: 300 for 'info')\n"
    )
}

sub <- get_logger("pkg/sub")
cat("pkg/sub$propagate = ", sub$propagate, " (expected: FALSE)\n")
cat("pkg/sub$filters length = ", length(sub$filters), "\n")

if (length(sub$filters) > 0) {
    cat("pkg/sub$filters[[1]] class: ", class(sub$filters[[1]])[1], "\n")
}

lgr$config(NULL)
get_logger("pkg/sub")$config(NULL)
old$config(NULL)

cat("\n=== Test complete ===\n")
