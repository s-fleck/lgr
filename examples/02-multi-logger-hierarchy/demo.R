# Named reusable components, descendant loggers, propagate, and
# disable_existing_loggers.
#
# Run interactively, one section at a time.

library(lgr)

# Create an "orphan" logger BEFORE applying the config, to observe what
# disable_existing_loggers does to loggers not mentioned in the config.
orphan <- get_logger("orphan")
orphan$set_threshold("fatal")
cat("orphan threshold before config:", orphan$threshold, "(100 = fatal)\n")

lgr$config(file.path("examples", "02-multi-logger-hierarchy", "lgr.yaml"))

# --- shared appender instances --------------------------------------------
cat("\n--- shared component instances ---\n")
root_buffer <- lgr$appenders[[2]]
app <- get_logger("app")
app_buffer <- app$appenders[[1]]
cat("root's buffer and app's buffer are the SAME object:",
    identical(root_buffer, app_buffer), "\n")  # EXPECT: TRUE

# --- descendant logger config ---------------------------------------------
cat("\n--- app/db ---\n")
app_db <- get_logger("app/db")
cat("app/db threshold:", app_db$threshold, "(NA = all)\n")       # EXPECT: NA
cat("app/db propagate:", app_db$propagate, "\n")                 # EXPECT: FALSE
cat("app/db parent is 'app':", identical(app_db$parent$name, "app"), "\n")

# force_info filter should upgrade a debug event to info before it reaches
# the buffer appender
app_db$debug("this gets force-upgraded to info by the filter")
last_event <- root_buffer$buffer_events[[length(root_buffer$buffer_events)]]
cat("last buffered event level:", last_event$level, "(400 = info)\n")  # EXPECT: 400

# --- disable_existing_loggers ----------------------------------------------
cat("\n--- disable_existing_loggers ---\n")
cat("orphan threshold after config (reset to the generic default):", orphan$threshold, "\n")  # EXPECT: 400 (info)
cat("orphan is a 'virgin' (unconfigured) logger:", is_virgin_Logger("orphan"), "\n")  # EXPECT: TRUE

# --- cleanup ---------------------------------------------------------------
lgr$config(NULL)
app$config(NULL)
app_db$config(NULL)
orphan$config(NULL)
