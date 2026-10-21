# Basic config loading: file path, YAML string, list - and reset with NULL.
#
# Run interactively, one section at a time.

library(lgr)

# --- 1. Load from a file path -------------------------------------------
cat("\n--- from file path ---\n")
ret <- lgr$config(file.path("examples", "01-basic-yaml-config", "lgr.yaml"))
cat("config() returned the root logger invisibly:", inherits(ret, "Logger"), "\n")

lgr$info("hello from a file-based config")   # EXPECT: printed (threshold = info)
lgr$debug("you should not see this")          # EXPECT: nothing printed (below threshold)

lgr$config(NULL)

# --- 2. Load from an in-memory YAML string -------------------------------
cat("\n--- from a YAML string ---\n")
yaml_string <- "
loggers:
  root:
    threshold: debug
    appenders:
      console:
        class: AppenderConsole
        threshold: debug
"
lgr$config(yaml_string)
lgr$debug("hello from a YAML string config")  # EXPECT: printed (threshold = debug)

lgr$config(NULL)

# --- 3. Load from a plain R list -----------------------------------------
cat("\n--- from a list ---\n")
lgr$config(list(loggers = list(root = list(threshold = "warn"))))
cat("root threshold is now:", lgr$threshold, "(300 = warn)\n")  # EXPECT: 300

lgr$warn("hello from a list config")
# EXPECT: NOTHING is printed. A `list` config is still a full snapshot: since
# this config doesn't mention `appenders`, root ends up with zero appenders,
# not whatever it had before.
cat("number of appenders:", length(lgr$appenders), "(0)\n")  # EXPECT: 0

# --- 4. Reset with NULL ---------------------------------------------------
cat("\n--- reset with NULL ---\n")
lgr$config(NULL)
cat("root threshold after reset:", lgr$threshold, "(400 = info, root's built-in default)\n")  # EXPECT: 400
cat("number of appenders after reset:", length(lgr$appenders), "\n")  # EXPECT: 0 (NOT the same as a fresh R session - see below)

# Note: config(NULL) resets to the *generic* unconfigured Logger state, which
# for the root logger has threshold = info but no appenders. This is
# different from root's state in a brand-new R session (threshold = info,
# ONE default console appender) - restart R if you want that exact state back.
