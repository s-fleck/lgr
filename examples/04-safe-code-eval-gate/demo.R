# Safe-by-default code evaluation: the `code:` form is rejected unless the
# caller opts in with allow_code = TRUE. Every other function-valued form
# (name references, `ref:`, `class:` specs) is always allowed.
#
# Run interactively, one section at a time.

library(lgr)

code_cfg <- file.path("examples", "04-safe-code-eval-gate", "lgr_code.yaml")
ref_cfg  <- file.path("examples", "04-safe-code-eval-gate", "lgr_ref.yaml")

# --- 1. code: form rejected by default -------------------------------------
cat("\n--- code: form, allow_code missing (default FALSE) ---\n")
threshold_before <- lgr$threshold
res <- try(lgr$config(code_cfg), silent = TRUE)
cat("error was raised:", inherits(res, "try-error"), "\n")               # EXPECT: TRUE
cat("error names 'exception_handler':",
    grepl("exception_handler", res, fixed = TRUE), "\n")                  # EXPECT: TRUE
cat("error mentions allow_code:", grepl("allow_code", res, fixed = TRUE), "\n")  # EXPECT: TRUE
cat("hierarchy untouched:", identical(lgr$threshold, threshold_before), "\n")  # EXPECT: TRUE

# --- 2. code: form accepted with allow_code = TRUE -------------------------
cat("\n--- code: form, allow_code = TRUE ---\n")
lgr$config(code_cfg, allow_code = TRUE)
cat("exception_handler is a function:", is.function(lgr$exception_handler), "\n")  # EXPECT: TRUE
lgr$config(NULL)

# --- 3. name/ref forms never need allow_code --------------------------------
cat("\n--- name reference form, allow_code default FALSE ---\n")
lgr$config(ref_cfg)  # no allow_code argument at all
cat("loaded without allow_code:", identical(lgr$exception_handler, default_exception_handler), "\n")  # EXPECT: TRUE
lgr$config(NULL)

# --- 4. a live R function object in a *list* config is always fine ----------
# Per the config-schema/config-loading specs, a live R `function` object
# passed inside a full/declarative `list` config should be accepted
# regardless of `allow_code` (it's a live object, not source to evaluate).
cat("\n--- live function object in a list config, allow_code default FALSE ---\n")
my_handler <- function(e) cat("live handler:", conditionMessage(e), "\n")
res <- try(lgr$config(list(loggers = list(root = list(
  threshold = "all",
  exception_handler = my_handler
)))), silent = TRUE)
if (inherits(res, "try-error")) {
  cat("(!) KNOWN GAP: this currently errors instead of being accepted:\n")
  cat(conditionMessage(attr(res, "condition")), "\n")
} else {
  cat("handler installed without allow_code:", identical(lgr$exception_handler, my_handler), "\n")  # EXPECT: TRUE
}

# --- cleanup -----------------------------------------------------------------
lgr$config(NULL)
