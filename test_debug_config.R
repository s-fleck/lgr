#!/usr/bin/env Rscript
devtools::load_all(".")

cat("\n=== Debug: Check what's happening in config call ===\n")

# Manually check the specified values
cfg <- NULL
file <- system.file("configs/production.yaml", package = "lgr")
text <- NULL
list <- NULL

specified <- c(
    cfg_specified = !is.null(cfg),
    file_specified = !is.null(file),
    text_specified = !is.null(text),
    list_specified = !is.null(list)
)

cat("specified = ", specified, "\n")
cat("sum(specified) = ", sum(specified), "\n")
cat(
    "cfg_specified && any(file_specified, text_specified, list_specified) = ",
    specified["cfg_specified"] &&
        any(specified[c("file_specified", "text_specified", "list_specified")]),
    "\n"
)

cond1 <- sum(specified) > 1
cond2 <- specified["cfg_specified"] &&
    any(specified[c("file_specified", "text_specified", "list_specified")])
cat("cond1 = ", cond1, "\n")
cat("cond2 = ", cond2, "\n")
cat("cond1 || cond2 = ", cond1 || cond2, "\n")

if (cond1 || cond2) {
    cat("ERROR: Condition is TRUE\n")
} else {
    cat("OK: Condition is FALSE\n")
}
