library(bench)
library(futile.logger)
library(yog)
library(magrittr)

# Compare appenders -------------------------------------------------------

ml <- list()

colors <- list(
  "fatal" = function(x) colt::clt_emph2(colt::clt_error(x)),
  "error" = colt::clt_error,
  "warn"  = colt::clt_warning,
  "info"  = colt::clt_info,
  "debug" = colt::clt_chr,
  "trace" = colt::clt_chr
)


# all loggers are created as Root loggers for the benchmarks. do not do this
# in real code, loggers should always inherit from root!

ml[["suspended"]] <- Logger$new(
  "suspended",
  threshold = 0,
  appenders = AppenderConsole$new(),
  parent = NULL
)


ml[["no appenders"]] <-
  Logger$new("no appenders", appenders = NULL, parent = NULL)

ml[["memory only"]] <-
  Logger$new("memory only", appenders = AppenderMemoryDt$new(), parent = NULL)

ml[["default (no colors)"]] <-
  Logger$new("default (no colors)", appenders = AppenderConsole$new(layout = LayoutFormat$new(colors = NULL)), parent = NULL)

ml[["default (colors)"]] <-
  Logger$new("default (colors)", appenders = AppenderConsole$new(layout = LayoutFormat$new(colors = colors)), parent = NULL)


ml$`default (no colors)`$fatal("blubb")
ml$`default (colors)`$fatal("test")

exps <- lapply(names(ml), function(x) bquote(ml[[.(x)]]$fatal("blubb")))
names(exps) = names(ml)
exps <- c(exps, alist(flog = flog.fatal("test")))
opts <- list(check = FALSE, min_iterations = 1e3, max_iterations = 1e5)

sink("/dev/null")
res <- do.call(mark, c(exps, opts))
sink()

dd <- list(res) %>% setNames(Sys.time())

hist <- readRDS("benchmarks/history.rds")

dd <- c(dd, hist)

saveRDS(dd, "benchmarks/history.rds")
