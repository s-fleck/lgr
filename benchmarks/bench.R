library(bench)
library(futile.logger)
library(yog)
library(magrittr)
library(logging)
t2 <- tempfile()

#devtools::install_github("smbache/loggr")


# Compare appenders -------------------------------------------------------

ml <- list()



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

ml[["memory buffer"]] <-
  Logger$new("memory buffer", appenders = AppenderBuffer$new(buffer_size = 1e5), parent = NULL)

ml[["default (no colors)"]] <-
  Logger$new("default (no colors)", appenders = AppenderConsole$new(layout = LayoutFormat$new(colors = NULL)), parent = NULL)

ml[["default (colors)"]] <-
  Logger$new("default (colors)", appenders = AppenderConsole$new(layout = LayoutFormat$new(colors = getOption("yog.colors"))), parent = NULL)



ml$`default (no colors)`$fatal("blubb")
ml$`default (colors)`$fatal("test")

exps <- lapply(names(ml), function(x) bquote(ml[[.(x)]]$warn("blubb")))
names(exps) = names(ml)
exps <- c(exps, alist(flog = flog.fatal("test"), flog_off = flog.trace("test")))
opts <- list(check = FALSE)

sink("/dev/null")
res <- do.call(mark, c(exps, opts))
sink()

dd <- list(res) %>% setNames(Sys.time())

hist <- readRDS("benchmarks/history.rds")
dd <- c(dd, hist)

print(dd[1:2])

saveRDS(dd, "benchmarks/history.rds")
