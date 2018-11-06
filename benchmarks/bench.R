library(bench)
library(futile.logger)
library(yog)

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
  "suspended", appenders = AppenderConsole$new(), parent = NULL)

ml[["suspended"]]$suspend()

ml[["no appenders"]] <-
  Logger$new("no appenders", appenders = NULL, parent = NULL)

ml[["memory only"]] <-
  Logger$new("memory only", appenders = AppenderMemoryDt$new(), parent = NULL)

ml[["default (no colors)"]] <-
  Logger$new("default (no colors)", appenders = AppenderConsole$new(layout = LayoutFormat$new(colors = NULL)), parent = NULL)

ml[["default (colors)"]] <-
  Logger$new("default (colors)", appenders = AppenderConsole$new(layout = LayoutFormat$new(colors = colors)), parent = NULL)


ml[["logger1"]] <-
  Logger$new("no appenders", appenders = AppenderMemoryDt$new(), parent = NULL)

ml[["logger2"]] <-
  Logger2$new("no appenders", appenders = AppenderMemoryDt2$new(), parent = NULL)

ml[[1]]$fatal("blubb")
ml[[2]]$fatal("blubb")


ml[[1]]$last_event
ml[[2]]$last_event

ls(ml[[1]]$last_event)
ls(ml[[2]]$last_event)

ml[[2]]$last_event$values

n <- 1e3
print(Sys.time())
sink("/dev/null")
exps <- lapply(
  names(ml),
  function(x) bquote(for (i in 1:n) ml[[.(x)]]$fatal("blubb"))
)
names(exps) = names(ml)
#exps <- append(exps, list(flog = quote(for (i in 1:n) flog.fatal("blubb"))))
res <- do.call(mark, c(exps, list(iterations = 15, check = FALSE)))
sink()
print(Sys.time())

print(data.table::as.data.table(res)[,
  .(
    expression,
    median,
    `median_d%` = round(as.numeric((median[expression == "default (no colors)"] - median) / median) * 100),
    mem_alloc,
    `mem_alloc_d%` = round(as.numeric((mem_alloc[expression == "default (no colors)"] - mem_alloc) / mem_alloc) * 100)
  )
])

stop()

# 2018-11-04 13:50:06 CET
#             expression   median mem_alloc
# 1:           suspended   3.11ms   11.69KB
# 2:        no appenders  61.77ms   11.69KB
# 3:         memory only 122.94ms   11.93KB
# 4: default (no colors) 433.12ms    7.92MB
# 5:    default (colors)    1.03s   11.23MB
# 6:                flog    1.63s   20.27MB

res$expression <- factor(res$expression, levels = rev(res$expression))
plot(res)

