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


ml[["suspended"]] <- Logger$new(appenders = AppenderConsole$new())
ml[["suspended"]]$suspend()

ml[["no appenders"]] <-
  Logger$new(appenders = NULL)

ml[["memory only"]] <-
  Logger$new(appenders = AppenderMemoryDt$new())

ml[["default (no colors)"]] <-
  Logger$new(appenders = AppenderConsole$new(layout = LayoutFormat$new(colors = NULL)), string_formatter = sprintf)

ml[["default (colors)"]] <-
  Logger$new(appenders = AppenderConsole$new(layout = LayoutFormat$new(colors = colors)))




#ml$min <- Logger$new(appenders = AppenderConsoleMinimal$new(colors = list()))
#ml$min_col <- Logger$new(appenders = AppenderConsoleMinimal$new(colors = colors))
#ml$glu <- Logger$new(appenders = AppenderConsoleGlue$new())



n <- 1e3
print(Sys.time())
sink("/dev/null")
exps <- lapply(
  names(ml),
  function(x) bquote(for (i in 1:n) ml[[.(x)]]$fatal("blubb"))
)
names(exps) = names(ml)
exps <- append(exps, list(flog = quote(for (i in 1:n) flog.fatal("blubb"))))
res <- do.call(mark, c(exps, list(iterations = 10, check = FALSE)))
sink()
print(Sys.time())

data.table::as.data.table(res)[,
  .(
    expression,
    median,
    `median_d%` = round(as.numeric((median[expression == "default (no colors)"] - median) / median) * 100),
    mem_alloc,
    `mem_alloc_d%` = round(as.numeric((mem_alloc[expression == "default (no colors)"] - mem_alloc) / mem_alloc) * 100)
  )
]


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

