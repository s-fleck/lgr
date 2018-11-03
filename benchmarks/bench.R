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


ml[["no appenders"]] <-
  Logger$new(appenders = NULL)

ml[["default (no colors, unsafe)"]] <-
  Logger$new(appenders = AppenderConsole$new(layout = LayoutFormat$new(colors = NULL)), string_formatter = sprintf)

ml[["default (no colors, safe string formatting)"]] <-
  Logger$new(appenders = AppenderConsole$new(layout = LayoutFormat$new(colors = NULL)), string_formatter = sprintf_safely)

ml[["default (colors)"]] <-
  Logger$new(appenders = AppenderConsole$new(layout = LayoutFormat$new(colors = colors)))



#ml$min <- Logger$new(appenders = AppenderConsoleMinimal$new(colors = list()))
#ml$min_col <- Logger$new(appenders = AppenderConsoleMinimal$new(colors = colors))
#ml$glu <- Logger$new(appenders = AppenderConsoleGlue$new())

ml[["suspended"]] <- Logger$new(appenders = AppenderConsole$new())
ml[["suspended"]]$suspend()

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


# 2018-10-27
#       expression   median mem_alloc
# 1:           for 918.91µs    6.76KB
# 2:           sus   2.52ms    8.45KB
# 3:           nul 215.18ms  470.09KB
# 4:           min  363.6ms  399.12KB
# 5:       min_col 895.61ms     3.7MB
# 6: min_col_nocol 361.83ms  399.12KB
# 7:           std 609.38ms    8.45MB
# 8:           col    1.11s   11.13MB
# 9:           glu    1.11s    1.55MB
# 10:          flg    1.59s   18.94MB

res$expression <- factor(res$expression, levels = rev(res$expression))
plot(res)

