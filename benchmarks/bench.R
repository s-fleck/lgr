library(bench)
library(futile.logger)
library(lgr)
library(magrittr)
library(logging)
library(ggplot2)
library(data.table)

t2 <- tempfile()

# Compare appenders -------------------------------------------------------

# Setup appenders

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

ml[["memory dt"]] <-
  Logger$new("memory only", appenders = AppenderDt$new(), parent = NULL)

ml[["memory buffer"]] <-
  Logger$new("memory buffer", appenders = AppenderBuffer$new(buffer_size = 1e5), parent = NULL)

ml[["default (no colors)"]] <-
  Logger$new("default (no colors)", appenders = AppenderConsole$new(layout = LayoutFormat$new(colors = NULL)), parent = NULL)

ml[["default (colors)"]] <-
  Logger$new("default (colors)", appenders = AppenderConsole$new(layout = LayoutFormat$new(colors = getOption("lgr.colors"))), parent = NULL)


# Setup bencharmks
n <- 100
exps <- lapply(names(ml), function(x){
   bquote({for (i in seq_len(n)) ml[[.(x)]]$warn("blubb")})
})
names(exps) <- names(ml)
exps <- c(
  exps,
  alist(
    flog = {for (i in seq_len(n)) flog.warn("blubb")},
    flog_off = {for (i in seq_len(n)) flog.trace("blubb")}
  )
)
opts <- list(check = FALSE, min_iterations = 10L)

sink("/dev/null")
res <- do.call(mark, c(exps, opts))
sink()
dd <- list(res) %>% setNames(Sys.time())

# print output
hist <- readRDS("benchmarks/history.rds")
dd <- c(dd, hist)
print(dd[1:2])



# plot output
pdat <- lapply(dd[1:4], function(.x) {
  as.data.table(.x)[, .(
    expression = as.character(.x$expression), median, time)]
})
pdat <- data.table::rbindlist(pdat, idcol = "date")
pdat <- tidyr::unnest(pdat)
pdat[, expression := forcats::fct_reorder(expression, median)]


pal <- "Oranges"

p <- ggplot(
  pdat,
  aes(
    x = expression,
    y = as.numeric(time),
    fill = date,
    color = date
  )
) +
  geom_boxplot(outlier.shape = NA) +
  geom_boxplot(outlier.shape = NA, fill = "white", alpha = 0.3) +
  scale_y_continuous(limits = c(0.001, 0.25)) +
  scale_fill_brewer(palette = pal, direction = -1) +
  scale_color_brewer(palette = pal, direction = -1) +
  theme_dark()

plot(p)

saveRDS(dd, "benchmarks/history.rds")



# Detail benchmarks -------------------------------------------------------

# get vs [[ ---------------------------------------------------------------
bench::mark(
  lgr$fatal,
  lgr[["fatal"]],
  get("fatal", lgr),
  get("fatal", envir = lgr),
  get("fatal", e = lgr),
  get("fatal", -1, lgr),
  iterations = 1e6
)

sink("/dev/null")
res <- bench::mark(
  lgr$fatal("test"),
  lgr[["fatal"]]("test"),
  get("fatal", lgr)("test")
)
sink()

print(res)

plot(res)

