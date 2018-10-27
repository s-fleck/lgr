library(bench)
library(futile.logger)
library(memlog)
library(logging)

walk <- function(.x, .f, ...){
  for (i in seq_along(.x)){
    .f(.x[[i]], ...)
  }
  invisible(.x)
}


# setup memlog
ml <- memlog$new()



#benchmark
n <- 1e2

res <- mark(
  memlog   = {walk(1:n, function(i) capture.output(ml$info("blah blah blah %s", i)))},
  flog     = {walk(1:n, function(i) capture.output(flog.info("blah blah blah %s", i)))},
  iterations = 10,
  check = FALSE
)


print(ggplot2::autoplot(res))



# Compare appenders -------------------------------------------------------

library(futile.logger)
library(bench)

ml_nul <- memlog$new(appenders = NULL)
ml_min <- memlog$new(appenders = console_appender_slim)
ml_std <- memlog$new(appenders = appender_console$new())
ml_glu <- memlog$new(appenders = appender_console_glue$new())
ml_col <- memlog$new(appenders = console_appender_color)
ml_sus <- memlog$new(appenders = console_appender_color)
ml_sus$suspend()


n <- 1e4

print(Sys.time())
sink("/dev/null")

res <- mark(
  "for" = {for (i in 1:n) NULL},
  "sus" = {for (i in 1:n) ml_sus$fatal("blubb")},
  "nul" = {for (i in 1:n) ml_nul$fatal("blubb")},
  "min" = {for (i in 1:n) ml_min$fatal("blubb")},
  "std" = {for (i in 1:n) ml_std$fatal("blubb")},
  "col" = {for (i in 1:n) ml_col$fatal("blubb")},
  "glu" = {for (i in 1:n) ml_glu$fatal("blubb")},
  "flg" = {for (i in 1:n) flog.info("blubb")},
  iterations = 10
)

sink()
print(Sys.time())
data.table::as.data.table(res[, c(1, 4, 7)])
# 2018-10-27
#    expression   median mem_alloc
# 1:        for 959.41µs    6.76KB
# 2:        sus   2.46ms    8.45KB
# 3:        nul  215.8ms    8.45KB
# 4:        min    356ms    8.45KB
# 5:        std 628.08ms    7.91MB
# 6:        col    1.16s   10.75MB
# 7:        glu    1.15s    1.08MB
# 8:        flg    1.58s   18.59MB

plot(res)

