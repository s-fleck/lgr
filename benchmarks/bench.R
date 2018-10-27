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
