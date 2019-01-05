lg1 <- Logger$new(
  "Buffer",
  appenders = AppenderBuffer$new(should_flush = NULL, buffer_size = 1e5, flush_on_rotate = FALSE),
  propagate = FALSE
)

lg2 <- Logger$new(
  "dt",
  appenders = AppenderDt$new(buffer_size = 1e5),
  propagate = FALSE
)

bench::mark(
  for (i in 1:1e4) lg1$fatal("blubb"),
  for (i in 1:1e4) lg2$fatal("blubb"),
  check = FALSE

)




lgr$appenders$memory$dt
lgr$appenders[[2]]$dt
