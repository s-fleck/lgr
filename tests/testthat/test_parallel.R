



for (strategy in c(
  "sequential",
  "transparent",
  "multisession",
  "multicore",
  "multiprocess",
  "cluster"
)){
  context(sprintf("future plan = '%s'", strategy))
  test_that(paste0(strategy, ": Logging works"), {
    skip_if_not_installed("future")
    skip_if_not_installed("future.apply")
    future::plan(strategy)

    tf <- tempfile()

    lr <- Logger$new("par_root", parent = NULL, appenders = AppenderFile$new(tf))
    lg <- lr$spawn("par_child")

    x <- future::future(lr$info("root_logger"))
    x <- future::value(x)
    expect_match(readLines(tf)[[1]], "root_logger")

    y <- future::future(lg$info("child_logger"))
    y <- future::value(y)
    expect_match(readLines(tf)[[2]], "child_logger")

    future.apply::future_lapply(
      c("flapply 1", "flapply 2"),
      function(.x) lr$info("root %s", .x, pid = Sys.getpid())
    )
    expect_match(readLines(tf)[[3]], "root.*flapply 1")
    expect_match(readLines(tf)[[4]], "root.*flapply 2")

    future.apply::future_lapply(
      c("flapply 1", "flapply 2"),
      function(.x) lr$info("child %s", .x)
    )
    expect_match(readLines(tf)[[5]], "child.*flapply 1")
    expect_match(readLines(tf)[[6]], "child.*flapply 2")
    unlink(tf)
  })


}

