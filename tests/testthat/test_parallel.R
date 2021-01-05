for (strategy in c(
  "sequential",
  "multicore"
  # "multisession",
  # "cluster"
)){
  context(sprintf("future plan = '%s'", strategy))

  if (!future::availableCores("multicore") > 1L){
    skip("'multicore' not supported on system")
  }


  test_that(paste0(strategy, ": Logging works"), {
    skip_if_not_installed("future")
    skip_if_not_installed("future.apply")
    future::plan(strategy)

    tf <- tempfile()
    on.exit(unlink(tf))

    lr <- get_logger("par_root")
    lr$
      set_appenders(AppenderFile$new(tf))$
      set_propagate(FALSE)


    lg <- get_logger("par_root/par_child")

    x <- future::future(lr$info("root_logger"))
    expect_silent(x <- future::value(x))
    expect_match(readLines(tf)[[1]], "root_logger")

    y <- future::future(lg$info("child_logger"))
    expect_silent(y <- future::value(y))
    expect_match(readLines(tf)[[2]], "child_logger")

    future.apply::future_lapply(
      c("flapply 1", "flapply 2"),
      function(.x) lr$info("root %s", .x, pid = Sys.getpid())
    )

    res <- readLines(tf)[3:4]
    expect_true(any(grepl("root.*flapply 1", res)))
    expect_true(any(grepl("root.*flapply 2", res)))

    future.apply::future_lapply(
      c("flapply 1", "flapply 2"),
      function(.x) lr$info("child %s", .x)
    )

    res <- readLines(tf)[5:6]
    expect_true(any(grepl("child.*flapply 1", res)))
    expect_true(any(grepl("child.*flapply 2", res)))

    unlink(tf)
  })


}

