context("read_json_log")


test_that("read_json_log works as expected", {
  tf <- tempfile()
  lo <- LayoutJson$new(
    logger_vals = "user",
    other_vals = list(pid = Sys.getpid, teststring = "blah")
  )
  lgr <- Logger$new(
    "test",
    appenders = AppenderFile$new(layout = lo, file = tf),
    threshold = NA,
    parent = NULL
  )

  lgr$fatal("test")
  lgr$error("test")
  lgr$warn("test")
  lgr$info("test")
  lgr$debug("test")
  lgr$trace("test")


  expect_true(all(read_json_log(tf)$level == seq(100, 600, by = 100)))
  file.remove(tf)
})
