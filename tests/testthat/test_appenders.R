context("appenders")




test_that("appender_file works as expected", {
  tf <- tempfile()

  ml <- Memlog$new(appenders = AppenderFile$new(file = tf))
  ml$fatal("foo")
  ml$info("bar")

  res <- readLines(tf)
  expect_true(grepl("foo", res[[1]]))
  expect_true(grepl("bar", res[[2]]))
})




test_that("appender_console_minimal works as expected", {
  ml <- Memlog$new(appenders = AppenderConsole$new())
  res <- c(
    capture.output(ml$fatal("foo")),
    capture.output(ml$info("bar"))
  )
  expect_true(grepl("foo", res[[1]]))
  expect_true(grepl("bar", res[[2]]))
})




test_that("appender_console_minimal works as expected", {
  ml <- Memlog$new(appenders = AppenderConsoleMinimal$new())
  res <- c(
    capture.output(ml$fatal("foo")),
    capture.output(ml$info("bar"))
  )
  expect_true(grepl("foo", res[[1]]))
  expect_true(grepl("bar", res[[2]]))
})



test_that("appenderGlue works as expected", {
  ml  <- Memlog$new(appenders = AppenderGlue$new())
  expect_silent(ml$fatal("foo"))
  expect_match(
    ml$appenders[[1]]$append(),
    "FATAL .* foo"
  )

  ml  <- Memlog$new(appenders = AppenderConsoleGlue$new())
  expect_output(ml$fatal("foo"), "FATAL .* foo")
})
