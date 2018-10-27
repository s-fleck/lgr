context("appenders")


test_that("appenders works as expected", {
  ml <- memlog$new()
  ml$fatal("test")
  x <- ml$showdt()


})




test_that("appender_file works as expected", {
  tf <- tempfile()

  ml <- memlog$new(appenders = appender_file$new(file = tf))
  ml$fatal("foo")
  ml$info("bar")

  res <- readLines(tf)
  expect_true(grepl("foo", res[[1]]))
  expect_true(grepl("bar", res[[2]]))
})




test_that("appender_console_minimal works as expected", {
  ml <- memlog$new(appenders = appender_console$new())
  res <- c(
    capture.output(ml$fatal("foo")),
    capture.output(ml$info("bar"))
  )
  expect_true(grepl("foo", res[[1]]))
  expect_true(grepl("bar", res[[2]]))
})



test_that("appender_console_minimal works as expected", {
  ml <- memlog$new(appenders = appender_console_minimal$new())
  res <- c(
    capture.output(ml$fatal("foo")),
    capture.output(ml$info("bar"))
  )
  expect_true(grepl("foo", res[[1]]))
  expect_true(grepl("bar", res[[2]]))
})
