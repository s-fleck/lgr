context("appenders")


test_that("dummy Appender works as expected", {
  app <- Appender$new()
  x <- list(
    level = 2,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  )
  expect_match(app$append(x), "17:19:33")
})




test_that("dummy AppenderFormat works as expected", {
  app <- AppenderFormat$new()
  x <- list(
    level = 2,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  )
  expect_equal(app$append(x), "ERROR [2018-11-02 17:19:33] foo bar")
})



test_that("AppenderFile works as expected", {
  tf <- tempfile()

  app <- AppenderFile$new(file = tf)
  x <- list(
    level = 2,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  )
  app$append(x)
  app$append(x)
  res <- readLines(tf)

  expect_true(grepl("foo", res[[1]]))
  expect_true(grepl("bar", res[[2]]))
})




test_that("AppenderConsole works as expected", {
  app <- AppenderConsole$new()

  x <- list(
    level = 2,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  )

  expect_identical(
    capture.output(app$append(x)),
    "ERROR [2018-11-02 17:19:33] foo bar"
  )
})
