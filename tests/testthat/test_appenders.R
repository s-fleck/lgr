context("appenders")


test_that("dummy Appender works as expected", {
  app <- Appender$new()
  x <- as.environment(list(
    level = 200,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  ))
  expect_match(app$append(x), "environment")
})




test_that("dummy AppenderFormat works as expected", {
  app <- AppenderFormat$new()
  x <- as.environment(list(
    level = 200,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  ))
  expect_equal(app$append(x), "ERROR [2018-11-02 17:19:33] foo bar")
})



test_that("AppenderFile works as expected", {
  tf <- tempfile()

  app <- AppenderFile$new(file = tf)
  x <- as.environment(list(
    level = 200,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  ))
  app$append(x)
  app$append(x)
  res <- readLines(tf)

  expect_true(grepl("foo", res[[1]]))
  expect_true(grepl("bar", res[[2]]))
})




test_that("AppenderConsole works as expected", {
  app <- AppenderConsole$new()

  x <- as.environment(list(
    level = 200,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  ))

  expect_identical(
    capture.output(app$append(x)),
    "ERROR [17:19:33] foo bar"
  )
})



# AppenderMemory ----------------------------------------------------------

test_that("memory cycling works", {
  app <- AppenderMemoryDt$new()

  x <- as.environment(list(
    level = 200L,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  ))

  app$append(x)

  expect_identical(app$data[1]$level, x$level)
  expect_identical(app$data[1]$timestamp, x$timestamp)
  expect_identical(app$data[1]$msg, x$msg)
  expect_identical(app$data[1]$caller, NA_character_)

  x$level <- 300
  app$append(x)

  expect_match(paste(capture.output(app$show()), collapse = ""), "ERROR.*WARN")
})




test_that("memory cycling works", {
  app <- AppenderMemoryDt$new(cache_size = 10)

  x <- as.environment(list(
    level = 200L,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  ))

  replicate(12, app$append(x))
  expect_equal(app$data$.id, 3:12)
})

