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
  expect_match(app$append(x), "ERROR \\[2018-11-02 17:19:33.*\\] foo bar")
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

  expect_match(
    capture.output(app$append(x)),
    "ERROR \\[17:19:33.*\\] foo bar"
  )
})



# AppenderMemory ----------------------------------------------------------

test_that("AppenderMemory: appending multiple rows works", {
  app <- AppenderMemoryDt$new()

  x <- as.environment(list(
    level = c(200L, 300L, 400L),
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  ))

  expect_silent(app$append(x))

  expect_identical(app$data[1:3]$level, x$level)
  expect_identical(app$data[1:3]$timestamp, rep(x$timestamp, 3))
  expect_identical(app$data[1:3]$msg, rep(x$msg, 3))
  expect_identical(app$data[1:3]$caller, rep(NA_character_, 3))

  x$level <- 300
  app$append(x)

  expect_identical(app$.__enclos_env__$private$.data$.id[1:4], 1:4)
  expect_match(paste(capture.output(app$show()), collapse = ""), "ERROR.*WARN")
})




test_that("AppenderMemory: memory cycling works", {
  app1 <- AppenderMemoryDt$new(cache_size = 10)
  x <- as.environment(list(
    level = 200L,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  ))
  replicate(12, app1$append(x))
  expect_equal(app1$data$.id, 3:12)
  r1 <- app1$data


  # bulk insert behaves like sepparate inserts
  app2 <- AppenderMemoryDt$new(cache_size = 10)
  x <- as.environment(list(
    level = rep(200L, 12),
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  ))
  app2$append(x)
  expect_equal(app2$data$.id,  3:12)
  expect_identical(app2$data, r1)
})

