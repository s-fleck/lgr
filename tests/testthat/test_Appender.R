context("appenders")

x <- LogEvent$new(logger = Logger$new("dummy"))

{
  x[["level"]] <- 200L
  x[["timestamp"]] <- structure(1541175573.9308, class = c("POSIXct", "POSIXt"))
  x[["caller"]] <- NA_character_
  x[["msg"]] <- "foo bar"
}



test_that("dummy Appender works as expected", {
  app <- Appender$new()
  expect_match(app$append(x), "foo bar")
})




test_that("dummy AppenderFormat works as expected", {
  app <- AppenderFormat$new()
  expect_match(app$append(x), "ERROR \\[2018-11-02 17:19:33.*\\] foo bar")
})




test_that("AppenderFile works as expected", {
  tf <- tempfile()

  # with default format
  app <- AppenderFile$new(file = tf)
  app$append(x)
  app$append(x)
  res <- readLines(tf)
  expect_true(grepl("foo", res[[1]]))
  expect_true(grepl("bar", res[[2]]))


  tf <- tempfile()
  # with Json
  app <- AppenderFile$new(file = tf, layout = LayoutJson$new())
  app$append(x)
  app$append(x)
  tres <- read_json_log(tf)
  eres <- data.table::rbindlist(
    list(x$values, x$values)
  )

  expect_identical(tres[["level"]], eres[["level"]])
  expect_identical(tres[["msg"]], eres[["msg"]])
  expect_true(all(is.na(tres[["caller"]])) && all(is.na(eres[["caller"]])))
  expect_equal(as.POSIXct(tres[["timestamp"]]), eres[["timestamp"]], tolerance = 1)
})




test_that("AppenderConsole works as expected", {
  app <- AppenderConsole$new()
  expect_match(
    capture.output(app$append(x)),
    "ERROR \\[17:19:33.*\\] foo bar"
  )
})




# AppenderMemory ----------------------------------------------------------

test_that("AppenderMemory: appending multiple rows works", {
  app <- AppenderMemoryDt$new()

  y <- x$clone()
  y$level <- seq(100L, 300L, 100L)

  expect_silent(app$append(y))
  expect_identical(app$data[1:3]$level, y$level)
  expect_identical(app$data[1:3]$timestamp, rep(y$timestamp, 3))
  expect_identical(app$data[1:3]$msg, rep(y$msg, 3))
  expect_identical(app$data[1:3]$caller, rep(NA_character_, 3))

  y <- x$clone()
  y$level <- 300
  app$append(y)
  expect_identical(app$.__enclos_env__$private$.data$.id[1:4], 1:4)
  app$logger <- yog  # so that log levels can be labelled

  expect_match(paste(capture.output(app$show()), collapse = ""), "ERROR.*WARN")
})




# AppenderMemoryBuffer ----------------------------------------------------

test_that("AppenderMemoryBufferDt: appending multiple rows works", {
  app <- AppenderMemoryBufferDt$new(

  )

  y <- x$clone()
  y$level <- seq(100L, 300L, 100L)

  expect_silent(app$append(y))
  expect_identical(app$data[1:3]$level, y$level)
  expect_identical(app$data[1:3]$timestamp, rep(y$timestamp, 3))
  expect_identical(app$data[1:3]$msg, rep(y$msg, 3))
  expect_identical(app$data[1:3]$caller, rep(NA_character_, 3))

  y <- x$clone()
  y$level <- 300
  app$append(y)
  expect_identical(app$.__enclos_env__$private$.data$.id[1:4], 1:4)
  app$logger <- yog  # so that log levels can be labelled

  expect_match(paste(capture.output(app$show()), collapse = ""), "ERROR.*WARN")
})




test_that("AppenderMemory: memory cycling works", {
  app1 <- AppenderMemoryDt$new(cache_size = 10)

  replicate(12, app1$append(x))
  expect_equal(app1$data$.id, 3:12)
  r1 <- app1$data


  # bulk insert behaves like sepparate inserts
  app2 <- AppenderMemoryDt$new(cache_size = 10)

  y <- x$clone()
  y$msg <- rep(y$msg, 12)

  app2$append(y)
  expect_equal(app2$data$.id,  3:12)
  expect_identical(app2$data, r1)
})




test_that("Appender: filters work", {
  app1 <- AppenderConsole$new()
  expect_true(app1$filter(x))
  app1$threshold <-  100
  expect_false(app1$filter(x))
})




test_that("AppenderRotating works as expected", {
  td <- tempdir()
  tf <- file.path(td, "logtest")

  # with default format
  app <- AppenderRotating$new(file = tf)
  app$append(x)
  app$do_rollover()

  for (i in 1:15){
    app$append(x)
    app$do_rollover()
  }

  logfiles <- list.files(dirname(tf), pattern = basename(tf))
  res <- sapply(strsplit(logfiles, ".", fixed = TRUE), function(.x) .x[[length(.x)]])
  expect_identical(sort(as.integer(res)), 1:16)

  # pruning retains the x newest files
  app$prune_backups(3)
  logfiles <- list.files(dirname(tf), pattern = basename(tf))
  res <- sapply(strsplit(logfiles, ".", fixed = TRUE), function(.x) .x[[length(.x)]])
  expect_identical(sort(as.integer(res)), 1:3)

  # pruning to the exact number of files does nothing
  app$prune_backups(3)
  logfiles <- list.files(dirname(tf), pattern = basename(tf))
  res <- sapply(strsplit(logfiles, ".", fixed = TRUE), function(.x) .x[[length(.x)]])
  expect_identical(sort(as.integer(res)), 1:3)

  file.remove(list.files(td, pattern = "logtest", full.names = TRUE))
})



test_that("AppenderRotating works as expected wiht compress option enabled", {
  td <- tempdir()
  tf <- file.path(td, "logtest")

  # with default format
  app <- AppenderRotating$new(file = tf, compress = TRUE)
  app$append(x)
  app$do_rollover()

  for (i in 1:8){
    app$append(x)
    app$do_rollover()
  }

  logfiles <- list.files(dirname(tf), pattern = basename(tf))
  res <- get_backup_index(logfiles)
  expect_identical(sort(as.integer(res)), 1:9)

  for (i in 1:5){
    app$append(x)
    app$do_rollover()
  }

  logfiles <- list.files(dirname(tf), pattern = basename(tf))
  res <- get_backup_index(logfiles)
  expect_identical(sort(res), c(paste0("0", 1:9), 10:14))

  # pruning retains the x newest files
  app$prune_backups(3)
  logfiles <- list.files(dirname(tf), pattern = basename(tf))
  res <- get_backup_index(logfiles)
  expect_identical(sort(as.integer(res)), 1:3)

  # pruning to the exact number of files does nothing
  app$prune_backups(3)
  logfiles <- list.files(dirname(tf), pattern = basename(tf))
  res <- get_backup_index(logfiles)
  expect_identical(sort(as.integer(res)), 1:3)

  file.remove(list.files(td, pattern = "logtest", full.names = TRUE))
})