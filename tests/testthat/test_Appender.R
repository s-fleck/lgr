context("appenders")




x <- LogEvent$new(
  logger = Logger$new("dummy"),
  level = 200L,
  timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
  caller = NA_character_,
  msg = "foo bar"
)




# Appender --------------------------------------------------------------------
test_that("dummy Appender works as expected", {
  app <- Appender$new()
  expect_match(app$append(x), "foo bar")
})




test_that("setting appender threshold works", {
  app <- Appender$new()

  app$threshold <- 200
  expect_identical(app$threshold, 200L)
  app$threshold <- "info"
  expect_identical(app$threshold, 400L)
  app$threshold <- NA
  expect_identical(app$threshold, NA_integer_)
  expect_error(app$threshold <- "blubb", "log levels")
})




# AppenderFile ---------------------------------------------------------
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
  eres <- data.table::rbindlist(list(x$values, x$values))

  expect_identical(tres[["level"]], eres[["level"]])
  expect_identical(tres[["msg"]], eres[["msg"]])
  expect_true(all(is.na(tres[["caller"]])) && all(is.na(eres[["caller"]])))
  expect_equal(as.POSIXct(tres[["timestamp"]]), eres[["timestamp"]], tolerance = 1)
})




# AppenderConsole ---------------------------------------------------------

test_that("AppenderConsole works as expected", {
  app <- AppenderConsole$new()
  expect_match(
    capture.output(app$append(x)),
    "ERROR .*:19:33.* foo bar"
  )
})




# AppenderMemoryDt ----------------------------------------------------------

test_that("AppenderMemoryDt: appending multiple rows works", {
  app <- AppenderMemoryDt$new()
  y <- x$clone()
  y$level <- seq(100L, 300L, 100L)

  expect_silent(app$append(y))


  expect_true(data.table::is.data.table(app$data))
  expect_identical(app$data$level[1:3], y$level)
  expect_identical(app$data$timestamp[1:3], rep(y$timestamp, 3))
  expect_identical(app$data$msg[1:3], rep(y$msg, 3))
  expect_identical(app$data$caller[1:3], rep(NA_character_, 3))

  y <- x$clone()
  y$level <- 300
  app$append(y)
  expect_identical(app$.__enclos_env__$private$.data$.id[1:4], 1:4)
  app$logger <- Logger$new("dummy")  # so that log levels can be labelled

  expect_match(paste(capture.output(app$show()), collapse = ""), "ERROR.*WARN")
})




# AppenderBuffer ----------------------------------------------------

test_that("AppenderBuffer: appending multiple rows works", {
  tf <- tempfile()

  # Sub sub appenders must have a reference to the original logger
  l <- Logger$new(
    "dummy",
    appenders = list(
      buffer = AppenderBuffer$new(
        appenders = list(file = AppenderFile$new(file = tf)),
        buffer_size = 10
      )
    ),
    parent = NULL
  )
  expect_identical(l, l$appenders$buffer$logger)
  l$info(LETTERS[1:3])
  expect_identical(length(l$appenders$buffer$buffered_events), 1L)
  l$info(LETTERS[4:7])
  expect_identical(length(l$appenders$buffer$buffered_events), 2L)


  # FATAL log level triggers flush
  l$fatal(letters[1:3])
  expect_identical(l$appenders$buffer$buffered_events, list())
  expect_match(
    paste(readLines(tf), collapse = "#"),
    "INFO.*A#INFO.*B#INFO.*C#INFO.*D#INFO.*E#INFO.*F#INFO.*G#FATAL.*a#FATAL.*b#FATAL.*c"
  )

  # Does the next flush flush the correct event?
  l$fatal("x")
  expect_identical(length(readLines(tf)), 11L)
  expect_identical(l$appenders$buffer$buffered_events, list())
  expect_match(paste(readLines(tf), collapse = "#"), ".*A#.*B#.*C#.*a#.*b#.*c#.*x")

  # does flushing on memory cycling work?
  replicate(10, l$info("z"))
  expect_identical(length(l$appenders$buffer$buffered_events), 10L)
  l$info(c("y", "y", "y"))
  expect_identical(length(readLines(tf)), 24L)
  expect_identical(l$appenders$buffer$buffered_events, list())
  expect_match(
    paste(readLines(tf), collapse = "#"),
    ".*A#.*B#.*C#.*a#.*b#.*c#.*x(.*z.*){10}(.*y.*){3}"
  )

  # manual flushing works
  l$info(c("y", "y", "y"))
  l$appenders$buffer$flush()
  expect_identical(length(readLines(tf)), 27L)
  expect_match(paste(readLines(tf), collapse = "#"), "(.*z.*){10}(.*y.*){6}")


  # does flushing on object destruction work?
  l$info(c("destruction", "destruction"))
  expect_identical(length(l$appenders$buffer$buffered_events), 1L)
  rm(l)
  gc()
  expect_match(
    paste(readLines(tf), collapse = "#"),
    "(.*destruction){2}"
  )

  # does flushing honor log levels and filters??
  try(file.remove(tf), silent = TRUE)
})




test_that("AppenderBuffer: dont flush on object destruction if switched of", {
  tf <- tempfile()

  # Sub sub appenders must have a reference to the original logger
  l <- Logger$new(
    "dummy",
    appenders = list(
      buffer = AppenderBuffer$new(
        appenders = list(file = AppenderFile$new(file = tf)),
        buffer_size = 10
      )
    ),
    parent = NULL
  )
  l$info(LETTERS[1:3])
  l$appenders$buffer$flush_on_exit <- FALSE
  rm(l)
  gc()
  expect_true(!file.exists(tf))
  suppressWarnings(file.remove(tf))
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
  expect_equal(app2$data, r1)
})




test_that("Appender: filters work", {
  app1 <- AppenderConsole$new()
  expect_true(app1$filter(x))
  app1$threshold <-  100
  expect_false(app1$filter(x))
})




# AppenderRotating --------------------------------------------------------

test_that("AppenderRotating works as expected", {
  skip("experimental")
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
  skip("experimental")
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




# AppenderRotatingDate ----------------------------------------------------

test_that("AppenderRotatingDate works as expected", {
  skip("experimental")

  td <- tempdir()
  tf <- file.path(td, "logtest")
  file.remove(list.files(td, pattern = "logtest", full.names = TRUE))

  d0  <- structure(1539262374.04303, class = c("POSIXct", "POSIXt"))
  d1  <- structure(1542027174.04303, class = c("POSIXct", "POSIXt"))
  d2  <- structure(1544705574.04303, class = c("POSIXct", "POSIXt"))
  d3  <- structure(1547383974.04303, class = c("POSIXct", "POSIXt"))

  # last_rollover paramter is set correclty for new appender
  testthat::with_mock(
    Sys.time = function(...){d0},
    Sys.Date = function(...){as.Date(d0)},
    {
      app <- AppenderRotatingDate$new(file = tf)
      app$append(x)
    }
  )
  expect_equal(app$last_rollover, d0)

  # Monthly rollover is triggered on first append in new month
  testthat::with_mock(
    Sys.time = function(...){d1},
    Sys.Date = function(...){as.Date(d1)},
    app$append(x)
  )
  expect_equal(app$last_rollover, d1)
  expect_true(file.exists(file.path(td, "logtest.2018-M11")))
  expect_identical(length(readLines(tf)), 1L)
  expect_identical(length(readLines(file.path(td, "logtest.2018-M11"))), 1L)


  # correct last_rollover is set for new appender if backups already exist
  testthat::with_mock(
    Sys.time = function(...){d0},
    Sys.Date = function(...){as.Date(d0)},
    {
      app <- AppenderRotatingDate$new(file = tf)
    }
  )
  app <- AppenderRotatingDate$new(file = tf)
  expect_equal(as.Date(app$last_rollover), as.Date("2018-11-01"))


  # trigger rollover
    app$append(x)
    testthat::with_mock(Sys.time = function(...){d2},  app$do_rollover() )
    expect_true(file.exists(file.path(td, "logtest.2018-M12")))
    expect_false(file.exists(file.path(td, "logtest")))

  # abbort rollover if target file exists
  app$append(x)
  expect_error(
    testthat::with_mock(Sys.time = function(...){d1},  app$do_rollover() ),
    "exists"
  )

  # compress
  app$compress <- TRUE

  # compressed backups also dont work if target backup already exists
  expect_error(
    testthat::with_mock(Sys.time = function(...){d1},  app$do_rollover() ),
    "exists"
  )

  # otherwise everything is fine
  app$append(x)
  testthat::with_mock(Sys.time = function(...){d3}, app$do_rollover())
  expect_true(file.exists(file.path(td, "logtest.2019-M01.zip")))
  expect_false(file.exists(file.path(td, "logtest")))

  file.remove(list.files(td, pattern = "logtest", full.names = TRUE))
})
