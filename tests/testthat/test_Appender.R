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

  app$set_threshold(200)
  expect_identical(app$threshold, 200L)
  app$set_threshold("info")
  expect_identical(app$threshold, 400L)
  app$set_threshold(NA)
  expect_identical(app$threshold, NA_integer_)
  expect_error(app$set_threshold("blubb"), "log levels")
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
  tres <- read_json_lines(tf)
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

  expect_match(paste(capture.output(app$show()), collapse = ""), "ERROR.*WARN")
})




# AppenderDbi -------------------------------------------------------------

test_that("AppenderDbi: appending multiple rows works", {
  if (!requireNamespace("RSQLite", quietly = TRUE))
    skip("Test requires RSQLite")

  tname <- "LOGGING_TEST"

  app <- expect_message(
    AppenderDbi$new(
      conn = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
      table = tname
    ),
    "new"
  )

  e <- LogEvent$new(yog, level = 600, msg = "ohno", caller = "nope()", timestamp = Sys.time())

  expect_silent(app$append(e))
  expect_silent(app$append(e))
  tres <- app$data
  expect_identical(nrow(tres), 2L)


  # test vectorized logging
  e$msg <- rep("ohyeah", 3)
  expect_silent(app$append(e))
  tres <- app$data
  expect_identical(tres$msg, c(rep("ohno", 2), rep("ohyeah", 3)))


  # test show method
  e$msg <- rep(1:20, 3)
  expect_silent(app$append(e))
  expect_output(
    app$show(5),
    paste(16:20, collapse = ".*")
  )

  expect_silent(app$append(e))
})





test_that("AppenderDbi: log access works", {
  if (!requireNamespace("RSQLite", quietly = TRUE))
    skip("Test requires RSQLite")

  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  tname <- "LOGGING_TEST"

  expect_message(
    lg <- Logger$new(
      "test_dbi",
      threshold = "trace",
      appenders = list(db = AppenderDbi$new(conn = conn, table = tname, close_on_exit = FALSE))
    ),
    "new"
  )

  expect_output({
    lg$fatal("blubb")
    lg$trace("blah")
  })

  expect_output(lg$appenders$db$show(), "FATAL.*TRACE")
  expect_output(
    expect_identical(nrow(lg$appenders$db$show(1)), 1L),
    "TRACE"
  )

  expect_identical(nrow(lg$appenders$db$data), 2L)

  expect_output(
    expect_identical(
      show_log(target = lg),
      lg$appenders$db$show()
    )
  )

  expect_silent(DBI::dbDisconnect(conn))
})



test_that("AppenderDbi: Automatic closing of connections works", {
  if (!requireNamespace("RSQLite", quietly = TRUE))
    skip("Test requires RSQLite")

  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  tname <- "LOGGING_TEST"

  # Autoclosing works
  expect_message(
    lg <- Logger$new(
      "test_dbi",
      threshold = "trace",
      appenders = list(db = AppenderDbi$new(conn = conn, table = tname, close_on_exit = TRUE))
    ),
    "new"
  )
  rm(lg)
  gc()
  expect_warning(DBI::dbDisconnect(conn), "Already disconnected")


  # Suppressing autoclose works
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  tname <- "LOGGING_TEST"
  expect_message(
    lg <- Logger$new(
      "test_dbi",
      threshold = "trace",
      appenders = list(db = AppenderDbi$new(conn = conn, table = tname, close_on_exit = FALSE))
    ),
    "new"
  )
  rm(lg)
  gc()
  expect_silent(DBI::dbDisconnect(conn))
})




# AppenderRjdbc -------------------------------------------------------------

test_that("AppenderRjdbc: appending multiple rows works", {
  if (!requireNamespace("dataSTAT", quietly = TRUE))
    skip("Currently only tested at work")

  tname <- "TMP.LOGGING_TEST"
  conn <- dataSTAT::dbConnectDB2("RTEST", "rtest", "rtest")
  try(DBI::dbRemoveTable(conn, tname), silent = TRUE)

  app <- expect_message(
    AppenderRjdbc$new(conn = conn, table = tname),
    "new"
  )

  expect_message(
    app <- AppenderRjdbc$new(conn = conn, table = tname),
    "existing"
  )

  e <- LogEvent$new(yog, level = 600, msg = "ohno", caller = "nope()", timestamp = Sys.time())

  expect_silent(app$append(e))
  expect_silent(app$append(e))
  tres <- DBI::dbGetQuery(conn, sprintf("select * from %s", tname))
  expect_identical(nrow(tres), 2L)


  # test vectorized logging
  e$msg <- rep("ohyeah", 3)
  expect_silent(app$append(e))
  tres <- app$data
  expect_identical(tres$msg, c(rep("ohno", 2), rep("ohyeah", 3)))


  # test show method
  e$msg <- rep(1:20, 3)
  expect_silent(app$append(e))
  expect_output(
    app$show(5),
    paste(16:20, collapse = ".*")
  )

  expect_silent(app$append(e))
  DBI::dbDisconnect(conn)
})





test_that("AppenderRjdbc: log access works", {

  if (!requireNamespace("dataSTAT", quietly = TRUE))
    skip("Currently only tested at work")

  tname <- "TMP.LOGGING_TEST"
  conn <- dataSTAT::dbConnectDB2("RTEST", "rtest", "rtest")
  try(DBI::dbRemoveTable(conn, tname), silent = TRUE)

  expect_message(
    lg <- Logger$new(
      "test_rjdbc",
      threshold = "trace",
      appenders = list(db = AppenderRjdbc$new(conn = conn, table = tname))
    ),
    "new"
  )


  expect_output({
    lg$fatal("blubb")
    lg$trace("blah")
  })


  expect_output(lg$appenders$db$show(), "FATAL.*TRACE")
  expect_output(
    expect_identical(nrow(lg$appenders$db$show(1)), 1L),
    "TRACE"
  )

  expect_identical(nrow(lg$appenders$db$data), 2L)

  expect_output(
    expect_identical(
      show_log(target = lg),
      lg$appenders$db$show()
    )
  )

  DBI::dbDisconnect(conn)
})




# AppenderBuffer ----------------------------------------------------

test_that("AppenderBuffer behaves as expected", {
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
  eres <- readLines(tf)
  expect_match(paste(eres, collapse = "#"), "(.*z.*){10}(.*y.*){6}")


  # memory cycling without flushing also works
  l$appenders$buffer$set_flush_on_rotate(FALSE)
  for (i in 1:15) l$info(i)
  expect_identical(length(l$appenders$buffer$buffered_events), 10L)
  msgs <- sapply(l$appenders$buffer$buffered_events, `[[`, "msg")
  expect_identical(msgs, as.character(6:15))
  # Nothing should have been flushed to the log file
  expect_identical(readLines(tf), eres)


  # does flushing on object destruction work?
  l$appenders$buffer$flush()  # ensure empty appender
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
  l$appenders$buffer$set_flush_on_exit(FALSE)
  rm(l)
  gc()
  expect_true(!file.exists(tf))
  suppressWarnings(file.remove(tf))
})




test_that("AppenderMemory: memory cycling works", {
  app1 <- AppenderMemoryDt$new(buffer_size = 10)
  replicate(12, app1$append(x))
  expect_equal(app1$data$.id, 3:12)
  r1 <- app1$data

  # bulk insert behaves like sepparate inserts
  app2 <- AppenderMemoryDt$new(buffer_size = 10)
  y <- x$clone()
  y$msg <- rep(y$msg, 12)

  app2$append(y)
  expect_equal(app2$data$.id,  3:12)
  expect_equal(app2$data, r1)
})




test_that("Appender: filters work", {
  app1 <- AppenderConsole$new()
  expect_true(app1$filter(x))
  app1$set_threshold(100)
  expect_false(app1$filter(x))
})
