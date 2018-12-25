context("Appender")




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





# AppenderJson ------------------------------------------------------------

test_that("AppenderJson works as expected", {
  tf <- tempfile()

  # with default format
  app <- AppenderJson$new(file = tf)

  for (i in 1:10) app$append(x)
  r <- capture_output(app$show(3))
  expect_true(grepl( "(level.*)[3]", r))
  expect_false(grepl("(level.*)[4]", r))

  r <- capture_output(app$show(threshold = 100))
  expect_identical(r, "")
})



# AppenderConsole ---------------------------------------------------------

test_that("AppenderConsole works as expected", {
  app <- AppenderConsole$new()
  expect_match(
    capture.output(app$append(x)),
    "ERROR .*:19:33.* foo bar"
  )
})





test_that("Appender: filters work", {
  app1 <- AppenderConsole$new()
  expect_true(app1$filter(x))
  app1$set_filters(list(function(event, obj) FALSE))
  expect_false(app1$filter(x))
})




# AppenderDt ----------------------------------------------------------

test_that("AppenderDt: appending multiple rows works", {
  app <- AppenderDt$new()
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



test_that("AppenderDt: works with list columns", {
  app <- AppenderDt$new(
    prototype = data.table::data.table(
      .id = NA_integer_,
      level = NA_integer_,
      timestamp = Sys.Date(),
      msg = NA_character_,
      caller = NA_character_,
      list = list(list())
    )
  )

  e <- LogEvent$new(
    level = 100,
    timestamp = Sys.Date(),
    msg = "blubb",
    caller = "blubb()",
    logger = yog
  )
  app$append(e)
  expect_true(is.na(app$data$list[[1]]))

  e <- LogEvent$new(
    level = 100,
    timestamp = Sys.Date(),
    msg = "blubb",
    caller = "blubb()",
    logger = yog,
    list = environment()
  )
  app$append(e)
  expect_true(is.environment(app$data$list[[2]]))

  e <- LogEvent$new(
    level = c(100L, 100L),
    timestamp = Sys.Date(),
    msg = "blubb",
    caller = "blubb()",
    logger = yog,
    list = iris
  )
  app$append(e)
  expect_true(is.data.frame(app$data$list[[3]]))
  expect_true(is.data.frame(app$data$list[[4]]))

  e <- LogEvent$new(
    level = 100L,
    timestamp = Sys.Date(),
    msg = "blubb",
    caller = "blubb()",
    logger = yog,
    foo = "bar"
  )
  app$append(e)
  expect_false("foo" %in% names(app$data))

  expect_identical(
    sapply(app$data$list, class),
    c("logical", "environment", "data.frame", "data.frame", "logical")
  )
})



test_that("AppenderDt: memory cycling works", {
  app1 <- AppenderDt$new(buffer_size = 10)
  replicate(12, app1$append(x))
  expect_equal(app1$data$.id, 3:12)
  r1 <- app1$data

  # bulk insert behaves like sepparate inserts
  app2 <- AppenderDt$new(buffer_size = 10)
  y <- x$clone()
  y$msg <- rep(y$msg, 12)

  app2$append(y)
  expect_equal(app2$data$.id,  3:12)
  expect_equal(app2$data, r1)
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





test_that("AppenderBuffer: add/remove appenders", {
  sapp  <- AppenderBuffer$new()
  app1 <- AppenderConsole$new(threshold = 100)
  app2 <- AppenderConsole$new(threshold = 300)

  # add
  expect_silent({
    sapp$add_appender(app1)
    sapp$add_appender(app2, "blah")
    sapp$add_appender(AppenderDt$new(), "blubb")
  })
  expect_identical(sapp$appenders[[1]], app1)
  expect_identical(sapp$appenders$blah, app2)

  # remove
  sapp$remove_appender(1)
  expect_length(sapp$appenders, 2L)
  sapp$remove_appender(c("blah", "blubb"))

  # set appenders
  sapp$set_appenders(list(app1, app2))
  expect_length(sapp$appenders, 2)
  sapp$set_appenders(list(app1, app2))
  expect_length(sapp$appenders, 2)
  expect_identical(sapp$appenders[[1]], app1)
  expect_identical(sapp$appenders[[2]], app2)
})





