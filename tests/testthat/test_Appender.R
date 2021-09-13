context("Appender")


x <- LogEvent$new(
  logger = Logger$new("dummy"),
  level = 200L,
  timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
  caller = NA_character_,
  msg = "foo bar"
)




# Appender --------------------------------------------------------------------
test_that("Appender: $append() works", {
  app <- Appender$new()
  expect_match(app$append(x), "foo bar")
})




test_that("Appender: $set_threshold() works", {
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
test_that("AppenderFile: logging with LayoutFormat", {
  tf <- tempfile()
  on.exit(unlink(tf))

  app <- AppenderFile$new(file = tf)
  app$append(x)
  app$append(x)
  res <- readLines(tf)
  expect_true(grepl("foo", res[[1]]))
  expect_true(grepl("bar", res[[2]]))
})



test_that("AppenderFile: logging with LayoutJson", {
  tf <- tempfile()
  on.exit(unlink(tf))

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



test_that("AppenderFile: creates empty log file on init", {
  tf <- tempfile()
  on.exit(unlink(tf))

  expect_error(AppenderFile$new(
    file = file.path(tempdir(), "non", "existing", "directory" )
  ))

  AppenderFile$new(file = file.path(tf))
  expect_true(file.exists(tf))
})



test_that("AppenderFile: $show() works", {
  tf <- tempfile()
  on.exit(unlink(tf))

  tf2 <- tempfile()
  on.exit(unlink(tf2), add = TRUE)

  lg <- get_logger("test")$
    set_propagate(FALSE)$
    set_threshold(NA)$
    set_appenders(list(
      char = AppenderFile$new(file = tf),
      num  = AppenderFile$new(file = tf2, layout = LayoutFormat$new("%n %m"))
    ))

  on.exit(get_logger("test", reset = TRUE), add = TRUE)

  lg$info("foo bar")
  lg$info("blah blubb")
  lg$warn("warnwarn")
  lg$debug("bugbug")

  expect_output({
    expect_length(lg$appenders$char$show("warn"), 1)
    expect_length(lg$appenders$char$show("info"), 3)
    expect_length(lg$appenders$char$show("debug"), 4)

    expect_length(lg$appenders$num$show("warn"), 1)
    expect_length(lg$appenders$num$show("info"), 3)
    expect_length(lg$appenders$num$show("debug"), 4)
  })
})



test_that("AppenderFile$data throws an error", {
  tf <- tempfile()
  on.exit(unlink(tf))

  lg <- get_logger("test")$
    set_propagate(FALSE)$
    set_threshold(NA)$
    set_appenders(list(file = AppenderFile$new(file = tf)))

  on.exit(get_logger("test", reset = TRUE), add = TRUE)

  lg$info("foo bar")
  lg$info("blah blubb")

  expect_error(lg$appenders$file$data, class = "CannotParseLogError")
})


# AppenderJson ------------------------------------------------------------

test_that("AppenderJson: AppenderFile with LayoutJson$show() and $data() work", {
  tf <- tempfile()
  on.exit(unlink(tf))

  # with default format
  app <- AppenderFile$new(file = tf, layout = LayoutJson$new())

  for (i in 1:10)
    app$append(x)

  # show shows the correct number of lines
  r <- utils::capture.output(app$show(n = 3))
  expect_true(grepl( "(level.*){3}", paste(r, collapse = "\n")))
  expect_false(grepl("(level.*){4}", paste(r, collapse = "\n")))

  r <- utils::capture.output(app$show(threshold = 100))
  expect_identical(r, "")

  expect_identical(nrow(app$data), 10L)
})


# AppenderConsole ---------------------------------------------------------

test_that("AppenderConsole: $append() works", {
  app <- AppenderConsole$new()
  expect_match(
    capture.output(app$append(x)),
    "ERROR.*:19:33.*foo.*bar"
  )
})




test_that("AppenderConsole: $filter() works", {
  app1 <- AppenderConsole$new()
  expect_true(app1$filter(x))
  app1$set_filters(list(function(event) FALSE))
  expect_false(app1$filter(x))
})




# AppenderBuffer ----------------------------------------------------
# Tests must be executed in sequence
# setup
  buffer_log <- tempfile()
  teardown(unlink(buffer_log))
  l <- Logger$new(
    "dummy",
    appenders = list(
      buffer = AppenderBuffer$new(
        appenders = list(file = AppenderFile$new(file = buffer_log)),
        buffer_size = 10,
        flush_threshold = "fatal"
      )
    ), propagate = FALSE
  )


test_that("AppenderBuffer: FATAL log level triggers flush", {
  l$info(LETTERS[1:3])
  expect_length(l$appenders$buffer$buffer_events, 1)
  l$info(LETTERS[4:7])
  expect_identical(length(l$appenders$buffer$buffer_events), 2L)

  # FATAL triggers flush with default filters
  l$fatal(letters[1:3])
  expect_identical(l$appenders$buffer$buffer_events, event_list())
  expect_match(
    paste(readLines(buffer_log), collapse = "#"),
    "INFO.*A#INFO.*B#INFO.*C#INFO.*D#INFO.*E#INFO.*F#INFO.*G#FATAL.*a#FATAL.*b#FATAL.*c"
  )

  # Does the next flush flush the correct event?
  l$fatal("x")
  expect_identical(length(readLines(buffer_log)), 11L)
  expect_identical(l$appenders$buffer$buffer_events, event_list())
  expect_match(paste(readLines(buffer_log), collapse = "#"), ".*A#.*B#.*C#.*a#.*b#.*c#.*x")
})




test_that("AppenderBuffer: buffer cycling triggers flush", {
  replicate(10, l$info("z"))
  expect_identical(length(l$appenders$buffer$buffer_events), 10L)
  l$info(c("y", "y", "y"))
  expect_identical(length(readLines(buffer_log)), 24L)
  expect_identical(l$appenders$buffer$buffer_events, event_list())
  expect_match(
    paste(readLines(buffer_log), collapse = "#"),
    ".*A#.*B#.*C#.*a#.*b#.*c#.*x(.*z.*){10}(.*y.*){3}"
  )
})




test_that("AppenderBuffer: manual flush trigger works", {
  l$info(c("y", "y", "y"))
  l$appenders$buffer$flush()
  expect_identical(length(readLines(buffer_log)), 27L)
  eres <- readLines(buffer_log)
  expect_match(paste(eres, collapse = "#"), "(.*z.*){10}(.*y.*){6}")
})




test_that("AppenderBuffer: flush on buffer cycling can be suppressed", {
  eres <- readLines(buffer_log)
  l$appenders$buffer$set_flush_on_rotate(FALSE)
  for (i in 1:15) l$info(i)

  # theres a 10% tolerance for flushing if no flush on rotate is set
  expect_true(length(l$appenders$buffer$buffer_events) >= 10L)
  # Nothing should have been flushed to the log file
  expect_identical(readLines(buffer_log), eres)
})




test_that("AppenderBuffer: object destruction triggers flush", {
  # this test also cleans up behind the logger created at the beginning
  # of this section
  l$appenders$buffer$flush()  # ensure empty appender
  l$info(c("destruction", "destruction"))
  expect_identical(length(l$appenders$buffer$buffer_events), 1L)
  rm(l, inherits = TRUE)
  gc()
  expect_match(
    paste(readLines(buffer_log), collapse = "#"),
    "(.*destruction){2}"
  )
  try(file.remove(buffer_log), silent = TRUE)
})




# the following AppenderBuffer tests are self contained again
test_that("AppenderBuffer: flush on object destruction can be suppressed", {
  # must re-initilaize logger
  buffer_log <- tempfile()
  l <- Logger$new(
    "dummy",
    appenders = list(
      buffer = AppenderBuffer$new(
        appenders = list(file = AppenderFile$new(file = buffer_log)),
        buffer_size = 10
      )
    ),
    propagate = FALSE
  )

  l$info(LETTERS[1:3])
  l$appenders$buffer$set_flush_on_exit(FALSE)
  rm(l)
  gc()
  expect_true(file.exists(buffer_log))
  expect_equal(file.size(buffer_log), 0)

  expect_true(file.remove(buffer_log))
})




test_that("AppenderBuffer: $add_appender()/$remove_appender()", {
  sapp  <- AppenderBuffer$new()
  app1 <- AppenderConsole$new(threshold = 100)
  app2 <- AppenderConsole$new(threshold = 300)

  # add
  expect_silent({
    sapp$add_appender(app1)
    sapp$add_appender(app2, "blah")
    sapp$add_appender(AppenderBuffer$new(), "blubb")
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




test_that("AppenderBuffer: $show()", {
  l <- Logger$new(
    "buffer test",
    appenders = AppenderBuffer$new(should_flush = NULL),
    propagate = FALSE
  )

  l$fatal("foo")
  l$warn("foo", bar = "foo")
  l$info(1:3)
  l$error("and a list column", df = head(iris), env = environment())

  expect_identical(nrow(l$appenders[[1]]$buffer_df), 6L)
  expect_identical(nrow(l$appenders[[1]]$buffer_dt), 6L)
  expect_length(capture.output(l$appenders[[1]]$show(n = 5, threshold = "warn")), 3L)
})



test_that("AppenderBuffer: cycling works", {
  l <- Logger$new(
    "buffer test",
    appenders = AppenderBuffer$new(buffer_size = 3L, flush_on_rotate = FALSE),
    propagate = FALSE
  )

  l$info("test1")
  l$info("test2")
  l$info("test3")
  l$info("test4")
  l$info("test5")

  expect_identical(
    sapply(l$appenders[[1]]$buffer_events, `[[`, "msg"),
    paste0("test", 2:5)
  )
})




test_that("AppenderBuffer: Custom $should_flush works", {
  l <- Logger$new(
    "buffer test",
    appenders = AppenderBuffer$new(),
    propagate = FALSE
  )

  # FALSE
  l$appenders[[1]]$set_should_flush(function(event) FALSE)
  l$fatal("test")
  expect_length(l$appenders[[1]]$buffer_events, 1L)

  # TRUE
  l$appenders[[1]]$set_should_flush(
    function(event) {
      inherits(event, "LogEvent") && inherits(.obj(), "AppenderBuffer")
    }
  )
  l$fatal("test")
  expect_length(l$appenders[[1]]$buffer_events, 0L)

  # Undefined
  l$appenders[[1]]$set_should_flush(function(event) NA)
  expect_warning(l$fatal("test"))
  expect_length(l$appenders[[1]]$buffer_events, 1L)
  l$appenders[[1]]$set_should_flush(function(event) iris)
  expect_warning(l$fatal("test"))
  expect_length(l$appenders[[1]]$buffer_events, 2L)

  # illegal filter
  expect_error(l$appenders[[1]]$set_should_flush(mean))
})




# self contained buffer tests
test_that("AppenderBuffer: buffer_size 0 works as expected", {
  l <- Logger$new(
    "0 buffer test",
    appenders = list(buffer = AppenderBuffer$new()$
      set_appenders(list(file = AppenderFile$new(file = tempfile())))),
    propagate = FALSE
  )
  on.exit(unlink(l$appenders$buffer$appenders$file$file))

  l$appenders$buffer$set_buffer_size(0)
  expect_silent(l$info(LETTERS[1:3]))
  expect_length(readLines(l$appenders$buffer$appenders$file$file), 3L)

  expect_silent({
    expect_identical(nrow(l$appenders$buffer$buffer_df), 0L)
    expect_identical(nrow(l$appenders$buffer$buffer_dt), 0L)
    expect_length(l$appenders$buffer$buffer_events, 0L)
  })
})




# utils -------------------------------------------------------------------

test_that("default_file_reader() works", {
  tf <- tempfile()
  on.exit(unlink(tf))

  expect_error(expect_warning(default_file_reader(tf, threshold = NA, n = 0)))

  writeLines(LETTERS, tf)
  expect_identical(default_file_reader(tf, threshold = NA, n = 3), c("X", "Y", "Z"))

  writeLines(LETTERS, tf)
  expect_warning(default_file_reader(tf, threshold = 4, n = 3))
})




test_that("standardize_should_flush_output() works", {
  expect_identical(standardize_should_flush_output(TRUE), TRUE)
  expect_identical(standardize_should_flush_output(FALSE), FALSE)
  expect_warning(
    expect_identical(standardize_should_flush_output(NA), FALSE),
    class = "ValueIsNotBoolError"
  )
  expect_warning(
    expect_identical(standardize_should_flush_output(iris), FALSE),
    class = "ValueIsNotBoolError"
  )
})
