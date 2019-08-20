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




# AppenderJson ------------------------------------------------------------

test_that("AppenderJson: $show() works", {
  tf <- tempfile()
  on.exit(unlink(tf))

  # with default format
  app <- AppenderJson$new(file = tf)

  for (i in 1:10)
    app$append(x)

  # show shows the correct number of lines
  r <- utils::capture.output(app$show(n = 3))
  expect_true(grepl( "(level.*){3}", paste(r, collapse = "\n")))
  expect_false(grepl("(level.*){4}", paste(r, collapse = "\n")))

  r <- utils::capture.output(app$show(threshold = 100))
  expect_identical(r, "")
})




# AppenderConsole ---------------------------------------------------------

test_that("AppenderConsole: $append() works", {
  app <- AppenderConsole$new()
  expect_match(
    capture.output(app$append(x)),
    "ERROR .*:19:33.* foo bar"
  )
})




test_that("AppenderConsole: $filter() works", {
  app1 <- AppenderConsole$new()
  expect_true(app1$filter(x))
  app1$set_filters(list(function(event) FALSE))
  expect_false(app1$filter(x))
})




# AppenderDt ----------------------------------------------------------

test_that("AppenderDt: appending multiple rows works", {
  app <- AppenderDt$new()
  y <- x$clone()
  y$level <- seq(100L, 300L, 100L)

  expect_silent(app$append(y))

  expect_true(data.table::is.data.table(app$dt))
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
    logger = lgr
  )
  app$append(e)
  expect_true(is.null(app$data$list[[1]]))

  e <- LogEvent$new(
    level = 100,
    timestamp = Sys.Date(),
    msg = "blubb",
    caller = "blubb()",
    logger = lgr,
    list = environment()
  )
  app$append(e)
  expect_true(is.environment(app$data$list[[2]]))

  e <- LogEvent$new(
    level = c(100L, 100L),
    timestamp = Sys.Date(),
    msg = "blubb",
    caller = "blubb()",
    logger = lgr,
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
    logger = lgr,
    foo = "bar"
  )
  app$append(e)
  expect_false("foo" %in% names(app$data))

  expect_identical(
    sapply(app$data$list, class),
    c("NULL", "environment", "data.frame", "data.frame", "NULL")
  )
})




test_that("AppenderDt: .custom works", {
  app <- AppenderDt$new()

  e <- LogEvent$new(
    level = 100,
    timestamp = Sys.Date(),
    msg = "blubb",
    caller = "blubb()",
    logger = lgr
  )

  app$append(e)
  expect_true(is_empty(app$data$.custom[[1]]))
  expect_true(is.list(app$data$.custom[[1]]))

  e$envir <- environment()
  e$schwupp = "foo"
  app$append(e)
  expect_identical(app$data$.custom[[2]]$schwupp, "foo")
  expect_true(is.environment(app$data$.custom[[2]]$envir))

  # warn if .custom is not a list column
  expect_warning(
    app <- AppenderDt$new(prototype = data.table::data.table(
      .id = NA_integer_,
      .custom = NA_integer_
    ))
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




test_that("AppenderDt: default format for show_log() looks like format.LogEvent()", {
  lg <- get_logger("test")
  on.exit(lg$config(NULL))
  lg$add_appender(AppenderDt$new(), "memory")

  xo <- capture.output(lg$fatal("blubb"))
  xp <- capture.output(lg$appenders$memory$show(n = 1))
  expect_identical(xo, xp)

  xo <- capture.output(
    lg$fatal("blubb", foo = "bar", fizz = "buzz", iris = iris)
  )
  xp <- capture.output(lg$appenders$memory$show(n = 1))
  expect_identical(xo, xp)

  expect_length(capture.output(lg$appenders$memory$show(n = 2)), 2)
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
        buffer_size = 10
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
  expect_identical(l$appenders$buffer$buffer_events, list())
  expect_match(
    paste(readLines(buffer_log), collapse = "#"),
    "INFO.*A#INFO.*B#INFO.*C#INFO.*D#INFO.*E#INFO.*F#INFO.*G#FATAL.*a#FATAL.*b#FATAL.*c"
  )

  # Does the next flush flush the correct event?
  l$fatal("x")
  expect_identical(length(readLines(buffer_log)), 11L)
  expect_identical(l$appenders$buffer$buffer_events, list())
  expect_match(paste(readLines(buffer_log), collapse = "#"), ".*A#.*B#.*C#.*a#.*b#.*c#.*x")
})




test_that("AppenderBuffer: buffer cycling triggers flush", {
  replicate(10, l$info("z"))
  expect_identical(length(l$appenders$buffer$buffer_events), 10L)
  l$info(c("y", "y", "y"))
  expect_identical(length(readLines(buffer_log)), 24L)
  expect_identical(l$appenders$buffer$buffer_events, list())
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




# AppenderSyslog ----------------------------------------------------------

test_that("AppenderSyslog: to_syslog_level works", {
  skip_if_not_installed("rsyslog")
  app <- AppenderSyslog$new("myapp")

  expect_identical(
    app$.__enclos_env__$private$to_syslog_levels(c("fatal", "info", "error", "debug", "warn", "trace")),
    c("CRITICAL", "INFO", "ERR", "DEBUG", "WARNING", "DEBUG")
  )

  expect_identical(
    app$.__enclos_env__$private$to_syslog_levels(c("fatal", "info", "error", "debug", "warn", "trace")),
    app$.__enclos_env__$private$to_syslog_levels(c(100, 400, 200, 500, 300, 600))
  )
})




test_that("AppenderSyslog: logging to syslog works", {
  skip_if_not_installed("rsyslog")
  msg <- format(Sys.time())

  lg <- get_logger("rsyslog/test")$set_propagate(FALSE)
  on.exit(lg$config(NULL))
  lg$add_appender(AppenderSyslog$new(), "syslog")
  lg$info("A test message %s", msg)

  log = system("cat /var/log/syslog | grep rsyslog/test", intern = TRUE)
  expect_true(any(grepl(msg, log), fixed = TRUE))
})
