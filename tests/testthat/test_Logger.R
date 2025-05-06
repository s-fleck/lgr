context("Logger")


# Logger ------------------------------------------------------------------



test_that("logging conditions works", {
  e <- error("blahblah")

  ln <- get_logger("test")$
    config(NULL)$
    set_propagate(FALSE)

  lg <- get_logger_glue("test_glue")$
    config(NULL)$
    set_propagate(FALSE)

  expect_match(ln$fatal(e), "blahblah")
  expect_match(lg$fatal(e, "this is <{it}>", e, it = 1, conditon = e), "blahblahthis is <1>blahblah")
})



test_that("set_threshold()", {
  ml <- Logger$new("test_logger")

  expect_silent(ml$set_threshold(5))
  expect_identical(ml$threshold, 5L)
  expect_silent(ml$set_threshold("fatal"))
  expect_identical(ml$threshold, 100L)
  expect_error(ml$set_threshold("blubb"), "fatal.*trace")

  walk(ml$appenders, function(.x) expect_true(inherits(.x, "Appender")))
})




test_that("$log() & co work", {
  ml <- Logger$new("test_logger", appenders = list(memory = AppenderBuffer$new()))
  ts <- structure(1540486764.41946, class = c("POSIXct", "POSIXt"))

  testfun <- function(){
    ml$fatal("blubb")
    ml$log(msg = "testfun", level = 500)
  }

  expect_output({
    testfun()
    testfun()
  })

  expect_true(
    all(ml$appenders$memory$data$caller == "testfun"),
    info = ml$appenders$memory$data$caller
  )
})




test_that("string formatting works", {
  l <- Logger$new("test_logger", propagate = FALSE)

  expect_identical(l$fatal("test"), "test")
  expect_identical(l$fatal("te%s", "st"), "test")
  expect_identical(l$fatal("te%s", "st", blah = "blubb"), "test")
})




test_that("string formatting is only used if unnamed arguments exist", {
  lg <- Logger$new(
    "test",
    appenders = list(AppenderConsole$new()),
    propagate = FALSE
  )

  # unnamed arguments trigger sprintf
  expect_output(lg$fatal("foo %s", "bar"), "foo bar")
  expect_output(lg$fatal("foo %s", "bar", fizz = "buzz"), "foo bar")

  # otherwise msg is processed as-is
  expect_output(lg$fatal("foo %s"), "foo %s")
  expect_output(lg$fatal("foo %s", fizz = "buzz"), "foo %s")
})




test_that("set_threshold() works for Loggers and Appenders", {
  lg <- Logger$new("dummy", appenders = AppenderConsole$new())
  lg$set_threshold(200)
  expect_identical(lg$threshold, 200L)
  lg$set_threshold("info")
  expect_identical(lg$threshold, 400L)
  lg$set_threshold(NA)
  expect_identical(lg$threshold, NA_integer_)
  expect_error(lg$set_threshold("blubb"), "log levels")

  # test if setting for appenders of a logger also works as this is somewhat tricky
  lg$appenders[[1]]$set_threshold(NA)
  expect_identical(lg$appenders[[1]]$threshold, NA_integer_)
  lg$appenders[[1]]$set_threshold(300)
  expect_identical(lg$appenders[[1]]$threshold, 300L)
  lg$appenders[[1]]$set_threshold("info")
  expect_identical(lg$appenders[[1]]$threshold, 400L)
  expect_error(lg$appenders[[1]]$set_threshold("blubb"), "log levels")
})




test_that("threshold of `0` suspends logging", {
  ml <- Logger$new("test_logger")

  expect_output(ml$info("blubb"), "blubb")

  expect_output(ml$fatal("blubb"), "FATAL")
  x <- capture.output(ml$fatal("blubb"))
  ml$set_threshold(0)
  expect_identical(ml$fatal("blubb %s", "blah"), NULL)
  ml$set_threshold("error")
  y <- capture.output(ml$fatal("blubb"))

  expect_output(ml$log(100, "test"), "FATAL")
  expect_output(ml$log("error", "test"), "ERROR")
  expect_silent(ml$warn("blubb"))
  expect_silent(ml$log(300, "blubb"))
  # ignore timestamp for comparison
  x <- gsub("[.*]", x, "[time]")
  y <- gsub("[.*]", x, "[time]")

  expect_identical(x, y)
})




test_that("add_appenders()/remove_appenders()", {
  tf <- tempfile()
  on.exit(unlink(tf))
  ml <- Logger$new("test_logger", appenders = AppenderFile$new(file = tf))
  app1 <- AppenderConsole$new(threshold = 100)
  app2 <- AppenderConsole$new(threshold = 300)

  # add
  ml$add_appender(app1)
  ml$add_appender(app2, "blah")
  ml$add_appender(AppenderBuffer$new(), "blubb")
  expect_identical(ml$appenders[[2]], app1)
  expect_identical(ml$appenders$blah, app2)

  # remove
  ml$remove_appender(2)
  expect_length(ml$appenders, 3)
  ml$remove_appender(c("blah", "blubb"))
  expect_length(ml$appenders, 1L)

  # set
  ml$set_appenders(list(app1, app2))
  expect_length(ml$appenders, 2)
  ml$set_appenders(list(app1, app2))
  expect_length(ml$appenders, 2)
  expect_identical(ml$appenders[[1]], app1)
  expect_identical(ml$appenders[[2]], app2)
})




test_that("setting Appender properties works", {
  ml <- Logger$new("test_logger", appenders = list(AppenderConsole$new()), propagate = FALSE)
  tf <- tempfile()
  on.exit(unlink(tf))

  # Add a new appender to a logger. We don't have to supply a name, but that
  # mak1es it easier to remove later.
  ml$add_appender(AppenderFile$new(file = tf), name = "file")

  # configure lgr so that it logs everything to the file, but only info and above
  # to the console
  ml$set_threshold(NA)
  ml$appenders[[1]]$set_threshold("info")
  ml$appenders$file$set_threshold(NA)
  expect_output(ml$info("Another informational message"))
  expect_silent(ml$debug("A debug message that the console appender doesn't show."))
  expect_identical(length(readLines(tf)), 2L)
  expect_match(paste(readLines(tf), collapse = "---"), "INFO.*---DEBUG.*")
})




test_that("Exceptions are cought and turned into warnings", {
  tf <- tempfile()
  on.exit(unlink(tf))
  ml <- Logger$new("test_logger",
    appenders = list(
      AppenderFile$new(file = tf),
      AppenderConsole$new()
    )
  )

  expect_warning(ml$fatal(stop("blubb")), "error.*blubb")
  expect_warning(ml$fatal(), "error")
})




test_that("Inheritance and event propagation works", {
  on.exit({
    c1$config(NULL)
    c2$config(NULL)
    c3$config(NULL)
    unlink(c(tf1, tf2, tf3))
  })

  tf1 <- tempfile()
  tf2 <- tempfile()
  tf3 <- tempfile()

  c1  <- get_logger("c1")
  c1$add_appender(AppenderFile$new(tf1))

  c2  <- get_logger("c1/c2")
  c2$add_appender(AppenderFile$new(tf2))

  c3  <- Logger$new("c1/c2/c3")
  c3$add_appender(AppenderFile$new(tf3))

  expect_output(c3$fatal("blubb"), "FATAL.*blubb")
  expect_match(readLines(tf1), "FATAL.*blubb")
  expect_identical(readLines(tf1), readLines(tf2))
  expect_identical(readLines(tf1), readLines(tf3))

  c2$set_propagate(FALSE)
  expect_silent(c3$error("blubb"))
  expect_identical(readLines(tf2), readLines(tf3))
  expect_lt(length(readLines(tf1)), length(readLines(tf3)))
})




test_that("Named inherited appenders are displayed correctly", {
  on.exit({
    c1$config(NULL)
    c2$config(NULL)
    c3$config(NULL)
  })

  c1  <- get_logger("c1")$
    set_propagate(FALSE)$
    add_appender(AppenderConsole$new(), "console")

  expect_null(c1$inherited_appenders)

  c2  <- get_logger("c1/c2")$
    add_appender(AppenderConsole$new(), "console")

  expect_length(c2$inherited_appenders, 1L)

  c3  <- Logger$new("c1/c2/c3")$
    add_appender(AppenderConsole$new(), "console")

  expect_length(c3$inherited_appenders, 2L)

  expect_identical(names(c3$inherited_appenders), c("console", "console"))
  expect_output(print(c3), "\\sconsole:.*\\sconsole:.*")
})




test_that("Unnamed inherited appenders are displayed correctly", {
  on.exit({
    c1$config(NULL)
    c2$config(NULL)
    c3$config(NULL)
  })

  c1  <- get_logger("c1")$
    set_propagate(FALSE)$
    add_appender(AppenderConsole$new())

  c2  <- get_logger("c1/c2")$
    add_appender(AppenderConsole$new())

  c3  <- Logger$new("c1/c2/c3")$
    add_appender(AppenderConsole$new())

  expect_null(names(c3$inherited_appenders))
  expect_output(print(c3), "\\[\\[1\\]\\]\\:.*\\[\\[2\\]\\]\\:.*")
})




test_that("threshold works", {
  c1  <- Logger$new("c1")
  expect_output(c1$error("blubb"), "ERROR")
  c1$set_threshold(100)
  expect_silent(c1$error("blubb"))
})




test_that("$ancestry works", {
  l4 <- get_logger("l1/l2/l3/l4")
  l2 <- get_logger("l1/l2")$set_propagate(FALSE)

  expect_equal(
    unname(unclass(l4$ancestry)),
    c(TRUE, FALSE, TRUE, TRUE)
  )
})




# LoggerGlue --------------------------------------------------------------

test_that("LoggerGlue supports custom fields", {
  l <- LoggerGlue$new("glue")

  expect_output(l$fatal("test", "test"), glue::glue("test", "test"))

  expect_output(
    l$fatal("blah", "blubb", foo = "bar"),
    ".*blahblubb.*bar.*\\}"
  )
  expect_output(
    l$fatal("blah", "blubb {fizz}", foo = "bar", fizz = "buzz"),
    "buzz.*foo.*bar.*fizz.*buzz.*"
  )

  expect_output(l$fatal("a", .open = "{", .foo = "bar", fizz = "buzz"))

  expect_true("fizz" %in% names(l$last_event))
  expect_false(".open" %in% names(l$last_event))
  expect_false(".foo" %in% names(l$last_event))
})




test_that("LoggerGlue uses the correct evaluation environment", {
  l <- LoggerGlue$new("glue", propagate = FALSE)

  expect_match(l$fatal("{iris[['Species']][[1]]}"), "setosa")
  expect_match(l$log(100, "{iris[['Species']][[1]]}"), "setosa")
  expect_match(l$fatal(100, "{x}", x = iris[['Species']][[1]]), "setosa")
  expect_match(l$log(100, "{x}", x = iris[['Species']][[1]]), "setosa")
})




test_that("$config works with lists", {
  l <- LoggerGlue$new("glue", propagate = FALSE)

  cfg <- list(
    appenders = list(blubb = AppenderConsole$new()),
    propagate = FALSE
  )

  expect_identical(names(l$config(cfg)$appenders), "blubb")
  expect_identical(names(l$config(list = cfg)$appenders), "blubb")
  expect_identical(l$config(cfg)$propagate, FALSE)
  expect_error(l$config(cfg = cfg, list = cfg))

  l$config(NULL)
  expect_true(is_virgin_Logger(l, allow_subclass = TRUE))
  expect_false(is_virgin_Logger(l))
})




# Multi-Logger tests -------------------------------------------------------------

test_that("Logger$log() dispatches to all appenders, even if some throw an error", {
  ln <- Logger$new("normal", propagate = FALSE)
  lg <- LoggerGlue$new("glue", propagate = FALSE)

  on.exit({
    ln$config(NULL)
    lg$config(NULL)
  })

  AppErr <- R6::R6Class(
    inherit = AppenderConsole,
    public = list(
      append = function(...) stop("error")
    )
  )

  tf <- tempfile()
  on.exit(unlink(tf))

  ln$set_appenders(list(
    err = AppErr$new(),
    file = AppenderFile$new(file = tf)
  ))
  lg$set_appenders(list(
    err = AppErr$new(),
    file = AppenderFile$new(file = tf)
  ))

  expect_warning(ln$info("test_normal"), class = "appendingFailedWarning")
  expect_warning(ln$info("test_glue"), class = "appendingFailedWarning")

  expect_true(any(grepl("test_normal", readLines(tf))))
  expect_true(any(grepl("test_glue", readLines(tf))))
})



test_that("Logger error contains useful call object", {
  l <- get_logger("test")
  g <- get_logger_glue("testglue")

  on.exit({
    l$config(NULL)
    g$config(NULL)
  })

  expect_warning(l$info("this will fail", e = stop()), "l\\$info")
  expect_warning(g$info("this will fail", e = stop()), "g\\$info")
})





test_that("Appender error contains useful call object", {
  l <- get_logger("test")$set_propagate(FALSE)
  g <- get_logger_glue("testglue")$set_propagate(FALSE)

  on.exit({
    l$config(NULL)
    g$config(NULL)
  })

  AppenderFail <- R6::R6Class(
    "AppenderFail",
    inherit = Appender,
    public = list(
      append = function(e) stop("bummer")
    )
  )

  a <- AppenderFail$new()

  l$add_appender(a, "fail")
  g$add_appender(a, "fail")

  expect_warning(l$info("this will fail"), ".*AppenderFail.*l\\$info")
  expect_warning(g$info("this will fail"), ".*AppenderFail.*g\\$info")
})


test_that("Logger$log - with 'msg' argument - logs the message", {
  l <- get_logger("test")$set_propagate(FALSE)

  on.exit({
    l$config(NULL)
  })

  expect_silent(l$log(level = "fatal", msg = "test"))
})


test_that("LoggerGlue$log - with 'msg' argument - throws a warning", {
  l <- get_logger_glue("testglue")$set_propagate(FALSE)

  on.exit({
    l$config(NULL)
  })

  expect_warning(l$log(level = "fatal", msg = "test"), "does not support")
})


test_that("Logger$log - with string interpolation - adds .rawMsg to event", {
  l <- get_logger("test")$set_propagate(FALSE)

  on.exit({
    l$config(NULL)
  })

  l$info("foo %s", "bar")

  expect_identical(l$last_event$msg, "foo bar")
  expect_identical(l$last_event$.rawMsg, "foo %s")
})


test_that("LoggerGlue$log - with string interpolation - adds .rawMsg to event", {
  l <- get_logger_glue("testglue")$set_propagate(FALSE)

  on.exit({
    l$config(NULL)
  })

  l$fatal("hash {x}", x = "baz")

  expect_identical(as.character(l$last_event$msg), "hash baz")
  expect_identical(l$last_event$.rawMsg, "hash {x}")
})

