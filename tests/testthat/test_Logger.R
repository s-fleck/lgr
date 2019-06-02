context("Logger")




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
  ml <- Logger$new("test_logger", appenders = list(memory = AppenderDt$new()))
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
  ml$add_appender(AppenderDt$new(), "blubb")
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

  expect_warning(ml$fatal(stop("blubb")), "Error.*blubb")
  expect_warning(ml$fatal(), "Error")
})




test_that("Inheritance and event propagation works", {
  tf1 <- tempfile()
  tf2 <- tempfile()
  tf3 <- tempfile()
  on.exit(unlink(c(tf1, tf2, tf3)))

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
    "blahblubb.*bar\\}"
  )
  expect_output(
    l$fatal("blah", "blubb {fizz}", foo = "bar", fizz = "buzz"),
    "buzz.*\\{foo.*bar.*fizz.*buzz\\}$"
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
