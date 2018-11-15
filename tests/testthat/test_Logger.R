context("Logger")


test_that("active bindings", {
  ml <- Logger$new("test_logger")

  expect_identical(
    ml$log_levels,
    structure(
      setNames(seq(100L, 600L, by = 100L), c("fatal", "error", "warn", "info", "debug", "trace")),
      class = c("log_levels", "integer")
    )
  )
  expect_error(ml$log_levels <- ml$log_levels, "cannot be modified")


  expect_silent(ml$threshold <- 5)
  expect_identical(ml$threshold, 5L)
  expect_silent(ml$threshold <- "fatal")
  expect_identical(ml$threshold, 100L)
  expect_error(ml$threshold <- "blubb", "fatal.*trace")

  walk(ml$appenders, function(.x) expect_true(inherits(.x, "Appender")))

  expect_silent(ml$user <- "blubb")
  expect_identical(ml$user, "blubb")
  expect_error(ml$user <- 5, "'user'")

  expect_true(is.function(ml$string_formatter))
})




test_that("basic logging", {
  ml <- Logger$new("test_logger", appenders = list(memory = AppenderMemoryDt$new()))
  ts <- structure(1540486764.41946, class = c("POSIXct", "POSIXt"))

  testfun <- function(){
    ml$fatal("blubb")
    ml$log(msg = "testfun", level = 500)
  }

  expect_output({
    testfun()
    testfun()
  })

  expect_true(all(ml$appenders$memory$data$caller == "testfun"))
})




test_that("suspending loggers works", {
  ml <- Logger$new("test_logger")

  expect_output(ml$fatal("blubb"), "FATAL")
  x <- capture.output(ml$fatal("blubb"))
  ml$threshold <- 0
  expect_identical(ml$fatal("blubb %s", "blah"), NULL)
  ml$threshold <- "error"
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




test_that("add/remove appenders", {
  ml <- Logger$new("test_logger", appenders = AppenderFile$new(file = tempfile()))
  app1 <- AppenderConsole$new(threshold = 100)
  app2 <- AppenderConsole$new(threshold = 300)

  ml$add_appender(app1)
  ml$add_appender(app2, "blah")
  ml$add_appender(AppenderMemoryDt$new(), "blubb")

  # because the now have the logger proerty set
  expect_identical(ml$appenders[[2]], app1)
  expect_identical(ml$appenders[[2]]$logger, ml)

  expect_identical(ml$appenders$blah, app2)
  expect_identical(ml$appenders$blah$logger, ml)

  ml$remove_appender(2)
  expect_identical(length(ml$appenders), 3L)

  ml$remove_appender(c("blah", "blubb"))
  expect_identical(length(ml$appenders), 1L)
})




test_that("Exceptions are cought and turned into warnings", {
  ml <- Logger$new("test_logger",
    appenders = list(
      AppenderFile$new(file = tempfile()),
      AppenderConsole$new()
    )
  )

  expect_warning(ml$fatal(stop("blubb")), "Error.*blubb")
  expect_warning(ml$fatal(), "Error")

  ml$add_appender(AppenderFile$new(
    file = file.path(tempfile(), "non", "existing", "directory" )
  ))

  expect_output(expect_warning(ml$fatal("blubb"), "Error"))
})



test_that("Logger inheritance and event propagation", {
  tf1 <- tempfile()
  tf2 <- tempfile()
  tf3 <- tempfile()
  c1  <- Logger$new("c1", appenders = AppenderFile$new(tf1))
  c2  <- Logger$new("c2", parent = c1, appenders = AppenderFile$new(tf2))
  c3  <- Logger$new("c3", parent = c2, appenders = AppenderFile$new(tf3))

  expect_output(c3$fatal("blubb"), "FATAL.*blubb")
  expect_match(readLines(tf1), "FATAL.*blubb")
  expect_identical(readLines(tf1), readLines(tf2))
  expect_identical(readLines(tf1), readLines(tf3))

  c2$propagate <- FALSE
  expect_silent(c3$error("blubb"))
  expect_identical(readLines(tf2), readLines(tf3))
  expect_lt(length(readLines(tf1)), length(readLines(tf3)))
})




test_that("filters work", {
  c1  <- Logger$new("c1")
  expect_output(c1$error("blubb"), "ERROR")
  c1$threshold <- 100
  expect_silent(c1$error("blubb"))
})
