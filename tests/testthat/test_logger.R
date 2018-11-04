context("loggers")


test_that("active bindings", {
  ml <- Logger$new()

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
  ml <- Logger$new()
  ts <- structure(1540486764.41946, class = c("POSIXct", "POSIXt"))

  testfun <- function(){
    ml$fatal("blubb")
  }

  expect_output({
    testfun()
    testfun()
  })

})




test_that("suspending loggers works", {
  ml <- Logger$new()

  expect_output(ml$fatal("blubb"), "FATAL")
  x <- capture.output(ml$fatal("blubb"))
  ml$suspend()
  expect_identical(ml$fatal("blubb %s", "blah"), NULL)
  ml$unsuspend()
  y <- capture.output(ml$fatal("blubb"))

  # ignore timestamp for comparison
  x <- gsub("[.*]", x, "[time]")
  y <- gsub("[.*]", x, "[time]")

  expect_identical(x, y)
})




test_that("add/remove appenders", {
  ml <- Logger$new(appenders = AppenderFile$new(file = tempfile()))
  app1 <- AppenderConsole$new(threshold = 100)
  app2 <- AppenderConsole$new(threshold = 300)

  ml$add_appender(app1)
  ml$add_appender(app2, "blah")
  ml$add_appender(AppenderMemoryDt$new(), "blubb")

  expect_true(identical(ml$appenders[[2]], app1))
  expect_true(identical(ml$appenders$blah, app2))

  ml$remove_appender(2)
  expect_identical(length(ml$appenders), 3L)

  ml$remove_appender(c("blah", "blubb"))
  expect_identical(length(ml$appenders), 1L)
})




test_that("Exceptions are cought and turned into warnings", {
  ml <- Logger$new(
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
