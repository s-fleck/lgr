context("format")


test_that("format works as expected", {

  x <- list(level = 100, caller = "blubb()", timestamp = Sys.time(), msg = "this is a test message", user = "foobert")

  expect_match(
    format.yog_data(x, fmt = "[%l -  %L -  %n]  %t -  %u -  %p -  %c:  %m", user = "foobert"),
    "fatal.*FATAL.*foobert.*blubb\\(\\).*message$"
  )

})




test_that("formatting Loggers works as expected", {

  l <- Logger$new(
    "test_logger",
    appenders = c(
      AppenderFile$new(file = tempfile()),
      AppenderConsole$new(),
      AppenderFile$new(threshold = 100, file = paste0(tempfile(), tempfile(), tempfile())),
      AppenderMemoryDt$new(),
      AppenderBuffer$new(
        appenders = list(AppenderMemoryDt$new(), AppenderFile$new(tempfile())))
    )
  )

  # ensure that print doesn't raise exceptions
  expect_output(print(l))
  expect_output(print(Logger$new("blubb", parent = NULL)))
  expect_output(print(Logger$new("blubb", parent = NULL, propagate = FALSE)))
  expect_output(print(Logger$new("blubb", parent = NULL, appenders = Appender$new())))
})
