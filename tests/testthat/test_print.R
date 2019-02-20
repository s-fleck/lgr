context("print_Logger")



test_that("formatting Loggers works as expected", {

  l <- Logger$new(
    "test_logger",
    appenders = c(
      AppenderFile$new(file = tempfile()),
      AppenderConsole$new(),
      AppenderFile$new(threshold = 100, file = paste0(tempfile(), tempfile(), tempfile())),
      AppenderDt$new(),
      AppenderBuffer$new(
        appenders = list(AppenderDt$new(), AppenderFile$new(tempfile())))
    )
  )

  # ensure that print doesn't raise exceptions
  expect_silent({
    expect_output(print(l))
    expect_output(print(Logger$new("blubb")))
    expect_output(print(Logger$new("blubb", propagate = FALSE)))
    expect_output(print(Logger$new("blubb", appenders = Appender$new())))
  })
})




