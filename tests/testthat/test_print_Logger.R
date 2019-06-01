context("format")



test_that("print.Logger() works as expected", {

  tf1 <- tempfile()
  tf2 <- tempfile()
  tf_long <- tempfile(pattern = paste(letters, LETTERS, sep = "-", collapse = "-"))
  on.exit(file.remove(tf1, tf2, tf_long))

  l <- Logger$new(
    "test_logger",
    appenders = c(
      AppenderFile$new(file = tf1),
      AppenderConsole$new(),
      AppenderFile$new(threshold = 100, file = tf_long),
      AppenderDt$new(),
      AppenderBuffer$new(
        appenders = list(AppenderDt$new(), AppenderFile$new(file = tf2))
      )
    )
  )


  # ensure that print doesn't raise exceptions
  expect_output(print(l))
  expect_output(print(Logger$new("blubb", propagate = FALSE)))
  expect_output(print(Logger$new("blubb", propagate = FALSE)))
  expect_output(print(Logger$new("blubb", propagate = FALSE, appenders = Appender$new())))
})
