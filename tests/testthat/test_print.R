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
      AppenderDt$new(),
      AppenderBuffer$new(
        appenders = list(AppenderDt$new(), AppenderFile$new(tempfile())))
    )
  )

  # ensure that print doesn't raise exceptions
  expect_output(print(l))
  expect_output(print(Logger$new("blubb", parent = NULL)))
  expect_output(print(Logger$new("blubb", parent = NULL, propagate = FALSE)))
  expect_output(print(Logger$new("blubb", parent = NULL, appenders = Appender$new())))
})



test_that("format works as expected", {

  # common case
  x <- LogEvent$new(
    logger = yog::yog,
    msg = "lorem skjdghsad akjsgh asdgjh asdgjshadk gklsd.",
    waypoints = 100,
    user = "max@company.com"
  )

  print(x)

  format(x)


  x <- LogEvent$new(
    logger = yog::yog,
    msg = "lorem skjdghsad akjsgh asdgjh asdgjshadk gklsd.",
    blah = "blubb",
    numbers = 1:100,
    large_number = c(23.525325235213525235525, 930.824687923409867298367293406),
    iris = iris,
    logg = yog::yog,
    letters = letters
  )

  print(x)


  format_custom_fields(x$custom_values)
  format_custom_fields(list("letters" = letters))
})


