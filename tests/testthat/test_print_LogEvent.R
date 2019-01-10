context("print_LogEvent")




test_that("format works as expected", {
  l <- Logger$new("test")

  x <- LogEvent$new(
    level = 100,
    caller = "blubb()",
    timestamp = Sys.time(),
    msg = "this is a test message",
    logger = l
  )

  expect_match(
    format.lgr_data(x, fmt = "[%l -  %L -  %n]  %t -  %p -  %c:  %m"),
    "fatal.*FATAL.*blubb\\(\\).*message$"
  )
})




test_that("format.LogEvent works as expected", {
  x <- LogEvent$new(
    logger = lgr::lgr,
    msg = "lorem skjdghsad akjsgh asdgjh asdgjshadk gklsd.",
    blah = "blubb",
    numbers = 1:100,
    large_number = c(23.525325235213525235525, 930.824687923409867298367293406),
    iris = iris,
    logg = lgr::lgr,
    letters = letters
  )

  expect_output(print(x))
  expect_true(!crayon::has_style(format(x, colors = NULL)))
  expect_output(expect_identical(x, print(x)))
})
