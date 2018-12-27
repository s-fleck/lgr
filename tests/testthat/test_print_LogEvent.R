context("print_LogEvent")


test_that("format works as expected", {

  x <- list(level = 100, caller = "blubb()", timestamp = Sys.time(), msg = "this is a test message", user = "foobert")

  expect_match(
    format.yog_data(x, fmt = "[%l -  %L -  %n]  %t -  %u -  %p -  %c:  %m", user = "foobert"),
    "fatal.*FATAL.*foobert.*blubb\\(\\).*message$"
  )

})





test_that("format.LogEvent works as expected", {
  # common case
  x <- LogEvent$new(
    logger = yog::yog,
    msg = "lorem skjdghsad akjsgh asdgjh asdgjshadk gklsd.",
    waypoints = 100,
    user = "max@company.com"
  )

  # print(x)
  # format(x)


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

  expect_output(print(x))
  expect_true(!crayon::has_style(format(x, colors = NULL)))
  expect_output(expect_identical(x, print(x)))

})
