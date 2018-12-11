context("format")


test_that("format works as expected", {

  x <- list(level = 100, caller = "blubb()", timestamp = Sys.time(), msg = "this is a test message", user = "foobert")

  expect_match(
    format.yog_data(x, fmt = "[%l -  %L -  %n]  %t -  %u -  %p -  %c:  %m", user = "foobert"),
    "fatal.*FATAL.*foobert.*blubb\\(\\).*message$"
  )

})




test_that("formatting Loggers works as expected", {

  tf <- tempfile()

  l <- Logger$new(
    "blubb",
    appenders = list(
      AppenderFile$new(file = tf),
      AppenderBuffer$new(
        appenders = list(
          AppenderMemoryDt$new(),
          AppenderConsole$new()
        )
      )
  ))


  l








})
