context("format")


test_that("format works as expected", {

  x <- list(level = 1, caller = "blubb()", user = "schwupp", timestamp = Sys.time(), msg = "this is a test message")

  format.yog_data(
    x,
    format = "[%l -  %L -  %n]  %t -  %u -  %p -  %c:  %m"
  )

  y <- list(level = 1, timestamp = Sys.time(), msg = "this is a test message")

  format.yog_data(
    y,
    format = "[%l -  %L -  %n]  %t -  %u -  %p -  %c:  %m"
  )
})
