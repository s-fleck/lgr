context("utils")


test_that("utils works as expected", {

  foo <- function() get_caller(-1L)
  expect_identical(foo(), "foo")

  lgr <- Logger$new(
    "test logger",
    appenders = AppenderConsole$new(layout = LayoutFormat$new(fmt = "%c")),
    parent = NULL
  )


  foobar <- function() lgr$error("test")
  expect_identical(foo(), "foo")

  blahblubb <- function() lgr$log(200, "test")
  expect_identical(capture.output(blahblubb()), "blahblubb")
})
