context("get_logger")


test_that("get_logger works as expected", {

  lg <- get_logger("blubb")

  expect_identical(lg$full_name, "lgr.blubb")


  lg1 <- get_logger("fizz.buzz")
  lg2 <- get_logger("fizz")

  expect_identical(lg1$parent, lg2)


})
