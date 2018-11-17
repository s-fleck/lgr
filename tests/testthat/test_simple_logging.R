context("simple_logging")


test_that("simple_logging works as expected", {

  ml <- Logger$new("dummy")

  expect_error(ml$threshold <- "blubb")
  add_log_levels(c(blubb = 250))
  expect_silent(ml$threshold <- "blubb")
  expect_identical(ml$threshold, 250L)

})
