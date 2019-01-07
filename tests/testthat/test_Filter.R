context("Filter")


test_that("Filter works as expected", {

  l <- Logger$new("foo")

  l$add_filter(FilterForceLevel$new("fatal"))
  l$add_filter(FilterInject$new(iris = iris, .list = list(cars = "foo")))
  expect_output(l$info("blubb"), "FATAL.*cars.*data\\.frame")
  l$remove_filter(1)
  expect_output(l$info("blubb"), "INFO.*cars.*data\\.frame")
  l$remove_filter(1)
  expect_output(l$info("test"), "test\\s*$")
})
