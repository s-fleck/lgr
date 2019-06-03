context("logger_tree")


testthat::setup(remove_all_loggers())
testthat::teardown(remove_all_loggers())


test_that("logger_tree works as expected", {
  get_logger("blah/blubb/foo")
  get_logger("blah/blubb/bar")
  expect_setequal(logger_tree()$parent, c("root", "blah", "blubb", "foo", "bar"))
  remove_all_loggers()
  expect_identical(logger_tree()$parent, "root")
})
