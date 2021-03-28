context("logger_index")


test_that("logger_index works as expected", {

  remove_all_loggers()
  on.exit(remove_all_loggers())

  get_logger("tree/leaf")
  get_logger("shrub/leaf")
  get_logger("plant/shrub/leaf")

  expect_identical(nrow(logger_index()), 8L)
})
