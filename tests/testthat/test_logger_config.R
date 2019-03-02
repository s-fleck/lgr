context("logger_config")


test_that("logger_config works as expected", {
  ty <- rprojroot::find_testthat_root_file("testdata", "lg_full.yaml")

  cfg <- as_logger_config(ty)



})
