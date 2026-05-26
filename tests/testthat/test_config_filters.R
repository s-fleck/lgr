config_with_file_fixture <- function(config_file, env = parent.frame()) {

  basic_config(console = "info")

  lgr$config(file = config_file)

  withr::defer({
    basic_config()
    for (l in logger_index()$name){
      get_logger(l, reset = TRUE)
    }
  },
  envir = env)

  invisible()
}


test_that("filters are configured and applied correctly", {
  skip_if_not_installed("yaml")

  config_file <- rprojroot::find_testthat_root_file(
    "testdata",
    "config_comprehensive.yaml"
  )

  config_with_file_fixture(config_file)

  # Root logger should have no filters
  expect_length(lgr$filters, 0)

  # app/db logger should have the warn_and_above filter
  app_db <- get_logger("app/db")
  expect_length(app_db$filters, 1)

  filter_obj <- app_db$filters[[1]]
  expect_true(inherits(filter_obj, "FilterForceLevel"))
  expect_identical(filter_obj$level, 300L)  # warn

  # Cleanup
  lgr$config(NULL)
  get_logger("app")$config(NULL)
  app_db$config(NULL)
  get_logger("app/api")$config(NULL)
})


test_that("filters from JSON config work correctly", {
  skip_if_not_installed("jsonlite")

  config_file <- rprojroot::find_testthat_root_file(
    "testdata",
    "config_filters.json"
  )

  config_with_file_fixture(config_file)

  # myapp logger should have error_level filter
  myapp <- get_logger("myapp")
  expect_length(myapp$filters, 1)

  filter_obj <- myapp$filters[[1]]
  expect_true(inherits(filter_obj, "FilterForceLevel"))
  expect_identical(filter_obj$level, 200L)  # error

  # Cleanup
  lgr$config(NULL)
  myapp$config(NULL)
})


test_that("FilterForceLevel properties are accessible", {
  skip_if_not_installed("yaml")

  config_file <- rprojroot::find_testthat_root_file(
    "testdata",
    "config_comprehensive.yaml"
  )

  config_with_file_fixture(config_file)

  app_db <- get_logger("app/db")
  filter_obj <- app_db$filters[[1]]

  # Check filter properties
  expect_true(inherits(filter_obj, "FilterForceLevel"))
  expect_identical(filter_obj$level, 300L)  # warn level

  # Cleanup
  lgr$config(NULL)
  get_logger("app")$config(NULL)
  app_db$config(NULL)
  get_logger("app/api")$config(NULL)
})




test_that("logger - with FilterForceLevel - overrides log level", {
  skip_if_not_installed("yaml")


  # arrange
  config_with_file_fixture(config_file)

  lgr$config(file = config_file)

  testing_force_error <- get_logger("testing/force_error_level")

  # act
  capture.output(testing_force_error$log(level = 400L, msg = "test message"))
  events <- testing_force_error$last_event

  # assert
  expect_identical(testing_force_error$threshold, 500L)  # debug
  expect_length(testing_force_error$filters, 1)
  filter_obj <- testing_force_error$filters[[1]]

  expect_identical(filter_obj$level, 200L)
  expect_identical(events$level, 200L)
})




test_that("logger thresholds and filters interact correctly", {
  skip_if_not_installed("yaml")

  # Cleanup
  config_with_file_fixture(config_file)

  # arrange
  config_file <- rprojroot::find_testthat_root_file(
    "testdata",
    "config_appenders.yaml"
  )

  lgr$config(file = config_file)


  # act
  testing_force_error <- get_logger("testing/force_error_level")
  testing_force_trace <- get_logger("testing/force_trace_level")
  testing_force_error$info("test message that should be promoted to error")
  testing_force_trace$info("test message that should be demoted to trace")

  # loggers are correctly configured
  expect_identical(testing_force_error$threshold, 500L)
  expect_identical(testing_force_trace$threshold, 500L)
  expect_length(testing_force_error$filters, 1)
  expect_length(testing_force_trace$filters, 1)

  expect_identical(testing_force_trace$filters[[1]]$level, 600L)
  expect_identical(testing_force_error$filters[[1]]$level, 200L)

  # Verify logging behavior
  # Events at trace level should be captured by the filter

  # filters are applied after the threshold, so the 600 (trace) event is
  # *not* filtered out despite the loggers threshold being 500 (debug)
  trace_event <- testing_force_trace$last_event
  expect_identical(error_event$level, 600L)

  error_event <- testing_force_error$last_event
  expect_identical(error_event$level, 200L)
})
