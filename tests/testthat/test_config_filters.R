context("config_filters")

test_that("filters are configured and applied correctly", {
  skip_if_not_installed("yaml")

  config_file <- rprojroot::find_testthat_root_file(
    "testdata",
    "config_comprehensive.yaml"
  )

  lgr$config(file = config_file)

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

  lgr$config(file = config_file)

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

  lgr$config(file = config_file)

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


test_that("logger thresholds and filters interact correctly", {
  skip_if_not_installed("yaml")

  config_file <- rprojroot::find_testthat_root_file(
    "testdata",
    "config_appenders.yaml"
  )

  lgr$config(file = config_file)

  testing <- get_logger("testing")

  # Testing logger has debug threshold and trace_only filter
  expect_identical(testing$threshold, 500L)  # debug
  expect_length(testing$filters, 1)

  filter_obj <- testing$filters[[1]]
  expect_identical(filter_obj$level, 600L)  # trace

  # Verify logging behavior
  # Events at trace level should be captured by the filter
  testing$log(level = 600L, msg = "test trace message")
  events <- testing$last_event

  expect_identical(events$level, 600L)

  # Cleanup
  lgr$config(NULL)
  get_logger("testing")$config(NULL)
  get_logger("testing/unit")$config(NULL)
  get_logger("testing/integration")$config(NULL)
})
