context("config_comprehensive")

test_that("comprehensive config loads and applies correctly", {
  cfg <- list(
    version = 1,
    disable_existing_loggers = TRUE,
    layouts = list(
      simple = list(class = "LayoutFormat", fmt = "%L %m"),
      detailed = list(class = "LayoutFormat", fmt = "%L [%t] <%c> %m")
    ),
    filters = list(
      info_filter = list(class = "FilterForceLevel", level = "info"),
      warn_filter = list(class = "FilterForceLevel", level = "warn"),
      debug_filter = list(class = "FilterForceLevel", level = "debug")
    ),
    appenders = list(
      console_info = list(
        class = "AppenderConsole",
        threshold = "info",
        layout = "simple"
      ),
      console_debug = list(
        class = "AppenderConsole",
        threshold = "debug",
        layout = "detailed"
      ),
      buffer = list(
        class = "AppenderBuffer",
        threshold = "all",
        buffer_size = 100
      )
    ),
    loggers = list(
      root = list(
        threshold = "debug",
        appenders = c("console_info", "buffer"),
        propagate = TRUE
      ),
      app = list(
        threshold = "info",
        appenders = c("console_debug", "buffer"),
        propagate = FALSE
      ),
      "app/db" = list(
        threshold = "warn",
        filters = "warn_filter",
        appenders = "buffer",
        propagate = FALSE
      ),
      "app/api" = list(
        threshold = "all",
        appenders = "buffer",
        propagate = TRUE
      )
    )
  )

  lgr$config(cfg)

  # Test root logger
  expect_identical(lgr$threshold, 500L)  # debug
  expect_length(lgr$appenders, 2)
  expect_true(inherits(lgr$appenders[[1]], "AppenderConsole"))
  expect_true(inherits(lgr$appenders[[2]], "AppenderBuffer"))

  # Test app logger
  app <- get_logger("app")
  expect_identical(app$threshold, 400L)  # info
  expect_length(app$appenders, 2)
  expect_false(app$propagate)

  # Test app/db logger with filter
  app_db <- get_logger("app/db")
  expect_identical(app_db$threshold, 300L)  # warn
  expect_length(app_db$filters, 1)
  expect_true(inherits(app_db$filters[[1]], "FilterForceLevel"))
  expect_false(app_db$propagate)

  # Test app/api logger
  app_api <- get_logger("app/api")
  expect_identical(app_api$threshold, NA_integer_)  # all
  expect_length(app_api$appenders, 1)
  expect_true(app_api$propagate)

  # Cleanup
  lgr$config(NULL)
  app$config(NULL)
  app_db$config(NULL)
  app_api$config(NULL)
})


test_that("comprehensive JSON config loads and applies correctly", {
  skip_if_not_installed("jsonlite")

  config_file <- rprojroot::find_testthat_root_file(
    "testdata",
    "config_comprehensive.json"
  )
  expect_true(file.exists(config_file))

  lgr$config(file = config_file)

  # Test root logger configuration
  expect_identical(lgr$threshold, 500L)
  expect_length(lgr$appenders, 2)

  # Test app logger configuration
  app <- get_logger("app")
  expect_identical(app$threshold, 400L)
  expect_length(app$appenders, 2)

  # Test app/db logger with filter configuration
  app_db <- get_logger("app/db")
  expect_length(app_db$filters, 1)

  # Cleanup
  lgr$config(NULL)
  app$config(NULL)
  app_db$config(NULL)
  get_logger("app/api")$config(NULL)
})

