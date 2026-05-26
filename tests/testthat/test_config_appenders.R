test_that("appender thresholds are configured correctly", {
  skip_if_not_installed("yaml")

  # Use a simple inline config with just appenders and thresholds
  cfg <- list(
    version = 1,
    disable_existing_loggers = TRUE,
    appenders = list(
      console_info = list(
        class = "AppenderConsole",
        threshold = "info"
      ),
      console_debug = list(
        class = "AppenderConsole",
        threshold = "debug"
      ),
      buffer = list(
        class = "AppenderBuffer",
        threshold = "all",
        buffer_size = 100
      )
    ),
    loggers = list(
      root = list(
        threshold = "all",
        appenders = c("console_info", "buffer")
      ),
      app = list(
        threshold = "info",
        appenders = c("console_debug", "buffer")
      )
    )
  )

  lgr$config(cfg)

  # Check root logger appenders
  expect_length(lgr$appenders, 2)

  console_appender <- lgr$appenders[[1]]
  buffer_appender <- lgr$appenders[[2]]

  expect_true(inherits(console_appender, "AppenderConsole"))
  expect_true(inherits(buffer_appender, "AppenderBuffer"))

  # Check console appender threshold
  expect_identical(console_appender$threshold, 400L)  # info

  # Check buffer appender threshold
  expect_identical(buffer_appender$threshold, NA_integer_)  # all

  # Check app logger appenders
  app <- get_logger("app")
  expect_length(app$appenders, 2)

  # Verify appender properties
  app_debug_appender <- app$appenders[[1]]
  expect_true(inherits(app_debug_appender, "AppenderConsole"))
  expect_identical(app_debug_appender$threshold, 500L)  # debug

  # Cleanup
  lgr$config(NULL)
  app$config(NULL)
})


test_that("appender layouts are applied correctly", {
  cfg <- list(
    version = 1,
    layouts = list(
      simple_fmt = list(class = "LayoutFormat", fmt = "%L %m"),
      detailed_fmt = list(class = "LayoutFormat", fmt = "%L [%t] <%c> %m")
    ),
    appenders = list(
      console_simple = list(
        class = "AppenderConsole",
        threshold = "info",
        layout = "simple_fmt"
      ),
      console_detailed = list(
        class = "AppenderConsole",
        threshold = "debug",
        layout = "detailed_fmt"
      )
    ),
    loggers = list(
      root = list(
        threshold = "all",
        appenders = "console_simple"
      ),
      app = list(
        threshold = "all",
        appenders = "console_detailed"
      )
    )
  )

  lgr$config(cfg)

  # Check root logger appender layouts
  console_app <- lgr$appenders[[1]]
  expect_true(inherits(console_app$layout, "LayoutFormat"))
  expect_identical(console_app$layout$fmt, "%L %m")

  # Check app logger appender layouts
  app <- get_logger("app")
  debug_console <- app$appenders[[1]]
  expect_identical(debug_console$layout$fmt, "%L [%t] <%c> %m")

  # Cleanup
  lgr$config(NULL)
  app$config(NULL)
})


test_that("buffer appender properties are correct", {
  cfg <- list(
    version = 1,
    appenders = list(
      buffer = list(
        class = "AppenderBuffer",
        threshold = "all",
        buffer_size = 100
      )
    ),
    loggers = list(
      root = list(
        threshold = "all",
        appenders = "buffer"
      )
    )
  )

  lgr$config(cfg)

  # Get buffer appender from root
  buffer_app <- lgr$appenders[[1]]
  expect_true(inherits(buffer_app, "AppenderBuffer"))

  # Buffer should have capacity
  expect_gt(buffer_app$buffer_size, 0)
  expect_identical(buffer_app$threshold, NA_integer_)

  # Cleanup
  lgr$config(NULL)
})


test_that("logger hierarchy and propagation work correctly", {
  cfg <- list(
    version = 1,
    appenders = list(
      console = list(class = "AppenderConsole", threshold = "info")
    ),
    loggers = list(
      root = list(threshold = "all", appenders = "console", propagate = TRUE),
      app = list(threshold = "all", appenders = "console", propagate = FALSE),
      "app/db" = list(threshold = "all", appenders = "console", propagate = FALSE),
      "app/api" = list(threshold = "all", appenders = "console", propagate = TRUE)
    )
  )

  lgr$config(cfg)

  # Root propagates
  expect_true(lgr$propagate)

  # app logger doesn't propagate
  app <- get_logger("app")
  expect_false(app$propagate)

  # app/db logger doesn't propagate
  app_db <- get_logger("app/db")
  expect_false(app_db$propagate)

  # app/api logger propagates
  app_api <- get_logger("app/api")
  expect_true(app_api$propagate)

  # Cleanup
  lgr$config(NULL)
  app$config(NULL)
  app_db$config(NULL)
  app_api$config(NULL)
})
