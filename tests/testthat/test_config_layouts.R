test_that("layout configurations are applied correctly from YAML", {
  skip_if_not_installed("yaml")

  config_file <- rprojroot::find_testthat_root_file(
    "testdata",
    "config_layouts.yaml"
  )

  lgr$config(file = config_file)

  # Check root logger appender layouts
  console_min <- lgr$appenders[[1]]
  expect_true(inherits(console_min$layout, "LayoutFormat"))
  expect_identical(console_min$layout$fmt, "%m")

  buffer_app <- lgr$appenders[[2]]
  expect_true(inherits(buffer_app$layout, "LayoutJson"))

  # Check debug_logger appender layout
  debug_logger <- get_logger("debug_logger")
  console_det <- debug_logger$appenders[[1]]
  expect_identical(console_det$layout$fmt, "[%L] (%t) {%c} %m")

  # Cleanup
  lgr$config(NULL)
  debug_logger$config(NULL)
  get_logger("standard_logger")$config(NULL)
})


test_that("layout configurations are applied correctly from JSON", {
  skip_if_not_installed("jsonlite")

  config_file <- rprojroot::find_testthat_root_file(
    "testdata",
    "config_layouts.json"
  )

  lgr$config(file = config_file)

  # Check root logger layouts
  console_msg <- lgr$appenders[[1]]
  expect_identical(console_msg$layout$fmt, "%m")

  buffer_app <- lgr$appenders[[2]]
  expect_true(inherits(buffer_app$layout, "LayoutJson"))

  # Check app/service logger layout
  app_service <- get_logger("app/service")
  console_comp <- app_service$appenders[[1]]
  expect_identical(console_comp$layout$fmt, "%L [%t] <%c:%n> %m")

  # Cleanup
  lgr$config(NULL)
  app_service$config(NULL)
})


test_that("LayoutFormat with different format strings are parsed correctly", {
  skip_if_not_installed("yaml")

  cfg <- list(
    version = 1,
    layouts = list(
      fmt1 = list(class = "LayoutFormat", fmt = "%m"),
      fmt2 = list(class = "LayoutFormat", fmt = "%L %m"),
      fmt3 = list(class = "LayoutFormat", fmt = "%L [%t] %m"),
      fmt4 = list(class = "LayoutFormat", fmt = "[%L] (%t) {%c:%n} %m")
    ),
    appenders = list(
      app1 = list(class = "AppenderConsole", threshold = "info", layout = "fmt1"),
      app2 = list(class = "AppenderConsole", threshold = "info", layout = "fmt2"),
      app3 = list(class = "AppenderConsole", threshold = "info", layout = "fmt3"),
      app4 = list(class = "AppenderConsole", threshold = "info", layout = "fmt4")
    ),
    loggers = list(
      root = list(threshold = "all", appenders = c("app1", "app2", "app3", "app4"))
    )
  )

  lgr$config(cfg)

  appenders <- lgr$appenders
  expect_identical(appenders[[1]]$layout$fmt, "%m")
  expect_identical(appenders[[2]]$layout$fmt, "%L %m")
  expect_identical(appenders[[3]]$layout$fmt, "%L [%t] %m")
  expect_identical(appenders[[4]]$layout$fmt, "[%L] (%t) {%c:%n} %m")

  # Cleanup
  lgr$config(NULL)
})


test_that("JSON layout is applied correctly from config", {
  skip_if_not_installed("jsonlite")
  on.exit(lgr$config(NULL))

  config_file <- rprojroot::find_testthat_root_file(
    "testdata",
    "config_layouts.json"
  )

  lgr$config(file = config_file)

  # Get buffer appender with JSON layout
  buffer_app <- lgr$appenders[[2]]
  expect_true(inherits(buffer_app$layout, "LayoutJson"))

  # Log something and verify it's stored correctly
  lgr$info("test message")

  # The buffer should have the event
  events <- buffer_app$buffer_events
  expect_length(events, 1)
})


test_that("layout format strings contain expected format codes", {
  skip_if_not_installed("yaml")

  config_file <- rprojroot::find_testthat_root_file(
    "testdata",
    "config_layouts.yaml"
  )

  lgr$config(file = config_file)

  # Test each format string
  minimal_fmt <- lgr$appenders[[1]]$layout$fmt
  expect_true(grepl("%m", minimal_fmt))
  expect_false(grepl("%L", minimal_fmt))

  detailed_fmt <- get_logger("debug_logger")$appenders[[1]]$layout$fmt
  expect_true(grepl("%L", detailed_fmt))
  expect_true(grepl("%t", detailed_fmt))
  expect_true(grepl("%c", detailed_fmt))

  # Cleanup
  lgr$config(NULL)
  get_logger("debug_logger")$config(NULL)
  get_logger("standard_logger")$config(NULL)
})
