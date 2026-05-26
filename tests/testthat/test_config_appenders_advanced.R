test_that("multiple appenders on single logger work correctly", {
  cfg <- list(
    version = 1,
    appenders = list(
      console = list(class = "AppenderConsole", threshold = "info"),
      buffer = list(class = "AppenderBuffer", threshold = "all", buffer_size = 100)
    ),
    loggers = list(
      root = list(threshold = "all", appenders = c("console", "buffer"))
    )
  )

  lgr$config(cfg)

  # Root logger has two appenders
  expect_length(lgr$appenders, 2)

  appenders <- lgr$appenders
  expect_true(inherits(appenders[[1]], "AppenderConsole"))
  expect_true(inherits(appenders[[2]], "AppenderBuffer"))

  # Cleanup
  lgr$config(NULL)
})


test_that("appender thresholds control what gets logged", {
  skip_if_not_installed("yaml")

  tf_info <- tempfile(fileext = ".log")
  tf_warn <- tempfile(fileext = ".log")

  cfg <- list(
    version = 1,
    disable_existing_loggers = TRUE,
    layouts = list(
      simple = list(class = "LayoutFormat", fmt = "%L %m")
    ),
    appenders = list(
      file_info = list(
        class = "AppenderFile",
        file = tf_info,
        threshold = "info",
        layout = "simple"
      ),
      file_warn = list(
        class = "AppenderFile",
        file = tf_warn,
        threshold = "warn",
        layout = "simple"
      )
    ),
    loggers = list(
      root = list(
        threshold = "debug",
        appenders = c("file_info", "file_warn")
      )
    )
  )

  lgr$config(cfg)

  # Log at different levels
  lgr$debug("debug message")
  lgr$info("info message")
  lgr$warn("warn message")

  # Check file_info (threshold = info)
  lines_info <- readLines(tf_info)
  expect_true(any(grepl("info message", lines_info)))
  expect_true(any(grepl("warn message", lines_info)))
  expect_false(any(grepl("debug message", lines_info)))

  # Check file_warn (threshold = warn)
  lines_warn <- readLines(tf_warn)
  expect_true(any(grepl("warn message", lines_warn)))
  expect_false(any(grepl("info message", lines_warn)))
  expect_false(any(grepl("debug message", lines_warn)))

  # Cleanup
  unlink(c(tf_info, tf_warn))
  lgr$config(NULL)
})


test_that("appender names are accessible", {
  skip_if_not_installed("yaml")

  config_file <- rprojroot::find_testthat_root_file(
    "testdata",
    "config_comprehensive.yaml"
  )

  lgr$config(file = config_file)

  # Check appender names in root logger
  appender_names <- names(lgr$appenders)
  expect_true("console_info" %in% appender_names)
  expect_true("buffer" %in% appender_names)

  # Check app logger appender names
  app <- get_logger("app")
  app_appender_names <- names(app$appenders)
  expect_true("console_debug" %in% app_appender_names)
  expect_true("buffer" %in% app_appender_names)

  # Cleanup
  lgr$config(NULL)
  app$config(NULL)
  get_logger("app/db")$config(NULL)
  get_logger("app/api")$config(NULL)
})


test_that("console appender properties are set from config", {
  skip_if_not_installed("yaml")

  cfg <- list(
    version = 1,
    layouts = list(
      custom = list(class = "LayoutFormat", fmt = ">>>%m<<<")
    ),
    appenders = list(
      console_custom = list(
        class = "AppenderConsole",
        threshold = "warn",
        layout = "custom"
      )
    ),
    loggers = list(
      root = list(
        threshold = "all",
        appenders = "console_custom"
      )
    )
  )

  lgr$config(cfg)

  console_app <- lgr$appenders[[1]]
  expect_true(inherits(console_app, "AppenderConsole"))
  expect_identical(console_app$threshold, 300L)  # warn
  expect_identical(console_app$layout$fmt, ">>>%m<<<")

  # Cleanup
  lgr$config(NULL)
})


test_that("file appender properties are set from config", {
  skip_if_not_installed("yaml")

  tf <- tempfile(fileext = ".log")

  cfg <- list(
    version = 1,
    layouts = list(
      simple = list(class = "LayoutFormat", fmt = "%L %m")
    ),
    appenders = list(
      file_custom = list(
        class = "AppenderFile",
        file = tf,
        threshold = "debug",
        layout = "simple"
      )
    ),
    loggers = list(
      root = list(
        threshold = "all",
        appenders = "file_custom"
      )
    )
  )

  lgr$config(cfg)

  file_app <- lgr$appenders[[1]]
  expect_true(inherits(file_app, "AppenderFile"))
  expect_identical(file_app$file, tf)
  expect_identical(file_app$threshold, 500L)  # debug

  # Cleanup
  unlink(tf)
  lgr$config(NULL)
})
