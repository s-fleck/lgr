test_that("complete logging configuration works end-to-end", {

  skip_if_not_installed("yaml")

  tf <- tempfile(fileext = ".log")

  cfg <- list(
    version = 1,
    disable_existing_loggers = TRUE,
    layouts = list(
      simple = list(class = "LayoutFormat", fmt = "%L: %m"),
      json = list(class = "LayoutJson")
    ),
    appenders = list(
      console = list(
        class = "AppenderConsole",
        threshold = "info",
        layout = "simple"
      ),
      file = list(
        class = "AppenderFile",
        file = tf,
        threshold = "all",
        layout = "simple"
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
        appenders = c("console", "file", "buffer")
      ),
      myapp = list(
        threshold = "info",
        appenders = c("file", "buffer"),
        propagate = FALSE
      )
    )
  )

  lgr$config(cfg)

  # act
  debug_output <- capture.output(lgr$debug("debug message"))
  info_output <- capture.output(lgr$info("info message"))
  warn_output <- capture.output(lgr$warn("warning message"))
  error_output <- capture.output(lgr$error("error message"))

  myapp <- get_logger("myapp")
  myapp$info("myapp info")
  myapp$debug("myapp debug")  # Should not appear due to threshold


  # assert

  # Check console output
  expect_length(debug_output, 0)  # below threshold
  expect_match(info_output, "info message")
  expect_match(warn_output, "warning message")
  expect_match(error_output, "error message")


  # Check root logger
  expect_identical(lgr$threshold, 500L)
  expect_length(lgr$appenders, 3)

  # Check file was written
  lines <- readLines(tf)
  expect_length(lines, 5)  # debug, info, warn, error, myapp info (not myapp debug)
  expect_true(any(grepl("info message", lines)))
  expect_true(any(grepl("warning message", lines)))

  # Check buffer
  buffer_app <- lgr$appenders[[3]]
  expect_length(buffer_app$buffer_events, 5)

  # Check myapp logger
  expect_identical(myapp$threshold, 400L)
  expect_false(myapp$propagate)
  expect_length(myapp$appenders, 2)

  # Cleanup
  unlink(tf)
  lgr$config(NULL)
  myapp$config(NULL)
})


test_that("logger hierarchy with disable_existing_loggers works", {
  skip_if_not_installed("yaml")

  # Create a logger before config
  old_logger <- get_logger("old")
  old_logger$set_threshold("fatal")

  cfg <- list(
    version = 1,
    disable_existing_loggers = TRUE,
    appenders = list(
      console = list(class = "AppenderConsole", threshold = "info")
    ),
    loggers = list(
      root = list(threshold = "info", appenders = "console")
    )
  )

  lgr$config(cfg)

  # Old logger should be reset
  expect_true(is_virgin_Logger("old"))

  # Cleanup
  lgr$config(NULL)
})


test_that("logger inheritance works with configuration", {
  skip_if_not_installed("yaml")

  cfg <- list(
    version = 1,
    disable_existing_loggers = TRUE,
    layouts = list(
      simple = list(class = "LayoutFormat", fmt = "%L %m")
    ),
    appenders = list(
      root_console = list(
        class = "AppenderConsole",
        threshold = "info",
        layout = "simple"
      ),
      child_console = list(
        class = "AppenderConsole",
        threshold = "debug",
        layout = "simple"
      )
    ),
    loggers = list(
      root = list(
        threshold = "info",
        appenders = "root_console"
      ),
      app = list(
        threshold = "debug",
        appenders = "child_console",
        propagate = TRUE
      ),
      "app/module" = list(
        threshold = "all",
        propagate = TRUE
      )
    )
  )

  lgr$config(cfg)

  app <- get_logger("app")
  app_module <- get_logger("app/module")

  # Check thresholds
  expect_identical(lgr$threshold, 400L)  # info
  expect_identical(app$threshold, 500L)  # debug
  expect_identical(app_module$threshold, NA_integer_)  # all

  # Check propagation
  expect_true(app$propagate)
  expect_true(app_module$propagate)

  # Check inheritance
  expect_identical(app_module$parent$name, "app")

  # Cleanup
  lgr$config(NULL)
  app$config(NULL)
  app_module$config(NULL)
})


test_that("threshold levels are correctly configured from strings", {
  skip_if_not_installed("yaml")

  cfg <- list(
    version = 1,
    disable_existing_loggers = TRUE,
    appenders = list(
      console = list(class = "AppenderConsole", threshold = "info")
    ),
    loggers = list(
      root = list(threshold = "all", appenders = "console"),
      debug_logger = list(threshold = "debug", appenders = "console"),
      info_logger = list(threshold = "info", appenders = "console"),
      warn_logger = list(threshold = "warn", appenders = "console"),
      error_logger = list(threshold = "error", appenders = "console"),
      fatal_logger = list(threshold = "fatal", appenders = "console"),
      off_logger = list(threshold = "off", appenders = "console")
    )
  )

  lgr$config(cfg)

  expect_identical(lgr$threshold, NA_integer_)  # all
  expect_identical(get_logger("debug_logger")$threshold, 500L)
  expect_identical(get_logger("info_logger")$threshold, 400L)
  expect_identical(get_logger("warn_logger")$threshold, 300L)
  expect_identical(get_logger("error_logger")$threshold, 200L)
  expect_identical(get_logger("fatal_logger")$threshold, 100L)
  expect_identical(get_logger("off_logger")$threshold, 0L)

  # Cleanup
  lgr$config(NULL)
  get_logger("debug_logger")$config(NULL)
  get_logger("info_logger")$config(NULL)
  get_logger("warn_logger")$config(NULL)
  get_logger("error_logger")$config(NULL)
  get_logger("fatal_logger")$config(NULL)
  get_logger("off_logger")$config(NULL)
})


test_that("complex logger structure with multiple appenders and filters", {
  skip_if_not_installed("yaml")

  tf1 <- tempfile(fileext = ".log")
  tf2 <- tempfile(fileext = ".log")

  cfg <- list(
    version = 1,
    disable_existing_loggers = TRUE,
    layouts = list(
      simple = list(class = "LayoutFormat", fmt = "%L %m")
    ),
    filters = list(
      info_filter = list(class = "FilterForceLevel", level = "info"),
      error_filter = list(class = "FilterForceLevel", level = "error")
    ),
    appenders = list(
      console = list(class = "AppenderConsole", threshold = "info", layout = "simple"),
      file1 = list(class = "AppenderFile", file = tf1, threshold = "all", layout = "simple"),
      file2 = list(class = "AppenderFile", file = tf2, threshold = "error", layout = "simple")
    ),
    loggers = list(
      root = list(threshold = "all", appenders = c("console", "file1")),
      app = list(
        threshold = "debug",
        appenders = c("file1", "file2"),
        filters = "error_filter",
        propagate = FALSE
      ),
      "app/db" = list(
        threshold = "info",
        appenders = "file2",
        propagate = FALSE
      )
    )
  )

  lgr$config(cfg)

  # Log at various levels
  lgr$debug("root debug")
  lgr$info("root info")

  app <- get_logger("app")
  app$debug("app debug")
  app$error("app error")

  app_db <- get_logger("app/db")
  app_db$info("app/db info")
  app_db$error("app/db error")

  # Check file1 (threshold = all)
  lines1 <- readLines(tf1)
  expect_true(any(grepl("root debug", lines1)))
  expect_true(any(grepl("app error", lines1)))

  # Check file2 (threshold = error, has error_filter)
  lines2 <- readLines(tf2)
  expect_true(any(grepl("app error", lines2)))
  expect_true(any(grepl("app/db error", lines2)))

  # Cleanup
  unlink(c(tf1, tf2))
  lgr$config(NULL)
  app$config(NULL)
  app_db$config(NULL)
})


test_that("appender references are properly resolved", {
  skip_if_not_installed("yaml")

  cfg <- list(
    version = 1,
    disable_existing_loggers = TRUE,
    appenders = list(
      shared_console = list(class = "AppenderConsole", threshold = "info"),
      shared_buffer = list(class = "AppenderBuffer", threshold = "all", buffer_size = 50)
    ),
    loggers = list(
      root = list(threshold = "all", appenders = c("shared_console", "shared_buffer")),
      app1 = list(threshold = "info", appenders = "shared_console", propagate = FALSE),
      app2 = list(threshold = "debug", appenders = "shared_buffer", propagate = FALSE)
    )
  )

  lgr$config(cfg)

  # Verify appenders are shared (same object references)
  root_console <- lgr$appenders[[1]]
  root_buffer <- lgr$appenders[[2]]

  app1 <- get_logger("app1")
  app1_console <- app1$appenders[[1]]

  app2 <- get_logger("app2")
  app2_buffer <- app2$appenders[[1]]

  expect_identical(root_console, app1_console)
  expect_identical(root_buffer, app2_buffer)

  # Cleanup
  lgr$config(NULL)
  app1$config(NULL)
  app2$config(NULL)
})
