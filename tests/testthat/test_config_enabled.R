test_that("a disabled named appender is dropped from a referencing logger without error", {
  cfg <- list(
    version = 1,
    appenders = list(
      console = list(class = "AppenderConsole"),
      metrics = list(class = "AppenderConsole", enabled = FALSE)
    ),
    loggers = list(root = list(threshold = "info", appenders = c("console", "metrics")))
  )

  lgr$config(cfg)

  expect_length(lgr$appenders, 1)
  expect_true(inherits(lgr$appenders[[1]], "AppenderConsole"))

  lgr$config(NULL)
})


test_that("a disabled inline filter is dropped from a filters list", {
  cfg <- list(
    version = 1,
    appenders = list(console = list(class = "AppenderConsole")),
    loggers = list(root = list(
      threshold = "info",
      appenders = "console",
      filters = list(
        list(class = "FilterForceLevel", level = "warn"),
        list(class = "FilterForceLevel", level = "error", enabled = FALSE)
      )
    ))
  )

  lgr$config(cfg)

  expect_length(lgr$filters, 1)
  expect_identical(lgr$filters[[1]]$level, 300L)  # warn

  lgr$config(NULL)
})


test_that("a disabled logger definition is fully reset regardless of prior state, and idempotently so", {
  sub <- get_logger("app/sub")
  sub$set_threshold("debug")
  sub$add_appender(AppenderConsole$new(), "x")
  sub$set_propagate(FALSE)

  cfg <- list(version = 1, loggers = list("app/sub" = list(enabled = FALSE)))
  lgr$config(cfg)

  expect_true(is_threshold_inherited(sub))
  expect_length(sub$appenders, 0)
  expect_true(sub$propagate)
  expect_identical(sub$exception_handler, default_exception_handler)

  # mutate again and re-apply: must still end up reset (idempotency)
  sub$set_threshold("trace")
  sub$set_propagate(FALSE)
  lgr$config(cfg)

  expect_true(is_threshold_inherited(sub))
  expect_true(sub$propagate)

  sub$config(NULL)
})


test_that("enabled can be toggled via environment interpolation", {
  withr::local_envvar(c(METRICS_ENABLED = NA))

  cfg <- list(
    version = 1,
    appenders = list(metrics = list(class = "AppenderConsole", enabled = "${METRICS_ENABLED:-false}")),
    loggers = list(root = list(threshold = "info", appenders = "metrics"))
  )

  lgr$config(cfg)
  expect_length(lgr$appenders, 0)
  lgr$config(NULL)

  withr::local_envvar(c(METRICS_ENABLED = "true"))
  lgr$config(cfg)
  expect_length(lgr$appenders, 1)
  lgr$config(NULL)
})


test_that("a non-boolean enabled value is rejected", {
  cfg <- list(
    version = 1,
    appenders = list(console = list(class = "AppenderConsole", enabled = "sure")),
    loggers = list(root = list(appenders = "console"))
  )

  expect_error(lgr$config(cfg), "boolean")
})


test_that("enabled is not forwarded as a constructor argument", {
  cfg <- list(
    version = 1,
    appenders = list(console = list(class = "AppenderConsole", enabled = TRUE)),
    loggers = list(root = list(threshold = "info", appenders = "console"))
  )

  # would error if `enabled` were passed on to AppenderConsole$new(...)
  expect_silent(lgr$config(cfg))
  expect_length(lgr$appenders, 1)

  lgr$config(NULL)
})
