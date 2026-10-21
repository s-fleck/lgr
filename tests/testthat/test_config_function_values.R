test_that("ref: resolves a function value on an arbitrary layout argument", {
  # use a real exported lgr function purely as a resolvable single-argument
  # function -- `ref:` is meant to look up functions from loaded packages
  # ("pkg::name"), not ad-hoc test-local closures
  cfg <- list(
    version = 1,
    layouts = list(
      custom = list(class = "LayoutJson", transform_event = list(ref = "lgr::default_exception_handler"))
    ),
    appenders = list(
      console = list(class = "AppenderConsole", layout = "custom")
    ),
    loggers = list(root = list(threshold = "info", appenders = "console"))
  )

  lgr$config(cfg)

  expect_identical(lgr$appenders[[1]]$layout$transform_event, default_exception_handler)

  lgr$config(NULL)
})


test_that("code: resolves a function value on an arbitrary appender argument with allow_code", {
  cfg <- list(
    version = 1,
    appenders = list(
      console = list(
        class = "AppenderConsole",
        threshold = list(code = "function() \"info\"")
      )
    ),
    loggers = list(root = list(threshold = "info", appenders = "console"))
  )

  expect_error(lgr$config(cfg), "allow_code")

  # AppenderConsole$threshold isn't itself function-valued, so use a
  # constructor arg that genuinely accepts a function: LayoutJson$transform_event
  cfg2 <- list(
    version = 1,
    layouts = list(
      dyn = list(
        class = "LayoutJson",
        transform_event = list(code = "function(event) event$values")
      )
    ),
    appenders = list(console = list(class = "AppenderConsole", layout = "dyn")),
    loggers = list(root = list(threshold = "info", appenders = "console"))
  )

  lgr$config(cfg2, allow_code = TRUE)
  expect_true(is.function(lgr$appenders[[1]]$layout$transform_event))

  lgr$config(NULL)
})


test_that("code: in a non-filter argument is rejected without allow_code", {
  cfg <- list(
    version = 1,
    layouts = list(
      dyn = list(
        class = "LayoutJson",
        transform_event = list(code = "function(event) event$values")
      )
    ),
    appenders = list(console = list(class = "AppenderConsole", layout = "dyn")),
    loggers = list(root = list(appenders = "console"))
  )

  # atomicity: an invalid config must not modify the hierarchy at all.
  # `is_virgin_Logger()` isn't usable for `root` here -- it is always a
  # `LoggerRoot` subclass and always has an explicit (non-NULL) threshold
  # even at "reset", by LoggerRoot's own design -- so check directly instead.
  expect_length(lgr$appenders, 0)
  expect_error(lgr$config(cfg), "allow_code")
  expect_length(lgr$appenders, 0)

  lgr$config(NULL)
})


test_that("non-scalar ref is rejected", {
  cfg <- list(
    version = 1,
    layouts = list(
      dyn = list(class = "LayoutJson", transform_event = list(ref = c("a", "b")))
    ),
    appenders = list(console = list(class = "AppenderConsole", layout = "dyn")),
    loggers = list(root = list(appenders = "console"))
  )

  expect_error(lgr$config(cfg), "ref")
})


test_that("a bare string argument stays a literal, even if it matches a loaded function's name", {
  # `mean` is a loaded base R function; a `fmt` argument that happens to be
  # the literal string "mean" must not be silently resolved to the function
  cfg <- list(
    version = 1,
    layouts = list(fmt = list(class = "LayoutFormat", fmt = "mean")),
    appenders = list(console = list(class = "AppenderConsole", layout = "fmt")),
    loggers = list(root = list(threshold = "info", appenders = "console"))
  )

  lgr$config(cfg)

  expect_identical(lgr$appenders[[1]]$layout$fmt, "mean")

  lgr$config(NULL)
})
