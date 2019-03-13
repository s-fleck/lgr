context("logger_config")


test_that("logger_config works as expected", {
  cfg <- logger_config(
    appenders = Appender$new(),
    propagate = FALSE,
    exception_handler = default_exception_handler,
    threshold = NA,
    filters = FilterForceLevel$new("info")
  )
  expect_s3_class(cfg, "logger_config")

  tl <- get_logger("test")$config(cfg)
  expect_true(inherits(tl$appenders[[1]], "Appender"))
  expect_false(is_virgin_Logger(tl))

  expect_silent(tl$fatal("test"))  # propagate
  expect_equal(tl$last_event$level, 400)  # force level filter

  tl <- get_logger("test")$config(logger_config())
  expect_true(is_virgin_Logger(tl))
})




test_that("as_logger_config works as expected with YAML file", {
  ty <- rprojroot::find_testthat_root_file("testdata", "lg_full.yaml")
  cfg <- as_logger_config(ty)
  expect_s3_class(cfg, "logger_config")

  expect_identical(cfg$appenders[[1]]$layout$fmt, "%L %t - %m")
  expect_s3_class(cfg, "logger_config")
})




test_that("setting logger$config fails if yaml file is passed to `text` instead of `file`", {
  ty <- rprojroot::find_testthat_root_file("testdata", "lg_full.yaml")
  lg <- get_logger("test")
  expect_error(lg$config(text = ty), "YAML")
})




test_that("as_logger_config works for simplified yaml logger config", {
  ty <- rprojroot::find_testthat_root_file("testdata", "lg_simple.yaml")
  cfg <- as_logger_config(ty)
  expect_s3_class(cfg, "logger_config")

  expect_identical(cfg$appenders[[1]]$layout$fmt, "%L %t - %m")
  expect_s3_class(cfg, "logger_config")
})




test_that("resolve_r6_ctors works as expected", {
  tf <- tempfile()
  x <- list(
    "Logger" = list(
      name = "test",
      appenders = list(
        "AppenderFile" = list(
          file = tf
        )
      )
    )
  )
  res <- resolve_r6_ctors(x)
  expect_true(is_Logger(res))
  expect_s3_class(as_logger_config(res), "logger_config")
  expect_identical(res$appenders[[1]]$file, tf)


  tf <- tempfile()
  x <- list(
    "Logger" = list(
      name = "test2",
      appenders = list(
        "AppenderBuffer" = list(
          threshold = NA,
          appenders = list(
            "AppenderJson" = list(threshold = 100, file = tf),
            "AppenderFile" = list(file = tempfile()),
            "Appender" = list()
          )
        )
      )
    )
  )

  res <- resolve_r6_ctors(x)
  expect_true(is_Logger(res))
  expect_s3_class(as_logger_config(res), "logger_config")
  expect_identical(res$appenders[[1]]$appenders[[1]]$file, tf)
})
