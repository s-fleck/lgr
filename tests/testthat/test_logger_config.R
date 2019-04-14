context("logger_config")


test_that("logger_config works as expected", {
  cfg <- logger_config(
    appenders = list("Appender" = list()),
    propagate = FALSE,
    exception_handler = default_exception_handler,
    threshold = NA,
    filters = list("FilterForceLevel" = list(level = "info"))
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




test_that("as_logger_config works as expected with YAML and JSON files", {
  cy <- as_logger_config(rprojroot::find_testthat_root_file("testdata", "lg_full.yaml"))
  cj <- as_logger_config(rprojroot::find_testthat_root_file("testdata", "lg_full.json"))
  expect_identical(cj, cy)
  expect_s3_class(cj, "logger_config")

  expect_identical(cy$appenders[[1]]$layout$LayoutFormat$fmt, "%L %t - %m")

  try(unlink(cy$appenders[[1]]$file), silent = TRUE)
  try(unlink(cy$appenders[[2]]$file), silent = TRUE)
})




test_that("as_logger_config works for simplified yaml logger config", {
  cy <- as_logger_config(rprojroot::find_testthat_root_file("testdata", "lg_simple.yaml"))
  cj <- as_logger_config(rprojroot::find_testthat_root_file("testdata", "lg_simple.json"))
  expect_identical(cj, cy)
  expect_s3_class(cj, "logger_config")

  expect_identical(cy$appenders[[1]]$layout$LayoutFormat$fmt, "%L %t - %m")
  expect_s3_class(cy, "logger_config")
  try(unlink(cy$appenders[[1]]$file), silent = TRUE)
})




test_that("setting logger$config fails if yaml file is passed to `text` instead of `file`", {
  ty <- rprojroot::find_testthat_root_file("testdata", "lg_full.yaml")
  lg <- get_logger("test")
  expect_error(lg$config(text = ty), "YAML")
  lg$config(NULL)
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
  res$config(NULL)
  try(unlink(tf), silent = TRUE)



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
  res$config(NULL)
  try(unlink(tf), silent = TRUE)
})



test_that("parse_logger_configs works", {
  # parse_logger_config turns logger_configs into lists of R6 objects
  # that cann direclty be applied to a logger
  full <- as_logger_config(rprojroot::find_testthat_root_file("testdata", "lg_full.yaml"))
  simple <- as_logger_config(rprojroot::find_testthat_root_file("testdata", "lg_simple.json"))

  pf <- parse_logger_config(full)
  ps <- parse_logger_config(simple)

  lg <- get_logger("test")$config(full)



})

