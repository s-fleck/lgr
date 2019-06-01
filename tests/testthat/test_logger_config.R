context("logger_config")


test_that("logger_config() works as expected", {
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




test_that("as_logger_config() works as expected with YAML and JSON files", {
  # files work...
  cy <- as_logger_config(rprojroot::find_testthat_root_file("testdata", "lg_full.yaml"))
  cj <- as_logger_config(rprojroot::find_testthat_root_file("testdata", "lg_full.json"))
  expect_identical(cj, cy)
  expect_s3_class(cj, "logger_config")
  expect_identical(cy$appenders[[1]]$layout$LayoutFormat$fmt, "%L %t - %m")

  # so do text vectors (and scalars with newlines)
  cy2 <- as_logger_config(
    readLines(rprojroot::find_testthat_root_file("testdata", "lg_full.yaml"))
  )
  cj2 <- as_logger_config(
    paste(readLines(rprojroot::find_testthat_root_file("testdata", "lg_full.json")), collapse = "\n")
  )
  expect_identical(cj2, cj)
  expect_identical(cy2, cy)
})




test_that("as_logger_config() works for simplified yaml logger config", {
  cy <- as_logger_config(rprojroot::find_testthat_root_file("testdata", "lg_simple.yaml"))
  cj <- as_logger_config(rprojroot::find_testthat_root_file("testdata", "lg_simple.json"))
  expect_identical(cj, cy)
  expect_s3_class(cj, "logger_config")

  expect_identical(cy$appenders[[1]]$layout$LayoutFormat$fmt, "%L %t - %m")
  expect_s3_class(cy, "logger_config")
})




test_that("logger$config() fails if yaml file is passed to `text` instead of `file`", {
  ty <- rprojroot::find_testthat_root_file("testdata", "lg_full.yaml")
  lg <- get_logger("test")
  expect_error(lg$config(text = ty), "YAML")
  lg$config(NULL)
})




test_that("resolve_r6_ctors() works as expected", {
  tf <- tempfile()
  on.exit(unlink(tf))
  x <- logger_config(
    appenders = list("AppenderFile" = list(file = tf))
  )

  res <- resolve_r6_ctors(x)
  expect_s3_class(as_logger_config(res), "logger_config")
  expect_identical(res$appenders[[1]]$file, tf)


  tf2 <- tempfile()
  tf3 <- tempfile()
  on.exit(unlink(c(tf2, tf3)), add = TRUE)
  x <- list(
    "Logger" = list(
      name = "test2",
      appenders = list(
        "AppenderBuffer" = list(
          threshold = NA,
          appenders = list(
            "AppenderJson" = list(threshold = 100, file = tf2),
            "AppenderFile" = list(file = tf3),
            "Appender" = list()
          )
        )
      )
    )
  )

  res <- resolve_r6_ctors(x)[[1]]
  expect_true(is_Logger(res))
  expect_identical(res$appenders[[1]]$appenders[[1]]$file, tf2)
  expect_s3_class(res$appenders[[1]]$appenders[[2]], "AppenderFile")
  expect_identical(res$appenders[[1]]$appenders[[2]]$file, tf3)
  expect_s3_class(res$appenders[[1]]$appenders[[3]], "Appender")

  res$config(NULL)
})



test_that("parse_logger_config() works", {
  # parse_logger_config turns logger_configs into lists of R6 objects
  # that cann direclty be applied to a logger
  full <- as_logger_config(rprojroot::find_testthat_root_file("testdata", "lg_full.yaml"))
  simple <- as_logger_config(rprojroot::find_testthat_root_file("testdata", "lg_simple.json"))

  pf <- parse_logger_config(full)
  ps <- parse_logger_config(simple)

  lg <- get_logger("test")$config(full)
  on.exit(lg$config(NULL))

  expect_length(lg$appenders, 2)
  expect_identical(lg$appenders$AppenderConsole$threshold, 200L)
  expect_identical(lg$appenders$AppenderBuffer$appenders[[1]]$threshold, 123L)
  expect_identical(lg$threshold, 400L)

  lg <- get_logger("test")$config(simple)
  expect_identical(lg$appenders$AppenderConsole$threshold, 200L)
  expect_identical(lg$propagate, FALSE)
  expect_identical(lg$threshold, 400L)
})
