context("logger_config")


test_that("logger_config works as expected", {
  ty <- rprojroot::find_testthat_root_file("testdata", "lg_full.yaml")
  cfg <- as_logger_config(ty)

  expect_identical(cfg$appenders[[1]]$layout$fmt, "%L %t - %m")
  expect_true(is_Logger(cfg))
})




test_that("logger_config works for simplified logger config", {
  ty <- rprojroot::find_testthat_root_file("testdata", "lg_simple.yaml")
  cfg <- as_logger_config(ty)

  expect_identical(cfg$appenders[[1]]$layout$fmt, "%L %t - %m")
  expect_true(is_Logger(cfg))
})





test_that("resolve_r6_ctors", {

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
  expect_identical(res$appenders[[1]]$appenders[[1]]$file, tf)
})
