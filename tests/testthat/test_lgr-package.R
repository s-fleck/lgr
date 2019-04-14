context("lgr-package")


ev_suspend_logging    <- Sys.getenv("LGR_SUSPEND_LOGGING")
ev_default_config     <- Sys.getenv("LGR_DEFAULT_CONFIG")
ev_default_threshold  <- Sys.getenv("LGR_DEFAULT_THRESHOLD")

teardown({
  Sys.setenv(
    LGR_SUSPEND_LOGGING = ev_suspend_logging,
    LGR_DEFAULT_CONFIG  = ev_default_config,
    LGR_DEFAULT_THRESHOLD = ev_default_threshold
  )
})




test_that("get_envar_suspend_logging works as expected", {

  Sys.setenv("LGR_SUSPEND_LOGGING" = TRUE)
  expect_true(get_envar_suspend_logging())

  Sys.setenv("LGR_SUSPEND_LOGGING" = FALSE)
  expect_false(get_envar_suspend_logging())

  Sys.setenv("LGR_SUSPEND_LOGGING" = "foo")
  expect_false(expect_warning(get_envar_suspend_logging()))

  Sys.setenv("LGR_SUSPEND_LOGGING" = "")
  expect_false(get_envar_suspend_logging())
})




test_that("get_envar_default_config works as expected", {

  Sys.setenv("LGR_DEFAULT_CONFIG" = system.file("configs/recommended.yaml", package = "lgr"))
  expect_s3_class(get_envar_default_config(), "logger_config")

  Sys.setenv("LGR_DEFAULT_CONFIG" = FALSE)
  expect_warning(expect_null(get_envar_default_config()))

  Sys.setenv("LGR_DEFAULT_CONFIG" = "")
  expect_null(get_envar_default_config())
})




test_that("get_envar_default_threshold works as expected", {

  Sys.setenv("LGR_DEFAULT_THRESHOLD" = "100")
  expect_identical(get_envar_default_threshold(), 100L)

  Sys.setenv("LGR_DEFAULT_THRESHOLD" = "iNfO")
  expect_identical(get_envar_default_threshold(), 400L)

  Sys.setenv("LGR_DEFAULT_THRESHOLD" = "-100")
  expect_warning(expect_equal(get_envar_default_threshold(), 400L))  # default

  Sys.setenv("LGR_DEFAULT_THRESHOLD" = FALSE)
  expect_warning(expect_equal(get_envar_default_threshold(), 400L))  # default

  Sys.setenv("LGR_DEFAULT_THRESHOLD" = "")
  expect_equal(get_envar_default_threshold(), 400L)  # default
})
