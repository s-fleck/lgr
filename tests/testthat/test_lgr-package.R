test_that("get_envar_suspend_logging works as expected", {
  on.exit(Sys.setenv(LGR_SUSPEND_LOGGING = NA_character_))

  Sys.setenv("LGR_SUSPEND_LOGGING" = TRUE)
  expect_true(get_envar_suspend_logging())

  Sys.setenv("LGR_SUSPEND_LOGGING" = FALSE)
  expect_false(get_envar_suspend_logging())

  Sys.setenv("LGR_SUSPEND_LOGGING" = "foo")
  expect_warning(get_envar_suspend_logging())
  expect_false(suppressWarnings(get_envar_suspend_logging()))

  Sys.setenv("LGR_SUSPEND_LOGGING" = "")
  expect_false(get_envar_suspend_logging())
})


test_that("get_envar_default_config works as expected", {
  withr::local_envvar(c(LGR_DEFAULT_CONFIG = NA_character_))

  Sys.setenv(
    "LGR_DEFAULT_CONFIG" = system.file(
      "configs/recommended.yaml",
      package = "lgr"
    )
  )
  expect_s3_class(get_envar_default_config(), "logger_config")

  Sys.setenv("LGR_DEFAULT_CONFIG" = FALSE)
  expect_warning(expect_null(get_envar_default_config()))

  Sys.setenv("LGR_DEFAULT_CONFIG" = "")
  expect_null(get_envar_default_config())
})


test_that("get_envar_default_threshold works as expected", {
  withr::local_envvar(c(LGR_DEFAULT_THRESHOLD = NA_character_))

  Sys.setenv("LGR_DEFAULT_THRESHOLD" = "100")
  expect_identical(get_envar_default_threshold(), 100L)

  Sys.setenv("LGR_DEFAULT_THRESHOLD" = "iNfO")
  expect_identical(get_envar_default_threshold(), 400L)

  Sys.setenv("LGR_DEFAULT_THRESHOLD" = "-100")
  expect_warning(expect_equal(get_envar_default_threshold(), 400L)) # default

  Sys.setenv("LGR_DEFAULT_THRESHOLD" = FALSE)
  expect_warning(expect_equal(get_envar_default_threshold(), 400L)) # default

  Sys.setenv("LGR_DEFAULT_THRESHOLD" = "")
  expect_equal(get_envar_default_threshold(), 400L) # default
})
