# reader --------------------------------------------------------------------

test_that("read_config - yaml file path - parses and validates", {
  cfg <- read_config(rprojroot::find_testthat_root_file("testdata", "config_appenders.yaml"))
  expect_s3_class(cfg, "lgr_config")
  expect_true("root" %in% names(cfg$loggers))
})


test_that("read_config - json file path - parses and validates", {
  skip_if_not_installed("jsonlite")
  cfg <- read_config(rprojroot::find_testthat_root_file("testdata", "config_comprehensive.json"))
  expect_s3_class(cfg, "lgr_config")
})


test_that("read_config - yaml string - parses and validates", {
  cfg <- read_config("loggers:\n  root:\n    threshold: info\n")
  expect_identical(cfg$loggers$root$threshold, "info")
})


test_that("read_config - json string - parses and validates", {
  skip_if_not_installed("jsonlite")
  cfg <- read_config('{"loggers": {"root": {"threshold": "info"}}}')
  expect_identical(cfg$loggers$root$threshold, "info")
})


test_that("read_config - list without a signal key - errors", {
  expect_error(
    read_config(list(appenders = list())),
    "loggers"
  )
})


test_that("read_config - not a mapping/list/string - errors", {
  expect_error(read_config(42), "file path")
})


# validation ------------------------------------------------------------

test_that("validate_config - unknown top-level key - errors with the key name", {
  expect_error(
    validate_config(list(loggers = list(), appdenders = list())),
    "appdenders"
  )
})


test_that("validate_config - did-you-mean suggestion for a typo'd appender ref", {
  expect_error(
    validate_config(list(
      appenders = list(console = list(class = "AppenderConsole")),
      loggers = list(root = list(appenders = "consle"))
    )),
    "console"
  )
})


test_that("validate_config - dangling layout reference - errors naming the layout", {
  expect_error(
    validate_config(list(
      appenders = list(a = list(class = "AppenderConsole", layout = "missing_layout")),
      loggers = list(root = list())
    )),
    "missing_layout"
  )
})


test_that("validate_config - cyclic appender reference - errors naming the cycle", {
  expect_error(
    validate_config(list(
      appenders = list(
        a = list(class = "AppenderBuffer", appenders = "b"),
        b = list(class = "AppenderBuffer", appenders = "a")
      ),
      loggers = list(root = list())
    )),
    "Cyclic"
  )
})


test_that("validate_config - multiple errors - all are reported", {
  err <- tryCatch(
    validate_config(list(
      version = 2,
      disable_existing_loggers = "maybe",
      loggers = list()
    )),
    error = function(e) e
  )
  expect_s3_class(err, "ConfigValidationError")
  expect_match(err$message, "version")
  expect_match(err$message, "disable_existing_loggers")
})


test_that("validate_config - root logger cannot be a LoggerGlue", {
  expect_error(
    validate_config(list(loggers = list(root = list(class = "LoggerGlue")))),
    "LoggerGlue"
  )
})


test_that("validate_config - valid config - returns TRUE invisibly", {
  expect_true(validate_config(list(loggers = list(root = list(threshold = "info")))))
})


# safety ------------------------------------------------------------------

test_that("validate_config - inline code filter rejected by default", {
  expect_error(
    validate_config(list(
      loggers = list(root = list(filters = list(code = "function(event) TRUE")))
    )),
    "allow_code"
  )
})


test_that("validate_config - inline code filter allowed with allow_code = TRUE", {
  expect_true(validate_config(
    list(loggers = list(root = list(filters = list(code = "function(event) TRUE")))),
    allow_code = TRUE
  ))
})


# atomicity -----------------------------------------------------------------

test_that("Logger$config - invalid declarative config leaves the hierarchy unchanged", {
  lgr$config(list(loggers = list(root = list(threshold = "warn"))))
  before_threshold <- lgr$threshold
  before_appenders <- lgr$appenders

  expect_error(
    lgr$config(list(
      appenders = list(a = list(class = "AppenderConsole")),
      loggers = list(root = list(threshold = "debug", appenders = "unknown_ref"))
    ))
  )

  expect_identical(lgr$threshold, before_threshold)
  expect_identical(lgr$appenders, before_appenders)

  lgr$config(NULL)
})


test_that("Logger$config - NULL resets a Logger to its default state", {
  lg <- get_logger("config_schema_reset_test")
  lg$config(list(threshold = "error", propagate = FALSE))
  expect_identical(lg$threshold, 200L)

  lg$config(NULL)
  expect_true(is_virgin_Logger(lg))
})


# JSON Schema -----------------------------------------------------------

test_that("config_schema_file - is installed and is valid JSON", {
  skip_if_not_installed("jsonlite")
  f <- config_schema_file()
  expect_true(file.exists(f))
  schema <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  expect_identical(schema[["$schema"]], "http://json-schema.org/draft-07/schema#")
})


test_that("bundled example configs validate against our own config validator", {
  skip_if_not_installed("yaml")

  config_files <- list.files(
    system.file("configs", package = "lgr"),
    pattern = "\\.(yaml|yml|json)$",
    full.names = TRUE
  )
  expect_true(length(config_files) > 0)

  for (f in config_files) {
    expect_true(validate_config(f), info = f)
  }
})
