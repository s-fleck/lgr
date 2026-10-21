# Acceptance test: prove the native `lgr` config format can reproduce the
# console/file/Dynatrace portion of a real production `logger:` config.
# ElasticSearch is intentionally excluded (see design.md for this change).

test_that("the production-equivalent native config reproduces the production logger setup", {
  skip_if_not_installed("yaml")
  skip_if_not_installed("lgrExtra")

  logfile <- withr::local_tempfile()
  withr::local_envvar(c(
    APP_LOG_FILE = logfile,
    # deliberately not a purely numeric-looking value: `${VAR}` interpolation
    # leniently coerces number-like strings to `numeric` (documented
    # behavior, see ?config) -- a real release id would rarely be a bare
    # integer either, so this also better reflects production usage
    RELEASE_ID = "Release-42",
    DYNATRACE_API_KEY = "test-key"
  ))

  config_file <- rprojroot::find_testthat_root_file(
    "testdata",
    "config_production_equivalent.yml"
  )

  lgr$config(config_file)

  # -- appenders: console, file, dynatrace, in that order ------------------
  expect_length(lgr$appenders, 3)
  expect_true(inherits(lgr$appenders[["console"]], "AppenderConsole"))
  expect_true(inherits(lgr$appenders[["file"]], "AppenderJson"))
  expect_true(inherits(lgr$appenders[["dynatrace"]], "AppenderDynatrace"))

  expect_identical(lgr$appenders[["console"]]$threshold, 500L)  # debug
  expect_identical(lgr$appenders[["file"]]$threshold, 500L)     # debug
  expect_identical(lgr$appenders[["dynatrace"]]$threshold, 600L)  # trace

  # -- Dynatrace appender: url/api_key are plain interpolated scalars -------
  expect_identical(lgr$appenders[["dynatrace"]]$url, "https://example-tenant.live.dynatrace.com/api/v2/logs/ingest")
  expect_identical(lgr$appenders[["dynatrace"]]$api_key, "test-key")

  # -- Dynatrace layout: custom LayoutJson with a ref:-resolved transform_event ----
  dynatrace_layout <- lgr$appenders[["dynatrace"]]$layout
  expect_true(inherits(dynatrace_layout, "LayoutJson"))
  expect_identical(
    dynatrace_layout$transform_event,
    get("transform_event_dynatrace", envir = asNamespace("lgrExtra"))
  )
  expect_identical(dynatrace_layout$transform_event_names[["hostname"]], "hostName")
  expect_identical(dynatrace_layout$transform_event_names[["release.id"]], "releaseId")

  # -- system-metadata injection filter --------------------------------
  expect_length(lgr$filters, 1)
  filter <- lgr$filters[[1]]
  expect_true(inherits(filter, "FilterInjectSystemInfo"))
  expect_identical(filter$values[["releaseId"]], "Release-42")
  expect_identical(filter$values[["environment"]], "DEV")
  expect_identical(filter$values[["service.name"]], "myService")
  expect_true(nzchar(filter$values[["hostName"]]))

  # -- end-to-end: a logged event carries the injected metadata and reaches
  #    the file appender in JSON form --------------------------------------
  # Silence the (already fully verified) Dynatrace appender for this one log
  # call: it would otherwise buffer the event and attempt a real HTTP flush
  # against the fake test endpoint/credentials at finalize() time, which is
  # exactly the live-network-I/O this acceptance test is designed to avoid
  # (see design.md: "appender construction ... is what's under test").
  lgr$appenders[["dynatrace"]]$set_threshold("off")

  lgr$info("acceptance test message")

  logged <- jsonlite::stream_in(file(logfile), verbose = FALSE)
  expect_identical(logged$msg[[1]], "acceptance test message")
  expect_identical(logged$service.name[[1]], "myService")
  expect_identical(logged$releaseId[[1]], "Release-42")

  lgr$config(NULL)
})
