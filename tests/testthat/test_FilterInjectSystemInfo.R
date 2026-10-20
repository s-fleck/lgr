test_that("default fields are present", {
  f <- FilterInjectSystemInfo$new()

  expect_true(nzchar(f$values[["hostName"]]))
  expect_true(nzchar(f$values[["rVersion"]]))
})


test_that("containerId is present only when running in a Docker container", {
  local_mocked_bindings(is_in_docker_container = function() TRUE, .package = "lgr")
  f <- FilterInjectSystemInfo$new()
  expect_true("containerId" %in% names(f$values))
  expect_true(nzchar(f$values[["containerId"]]))

  local_mocked_bindings(is_in_docker_container = function() FALSE, .package = "lgr")
  f2 <- FilterInjectSystemInfo$new()
  expect_false("containerId" %in% names(f2$values))
})


test_that("scriptFile is omitted (not erroring) when it cannot be determined", {
  local_mocked_bindings(get_current_scriptfile = function() NULL, .package = "lgr")
  f <- FilterInjectSystemInfo$new()
  expect_false("scriptFile" %in% names(f$values))
})


test_that("an explicit value overrides a computed default", {
  f <- FilterInjectSystemInfo$new(hostName = "custom-host")
  expect_identical(f$values[["hostName"]], "custom-host")
  # other computed defaults are still present
  expect_true(nzchar(f$values[["rVersion"]]))
})


test_that("extra custom values are injected alongside the computed bundle", {
  f <- FilterInjectSystemInfo$new(environment = "prod", "service.name" = "myApp")
  expect_identical(f$values[["environment"]], "prod")
  expect_identical(f$values[["service.name"]], "myApp")
  expect_true(nzchar(f$values[["hostName"]]))
})


test_that("scriptFile detection degrades gracefully when rstudioapi reports unavailable", {
  skip_if_not_installed("rstudioapi")
  local_mocked_bindings(isAvailable = function(...) FALSE, .package = "rstudioapi")
  expect_null(get_current_scriptfile_rstudio())
  # the whole bundle still constructs fine, falling through to other detection methods
  expect_no_error(FilterInjectSystemInfo$new())
})


test_that("FilterInjectSystemInfo actually injects values into log events", {
  lg <- get_logger("test/system_info")
  lg$set_propagate(FALSE)
  lg$add_appender(AppenderBuffer$new(), "buf")
  lg$add_filter(FilterInjectSystemInfo$new(environment = "test"), "systemInfo")

  lg$info("hello")
  event <- lg$appenders$buf$buffer_events[[1]]

  expect_identical(event$values[["environment"]], "test")
  expect_true(nzchar(event$values[["hostName"]]))

  lg$config(NULL)
})


test_that("FilterInjectSystemInfo is usable from a declarative config with no loader changes", {
  cfg <- list(
    version = 1,
    filters = list(systemInfo = list(class = "FilterInjectSystemInfo", environment = "prod")),
    appenders = list(console = list(class = "AppenderConsole")),
    loggers = list(root = list(threshold = "info", appenders = "console", filters = "systemInfo"))
  )

  lgr$config(cfg)

  expect_length(lgr$filters, 1)
  expect_true(inherits(lgr$filters[[1]], "FilterInjectSystemInfo"))
  expect_identical(lgr$filters[[1]]$values[["environment"]], "prod")
  expect_true(nzchar(lgr$filters[[1]]$values[["hostName"]]))

  lgr$config(NULL)
})
