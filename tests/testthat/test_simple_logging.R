local_simple_logging_fixture <- function(env = parent.frame()) {

  basic_config(console = "info")
  lgr$add_appender(AppenderBuffer$new(threshold = NA), "memory")

  withr::defer(basic_config(), envir = env)
  invisible()
}


test_that("threshold(), console_threshold() and log_exception() work as expected", {
  local_simple_logging_fixture()

  expect_identical(threshold(), lgr$threshold)
  expect_identical(console_threshold(), lgr$appenders$console$threshold)

  yth <- lgr$threshold
  cth <- lgr$appenders$console$threshold

  on.exit({
    lgr$set_threshold(yth)
    lgr$appenders$console$set_threshold(cth)
  })

  threshold(NA)
  console_threshold(NA)

  expect_output(lgr$fatal("test"), "FATAL")
  expect_output(lgr$trace("test"), "TRACE")

  expect_error(
    expect_output(log_exception(stop("oops")), "FATAL.*oops$"),
    "oops"
  )
})


test_that("show_log()", {
  local_simple_logging_fixture()

  expect_output(lgr$info("test1"))
  expect_output(lgr$info("test2"))
  expect_output(lgr$info("test3"))

  expect_output(show_log())
  expect_true(nrow(show_data()) > 2)
  expect_output(
    expect_equal(show_log(), show_log(target = lgr$appenders$memory))
  )
  expect_error(show_log(target = lgr$appenders$console), "no method")

  lg <- Logger$new("test", propagate = FALSE)
  expect_error(show_log(target = lg), "has no Appender")
  expect_error(show_log(target = iris), "not a Logger or Appender")
})


test_that("add_appender() and remove_appender() work", {
  local_simple_logging_fixture()

  tlg <- Logger$new("test", propagate = FALSE)

  add_appender(AppenderConsole$new(), target = tlg)
  expect_output(tlg$warn("test"), "WARN.*test\\s*$")
  remove_appender(1, target = tlg)
  expect_silent(tlg$warn("test"))
  expect_error(show_log(tlg))
})


test_that("Option 'lgr.log_file' works", {
  local_simple_logging_fixture()

  old <- getOption("lgr.log_file")
  on.exit(options("lgr.log_file" = old))

  options("lgr.log_file" = tempfile())

  res <- default_appenders()
  expect_identical(length(res), 1L)
  expect_s3_class(res[[1]], "AppenderFile")
  expect_s3_class(res[[1]]$layout, "LayoutFormat")
  expect_true(is.na(res[[1]]$threshold))
  unlink(getOption("lgr.log_file"))

  options("lgr.log_file" = tempfile(pattern = ".json"))
  res <- default_appenders()
  expect_identical(length(res), 1L)
  expect_s3_class(res[[1]], "AppenderFile")
  expect_s3_class(res[[1]]$layout, "LayoutJson")
  expect_true(is.na(res[[1]]$threshold))
  unlink(getOption("lgr.log_file"))

  options("lgr.log_file" = c(trace = tempfile(pattern = ".json")))
  res <- default_appenders()
  expect_identical(length(res), 1L)
  expect_s3_class(res[[1]], "AppenderFile")
  expect_s3_class(res[[1]]$layout, "LayoutJson")
  expect_identical(res[[1]]$threshold, 600L)
  unlink(getOption("lgr.log_file"))

  options("lgr.log_file" = c(blubb = tempfile('_fail')))
  expect_warning(res <- default_appenders(), "_fail")
  unlink(getOption("lgr.log_file"))
})


test_that("show_data() works", {
  local_simple_logging_fixture()

  basic_config(memory = TRUE)
  on.exit(basic_config())

  expect_output({
    lgr$info("a log message")
    lgr$info("another message with data", data = 1:10)
    lgr$info(c("a vectorized message", "blubb"))
  })

  expect_output(show_log())
  expect_s3_class(show_data(), "data.frame")
  expect_identical(nrow(show_data()), 4L)
  expect_s3_class(show_dt(), "data.table")
  expect_identical(nrow(show_dt()), 4L)
})


test_that("basic_config can set the console output format", {
  local_simple_logging_fixture()

  basic_config(console_fmt = "foobar %L [%t] %c: %m")
  expect_output(lgr$info("baz"), "foobar")
})

