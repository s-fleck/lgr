context("simple_logging")

setup({
  lgr$add_appender(AppenderDt$new(), "memory")
  get_logger("test")$config(NULL)
})

teardown({
  basic_config()
})




test_that("simple_logging works as expected", {
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
  expect_output(expect_true(is.data.frame(show_log())))
  expect_output(expect_true(nrow(show_log()) > 2))
  expect_output(
    expect_identical(show_log(), show_log(target = lgr$appenders$memory))
  )
  expect_error(show_log(target = lgr$appenders$console), "no method")

  lg <- Logger$new("test", propagate = FALSE)
  expect_error(show_log(target = lg), "has no Appender")
  expect_error(show_log(target = iris), "not a Logger or Appender")
})




test_that("add/remove_appender", {
  tlg <- Logger$new("test", propagate = FALSE)

  add_appender(AppenderConsole$new(), target = tlg)
  expect_output(tlg$warn("test"), "WARN.*test\\s*$")
  remove_appender(1, target = tlg)
  expect_silent(tlg$warn("test"))
  expect_error(show_log(tlg))
})




test_that("option appenders setup", {
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
