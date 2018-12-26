context("simple_logging")



test_that("simple_logging works as expected", {
  expect_identical(threshold(), yog::yog$threshold)
  expect_identical(console_threshold(), yog::yog$appenders$console$threshold)

  yth <- yog::yog$threshold
  cth <- yog::yog$appenders$console$threshold

  threshold(NA)
  console_threshold(NA)

  expect_output(FATAL("test"), "FATAL")
  expect_output(ERROR("test"), "ERROR")
  expect_output(WARN("test"), "WARN")
  expect_output(INFO("test"), "INFO")
  expect_output(DEBUG("test"), "DEBUG")
  expect_output(TRACE("test"), "TRACE")

  expect_error(
    expect_output(log_exception(stop("oops")), "FATAL.*oops$"),
    "oops"
  )

  yog$set_threshold(yth)
  yog$appenders$console$set_threshold(cth)
})




test_that("show_log()", {
  expect_output(expect_true(is.data.frame(show_log())))
  expect_output(expect_true(nrow(show_log()) > 5))
  expect_output(
    expect_identical(show_log(), show_log(target = yog$appenders$memory))
  )
  expect_error(show_log(target = yog$appenders$console), "does not have")

  lg <- Logger$new("test", propagate = FALSE)
  expect_error(show_log(target = lg), "has no Appender")
  expect_error(show_log(target = iris), "not a valid")
})



test_that("add/remove_appender", {
  tlg <- Logger$new("test", propagate = FALSE)

  add_appender(AppenderConsole$new(), target = tlg)
  expect_output(tlg$warn("test"), "WARN.*test$")
  remove_appender(1, target = tlg)
  expect_silent(tlg$warn("test"))
  expect_error(show_log(tlg))
})



test_that("option appenders setup", {
  options("yog.log_file" = tempfile())

  res <- default_appenders()
  expect_identical(length(res), 1L)
  expect_s3_class(res[[1]], "AppenderFile")
  expect_s3_class(res[[1]]$layout, "LayoutFormat")
  expect_true(is.na(res[[1]]$threshold))

  options("yog.log_file" = tempfile(pattern = ".json"))
  res <- default_appenders()
  expect_identical(length(res), 1L)
  expect_s3_class(res[[1]], "AppenderFile")
  expect_s3_class(res[[1]]$layout, "LayoutJson")
  expect_true(is.na(res[[1]]$threshold))

  options("yog.log_file" = c(trace = tempfile(pattern = ".json")))
  res <- default_appenders()
  expect_identical(length(res), 1L)
  expect_s3_class(res[[1]], "AppenderFile")
  expect_s3_class(res[[1]]$layout, "LayoutJson")
  expect_identical(res[[1]]$threshold, 600L)

  options("yog.log_file" = c(blubb = tempfile('_fail')))
  expect_warning(res <- default_appenders(), "_fail")
})


