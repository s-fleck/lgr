context("get_logger")


test_that("get_logger works as expected", {

  lg <- get_logger("blubb")

  expect_identical(lg$name, "blubb")
  expect_identical(lg$name, "blubb")
  expect_identical(lg$parent, lgr)

  lg1 <- get_logger("fizz")
  lg2 <- get_logger("fizz/buzz")
  lg3 <- get_logger("fizz/buzz/wuzz")

  expect_identical(lg3$parent, lg2)
  expect_identical(lg2$parent, lg1)
  expect_identical(lg1$parent, lgr::lgr)

  lg2$set_propagate(FALSE)

  expect_equal(
    unname(unclass(lg3$ancestry)),
    c(TRUE, FALSE, TRUE)
  )

  expect_equal(
    names(lg3$ancestry),
    c("fizz", "buzz", "wuzz")
  )
})




test_that("logger names can be specified using scalars or vectors", {
  lg1 <- get_logger("fizz/buzz/wuzz")
  lg2 <- get_logger(c("fizz", "buzz", "wuzz"))
  expect_identical(lg1, lg2)
})




test_that("get_logger is not confused by existing objects with the same name as logger", {
  lg1 <- get_logger("log/ger/test")
  lg2 <- get_logger("log/ger")
  expect_identical(lg1$parent, lg2)
})




test_that("is_virgin_Logger identifies loggers without settings", {
  lg1 <- get_logger("foo/bar")
  expect_true(is_virgin_Logger("foo/bar"))
  lg1$set_threshold("off")
  expect_false(is_virgin_Logger("foo/bar"))
  lg1$set_threshold(NULL)
  expect_true(is_virgin_Logger(get_logger("foo/bar")))
})




test_that("Creating LoggerGlue with get_logger_glue works as expected", {
  lg <- get_logger("log/ger/test")

  expect_true(is_Logger(lg))
  expect_s3_class(get_logger_glue("log/ger"), "LoggerGlue")
  expect_s3_class(get_logger("log/ger"), "LoggerGlue")
  expect_identical(lg$parent, get_logger("log/ger"))
})
