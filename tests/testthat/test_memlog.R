context("memlog")


test_that("active bindings", {
  ml <- Memlog$new()

  expect_identical(
    ml$log_levels,
    setNames(1L:6L, c("fatal", "error", "warn", "info", "debug", "trace"))
  )
  expect_error(ml$log_levels <- ml$log_levels, "cannot be modified")


  expect_silent(ml$threshold <- 5)
  expect_identical(ml$threshold, 5L)
  expect_silent(ml$threshold <- "fatal")
  expect_identical(ml$threshold, 1L)
  expect_error(ml$threshold <- "blubb", "fatal.*trace")


  expect_s3_class(ml$collector, "CollectorDefault")
  expect_error(ml$collector <- ml$collector, "cannot be modified")


  walk(ml$appenders, function(.x) expect_true(inherits(.x, "Appender")))


  expect_silent(ml$user <- "blubb")
  expect_identical(ml$user, "blubb")
  expect_error(ml$user <- 5, "'user'")


  expect_true(is.function(ml$string_formatter))
  expect_true(is_scalar_character(ml$fmt))
  expect_true(is_scalar_character(ml$timestamp_fmt))
})




test_that("basic logging", {
  ml <- Memlog$new()
  ts <- structure(1540486764.41946, class = c("POSIXct", "POSIXt"))

  testfun <- function(){
    ml$fatal("blubb")
  }

  expect_output({
    testfun()
    testfun()
  })

  expect_true(
    data.table::is.data.table(ml$showdt())
  )

  expect_true(nrow(ml$showdt()) == 2L)

  fatalfun <- function() ml$fatal("test ftl")
  expect_output(fatalfun(), "FATAL")
  expect_true(grepl("fatalfun", ml$showdt()[3]$caller))
})




test_that("memory cycling works", {
  ml <- Memlog$new(collector = CollectorDefault$new(
    level = NA_integer_,
    msg = NA_character_,
    .cache_size = 10)
  )

  expect_output(replicate(12, ml$info("blubb info")))
  expect_equal(ml$showdt()$id, 3:12)
})



test_that("suspending loggers works", {

  expect_output(ml_col$fatal("blubb"), "FATAL")
  x <- capture.output(ml_col$fatal("blubb"))
  ml_col$suspend()
  expect_identical(ml_col$fatal("blubb %s", "blah"), NULL)
  ml_col$unsuspend()
  y <- capture.output(ml_col$fatal("blubb"))

  # ignore timestamp for comparison
  x <- gsub("[.*]", x, "[time]")
  y <- gsub("[.*]", x, "[time]")

  expect_identical(x, y)
})
