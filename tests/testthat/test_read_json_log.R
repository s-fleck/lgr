context("read_json_lines")


test_that("read_json_lines() works as expected", {
  tf <- tempfile()
  lo <- LayoutJson$new()
  lgr <- Logger$new(
    "test",
    appenders = AppenderFile$new(layout = lo, file = tf),
    threshold = NA,
    propagate = FALSE
  )

  lgr$fatal("test")
  lgr$error("test")
  lgr$warn("test")
  lgr$info("test")
  lgr$debug("test")
  lgr$trace("test")

  tres <- read_json_lines(tf)

  expect_identical(names(tres), c("level", "timestamp", "logger", "caller", "msg"))
  expect_true(all(tres$level == seq(100, 600, by = 100)))
  file.remove(tf)
})
