context("event_list")


test_that("event_list works as expected", {
  x <- LogEvent$new(msg = LETTERS, logger = lgr)
  expect_length(as_event_list(x), 1L)
  expect_length(as_event_list(x, scalarize = TRUE), 26L)

  l <- event_list(
    LogEvent$new(level = 100, msg = 1:5, logger = lgr),
    LogEvent$new(level = 300, msg = c("A", "B"), logger = lgr)
  )

  expect_length(as_event_list(l, scalarize = TRUE), 7L)
  expect_length(as_event_list(list(l, c(l, list(l))), scalarize = TRUE), 21L)
})
