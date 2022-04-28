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




test_that("as_event_list works for data.frames", {

  dd <- data.frame(
    timestamp = as.POSIXct("2022-04-28 00:00:01") + 1:6,
      level = c(100, 200, 300, 400, 500, 600),
      message = paste("test", 1:6)
    )


  res <- as_event_list(dd)

  expect_s3_class(res, "event_list")
  expect_true(is.list(res))
  expect_identical(length(res), nrow(dd))

  for (event in res){
    expect_true(inherits(event, "LogEvent"))
  }
})
