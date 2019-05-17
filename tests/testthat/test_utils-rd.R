context("utils-rd")


test_that("utils-rd works as expected", {

  res <- r6_usage(AppenderFileRotating)


  res <- r6_usage(list(
    AppenderFileRotating,
    AppenderFileRotatingDate,
    AppenderFileRotatingTime
  ))

  expect_true(is.character(res))
})
