context("utils-rd")


test_that("utils-rd works as expected", {

  r6_usage(list(
    AppenderFileRotating,
    AppenderFileRotatingDate,
    AppenderFileRotatingTime
  ))

})
