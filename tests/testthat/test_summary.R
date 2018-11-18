context("summary")


test_that("summary works as expected", {

  format_threshold("error")
  format_threshold(300)
  format_threshold(NA)
  format_threshold(0)
  format_threshold(10)

  format_threshold(c(300, 400, NA))


})
