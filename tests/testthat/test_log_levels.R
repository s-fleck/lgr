context("simple_logging")


test_that("simple_logging works as expected", {
  ml <- Logger$new("dummy")

  expect_error(ml$threshold <- "blubb")
  add_log_levels(c(blubb = 250))
  expect_silent(ml$threshold <- "blubb")
  expect_identical(ml$threshold, 250L)
  remove_log_levels("blubb")  # cleanup
  expect_error(ml$threshold <- "blubb")
})


test_that("labelling/unlabelling log levels is symetirc", {
  tdat <- sample(c(seq(0, 600, by = 100), NA))

  expect_silent({
    x <- label_levels(tdat)
    expect_equal(names(x), as.character(tdat))
    y <- unlabel_levels(x)
    expect_equal(unname(y), tdat)
    expect_equal(names(y), unname(x))
  })

  expect_silent(label_levels(NA_integer_))
  expect_warning(label_levels(700))
  expect_warning(unlabel_levels(NA_character_))
  expect_warning(unlabel_levels("foo"))
})
