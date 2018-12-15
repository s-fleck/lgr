context("predicates")


test_that("predicates works as expected", {
  expect_true(is_valid_log_level(300))
  expect_true(is_valid_log_level("info"))
  expect_true(is_valid_log_level(231))
  expect_true(is_valid_log_level(NA))

  expect_false(is_valid_log_level(-1))
  expect_false(is_valid_log_level("potato"))

  expect_error(assert_valid_log_levels(-1, "info"))
  expect_true(assert_valid_log_levels(c("info", 231, 300, NA)))
})
